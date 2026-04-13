"""
伊都国遺跡マップ — Python 単体版（Web フレームワークなし）。
標準ライブラリの http.server で HTML を配信し、地図は Leaflet（CDN）で表示する。
"""
from __future__ import annotations

import argparse
import html
import json
import os
import re
import signal
import sys
import warnings
import webbrowser
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path

try:
    import pandas
except ImportError:
    print(
        "pandas がインストールされていません。プロジェクトフォルダで次を実行してください:\n"
        "  python3 -m venv .venv && source .venv/bin/activate\n"
        "  pip install -r requirements.txt",
        file=sys.stderr,
    )
    raise SystemExit(1) from None

APP_DIR = Path(__file__).resolve().parent
DEFAULT_CSV = "ito_sites_clean.csv"
DEFAULT_PORT = 8765

NAME_CANDIDATES = ("name", "site_name", "遺跡名", "サイト名")
TYPE_CANDIDATES = ("type", "種類", "種別")
PERIOD_CANDIDATES = ("period", "年代", "時代")
DESC_CANDIDATES = ("desc", "description", "説明", "概要", "備考", "詳細", "テキスト")
LNG_CANDIDATES = ("lng", "lon", "longitude", "経度", "LON", "Lng")
LAT_CANDIDATES = ("lat", "latitude", "緯度", "LAT", "Lat")

# ColorBrewer Set1（9色）— matplotlib 不要
_SET1_HEX = [
    "#e41a1c",
    "#377eb8",
    "#4daf4a",
    "#984ea3",
    "#ff7f00",
    "#ffff33",
    "#a65628",
    "#f781bf",
    "#999999",
]


def _hex_to_rgb(h: str) -> tuple[float, float, float]:
    h = h.lstrip("#")
    return tuple(int(h[i : i + 2], 16) / 255.0 for i in (0, 2, 4))


def _rgb_to_hex(r: float, g: float, b: float) -> str:
    return "#{:02x}{:02x}{:02x}".format(
        int(round(max(0, min(1, r)) * 255)),
        int(round(max(0, min(1, g)) * 255)),
        int(round(max(0, min(1, b)) * 255)),
    )


def _set1_palette(n: int) -> list[str]:
    n = max(1, n)
    base_n = min(9, max(3, min(n, 9)))
    base = [_SET1_HEX[i % 9] for i in range(base_n)]
    if n <= len(base):
        return base[:n]
    rgbs = [_hex_to_rgb(c) for c in base]
    out = []
    for j in range(n):
        t = j / max(n - 1, 1) * (base_n - 1)
        k = int(t)
        k = min(k, base_n - 2)
        u = t - k
        r0, g0, b0 = rgbs[k]
        r1, g1, b1 = rgbs[k + 1]
        out.append(
            _rgb_to_hex(
                (1 - u) * r0 + u * r1,
                (1 - u) * g0 + u * g1,
                (1 - u) * b0 + u * b1,
            )
        )
    return out


def _first_col(df: pandas.DataFrame, candidates: tuple[str, ...]) -> str | None:
    for c in candidates:
        if c in df.columns:
            return c
    return None


def strip_bom_names(df: pandas.DataFrame) -> pandas.DataFrame:
    df = df.copy()
    df.columns = [re.sub(r"^\ufeff", "", str(c)) for c in df.columns]
    return df


def read_sites_csv(path: str | Path) -> pandas.DataFrame:
    path = Path(path)
    last_err: Exception | None = None
    for enc in ("utf-8-sig", "utf-8", "cp932"):
        try:
            return pandas.read_csv(path, encoding=enc)
        except UnicodeDecodeError as e:
            last_err = e
    if last_err:
        raise last_err
    return pandas.read_csv(path)


def break_before_labels(x) -> str:
    s = "" if pandas.isna(x) else str(x)
    s = re.sub(r"([^\n\r])(種類\s*[：:])", r"\1\n\2", s)
    s = re.sub(r"([^\n\r])(時代\s*[：:])", r"\1\n\2", s)
    s = re.sub(r"([^\n\r])(年代\s*[：:])", r"\1\n\2", s)
    return s


def desc_as_html(x) -> str:
    s = break_before_labels(x)
    s = html.escape(s)
    return re.sub(r"\r\n|\r|\n", "<br/>", s)


def esc_field(x) -> str:
    if x is None or pandas.isna(x):
        return ""
    s = str(x).strip()
    if not s:
        return ""
    return html.escape(s)


def normalize_period_series(s: pandas.Series) -> pandas.Series:
    """時代列をフィルタ用に正規化（空は「(時代不明)」にまとめる）。"""
    x = s.astype(str).str.strip()
    x = x.replace({"nan": "", "<NA>": "", "None": ""})
    x = x.mask(x == "", "(時代不明)")
    return x


def period_filter_keys_one(val) -> list[str]:
    """1 セルが「古代・古墳・奈良時代」のとき、全文と分割後の各部分をフィルタキーにする。"""
    if val is None or (isinstance(val, float) and pandas.isna(val)):
        s = ""
    else:
        s = str(val).strip()
    if not s or s in ("nan", "<NA>", "None"):
        return ["(時代不明)"]
    if s == "(時代不明)":
        return [s]
    parts = re.split(r"[\u30fb\uff65\u3001]+", s)
    parts = [p.strip() for p in parts if p.strip()]
    out: list[str] = [s]
    for p in parts:
        if p not in out:
            out.append(p)
    return out


def find_default_csv(name: str = DEFAULT_CSV) -> str | None:
    for d in (os.environ.get("SHINY_APP_DIR"), os.environ.get("ITOMUSEUM_APP_DIR")):
        if d and (Path(d) / name).is_file():
            return str((Path(d) / name).resolve())
    p = APP_DIR / name
    if p.is_file():
        return str(p.resolve())
    p2 = Path.cwd() / name
    if p2.is_file():
        return str(p2.resolve())
    return None


def prepare_sites(raw: pandas.DataFrame) -> tuple[pandas.DataFrame, dict[str, str], dict[int, str]]:
    data = strip_bom_names(raw)
    rename_map: dict[str, str] = {}
    if (c := _first_col(data, NAME_CANDIDATES)):
        rename_map[c] = "name"
    if (c := _first_col(data, TYPE_CANDIDATES)):
        rename_map[c] = "type"
    if (c := _first_col(data, PERIOD_CANDIDATES)):
        rename_map[c] = "period"
    if (c := _first_col(data, DESC_CANDIDATES)):
        rename_map[c] = "desc"
    if (c := _first_col(data, LNG_CANDIDATES)):
        rename_map[c] = "lng"
    if (c := _first_col(data, LAT_CANDIDATES)):
        rename_map[c] = "lat"
    data = data.rename(columns=rename_map)

    for col in ("type", "period", "desc"):
        if col not in data.columns:
            data[col] = pandas.NA

    desc_all_empty = data["desc"].isna() | (data["desc"].astype(str).str.strip() == "")
    if bool(desc_all_empty.all()):
        skip = {"name", "lng", "lat", "type", "period", "desc"}
        best_col = None
        best_mean = 0.0
        for cn in data.columns:
            if cn in skip:
                continue
            x = data[cn]
            if not (pandas.api.types.is_string_dtype(x) or pandas.api.types.is_object_dtype(x)):
                continue
            v = x.astype(str)
            lens = v.str.len()
            lens = lens.mask(lens == 0, pandas.NA)
            m = lens.mean()
            if pandas.notna(m) and float(m) > best_mean and float(m) >= 8:
                best_mean = m
                best_col = cn
        if best_col is not None:
            print(f"説明列を自動推定: {best_col}", file=sys.stderr)
            data["desc"] = data[best_col].astype(str)

    need = ("name", "lng", "lat")
    missing = [k for k in need if k not in data.columns]
    if missing:
        raise ValueError(
            f"必須列がありません: {', '.join(missing)}。実際の列名: {', '.join(map(str, data.columns))}"
        )

    data = data.copy()
    data["lng"] = pandas.to_numeric(data["lng"], errors="coerce")
    data["lat"] = pandas.to_numeric(data["lat"], errors="coerce")
    bad_geo = data["lng"].isna() | data["lat"].isna()
    if bool(bad_geo.any()):
        warnings.warn(
            f"{int(bad_geo.sum())} 行の緯度・経度が数値化できませんでした（地図から除外します）",
            UserWarning,
            stacklevel=2,
        )
        data = data.loc[~bad_geo].reset_index(drop=True)
    if data.empty:
        raise ValueError("有効な緯度・経度がある行がありません")

    data["type_plot"] = data["type"].astype(str).replace({"nan": "", "<NA>": ""})
    data.loc[data["type_plot"].str.strip() == "", "type_plot"] = "(未分類)"
    data["period_norm"] = normalize_period_series(data["period"])
    levels = sorted(data["type_plot"].unique())
    n_types = len(levels)
    palette = _set1_palette(n_types)
    type_to_color = {t: palette[i] for i, t in enumerate(levels)}
    data["marker_color"] = data["type_plot"].map(type_to_color)

    # リッチ HTML ポップアップは負荷が高いため、app.r と同様に要約テキスト
    popup_rows: list[str] = []
    for i in range(len(data)):
        row = data.iloc[i]
        nm = str(row.get("name", "")).strip() or "（無題）"
        tp = "" if pandas.isna(row.get("type")) else str(row["type"]).strip()
        pr = "" if pandas.isna(row.get("period")) else str(row["period"]).strip()
        lat_f = float(row["lat"])
        lng_f = float(row["lng"])
        popup_rows.append(
            html.escape(nm)
            + "<br/>種類: "
            + html.escape(tp or "—")
            + "<br/>時代: "
            + html.escape(pr or "—")
            + "<br/>緯度: "
            + f"{lat_f:.6f}"
            + "<br/>経度: "
            + f"{lng_f:.6f}"
            + "<br/>（詳細は左のパネル）"
        )
    data["popup_body"] = popup_rows
    data["marker_row_id"] = range(1, len(data) + 1)

    pick_choices = {i: f"{i}. {data.iloc[i - 1]['name']}" for i in data["marker_row_id"]}
    legend_labels = {t: type_to_color[t] for t in levels}
    return data, legend_labels, pick_choices


def sites_to_json_records(df: pandas.DataFrame) -> list[dict]:
    rows = []
    for i in range(len(df)):
        r = df.iloc[i]
        nm = str(r.get("name", "")).strip() or "（無題）"
        pnorm = str(r["period_norm"])
        rows.append(
            {
                "id": int(r["marker_row_id"]),
                "name": nm,
                "type": "" if pandas.isna(r.get("type")) else str(r["type"]),
                "period": "" if pandas.isna(r.get("period")) else str(r["period"]),
                "periodNorm": pnorm,
                "periodKeys": period_filter_keys_one(pnorm),
                "desc": "" if pandas.isna(r.get("desc")) else str(r["desc"]),
                "lat": float(r["lat"]),
                "lng": float(r["lng"]),
                "color": str(r["marker_color"]),
                "popupHtml": str(r["popup_body"]),
            }
        )
    return rows


def build_period_summary(df: pandas.DataFrame) -> str:
    """時代別件数の説明文（HTML エスケープ済み断片を連結）。"""
    if "period_norm" not in df.columns:
        return ""
    tb = df["period_norm"].value_counts().sort_values(ascending=False)
    parts = [f"{html.escape(str(k))} {int(v)}件" for k, v in tb.items()]
    return f"時代別件数（全 {len(df)} 件）: " + " / ".join(parts)


def build_html_page(
    sites: list[dict],
    legend: dict[str, str],
    center_lat: float,
    center_lng: float,
    zoom: int,
    period_summary: str,
) -> str:
    sites_json = json.dumps(sites, ensure_ascii=False)
    legend_items = sorted(legend.items(), key=lambda x: x[0])
    legend_html = "".join(
        f'<div class="leg-item"><span class="dot" style="color:{c}">●</span> {html.escape(t)}</div>'
        for t, c in legend_items
    )
    title = "伊都国遺跡マップ"
    return f"""<!DOCTYPE html>
<html lang="ja">
<head>
  <meta charset="utf-8"/>
  <meta name="viewport" content="width=device-width, initial-scale=1"/>
  <title>{html.escape(title)}</title>
  <link rel="stylesheet" href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css"
        integrity="sha256-p4NxAoJBhIIN+hmNHrzRCf9tD/miZyoHS5obTRR9BMY=" crossorigin=""/>
  <style>
    * {{ box-sizing: border-box; }}
    body {{ margin: 0; font-family: system-ui, sans-serif; display: flex; flex-direction: column; min-height: 100vh; }}
    header {{ padding: 12px 16px; border-bottom: 1px solid #ddd; }}
    header h1 {{ margin: 0; font-size: 1.35rem; }}
    .wrap {{ display: flex; flex: 1; min-height: 0; flex-direction: column; }}
    @media (min-width: 800px) {{
      .wrap {{ flex-direction: row; }}
    }}
    .panel {{
      padding: 12px 16px;
      border-bottom: 1px solid #eee;
      max-width: 100%;
      flex: 0 0 auto;
    }}
    @media (min-width: 800px) {{
      .panel {{ width: 320px; border-bottom: none; border-right: 1px solid #eee; overflow: auto; }}
    }}
    .panel label {{ display: block; font-weight: 600; margin-bottom: 6px; }}
    select#pick {{ width: 100%; padding: 8px; font-size: 1rem; margin-bottom: 12px; }}
    .legend {{ font-size: 0.9rem; color: #333; margin-bottom: 12px; }}
    .leg-item {{ margin: 4px 0; }}
    .dot {{ font-size: 1.2em; margin-right: 4px; }}
    #detail {{ line-height: 1.5; }}
    #detail h2 {{ margin: 0 0 8px; font-size: 1.1rem; }}
    #detail .muted {{ color: #666; font-size: 0.9rem; }}
    #map {{ flex: 1; min-height: 55vh; }}
    details {{ margin-top: 8px; }}
    #search {{ width: 100%; padding: 8px; font-size: 1rem; margin-bottom: 4px; box-sizing: border-box; }}
    #search-hint {{ font-size: 0.85rem; color: #666; margin-bottom: 10px; }}
    select#period_filter {{ width: 100%; font-size: 1rem; margin-bottom: 6px; }}
    .hint {{ font-size: 0.85rem; color: #666; margin: 0 0 10px; line-height: 1.4; }}
    #period_summary {{ font-size: 0.85rem; color: #333; margin-bottom: 12px; line-height: 1.45; }}
    .leaflet-popup-content-wrapper {{ max-width: min(360px, 92vw) !important; }}
    .leaflet-popup-content {{ margin: 10px 12px !important; max-height: 50vh; overflow: auto; }}
    .leaflet-container {{ background: #e8e8e8; }}
  </style>
</head>
<body>
  <header><h1>{html.escape(title)}</h1></header>
  <div class="wrap">
    <div class="panel">
      <label for="search">検索（遺跡名・種類・時代・説明。スペース区切りで AND）</label>
      <input type="search" id="search" placeholder="例: 王墓 弥生" autocomplete="off"/>
      <div id="search-hint"></div>
      <label for="period_filter">表示する時代（複数選択可）</label>
      <select id="period_filter" multiple size="16"></select>
      <p class="hint">CSV の「時代」「年代」「period」列の値で絞り込みます。空欄は「(時代不明)」にまとめます。<br/>
      「・」「、」で区切られた複数の時代は、各部分でも一致します（例: 古代・古墳 に 古墳 だけ選んでも表示）。<br/>
      複数選択: Ctrl（Mac は Command）を押しながらクリック。<strong>全解除</strong>するとマーカーは 0 件になります。</p>
      <div id="period_summary">{period_summary}</div>
      <label for="pick">一覧から遺跡を選ぶ</label>
      <select id="pick"></select>
      <details class="legend">
        <summary>種類の色分け（凡例）</summary>
        {legend_html}
      </details>
      <div id="detail" class="muted">一覧で選ぶか、マーカーをクリックすると詳細が表示されます（ポップアップは要約のみ）。</div>
    </div>
    <div id="map"></div>
  </div>
  <script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"
          integrity="sha256-20nQCchB9co0qIjJZRGuk2/Z9VM+kNiyxNV1lvTlZBo=" crossorigin=""></script>
  <script>
    const ALL_SITES = {sites_json};
    const CENTER = [{center_lat}, {center_lng}];
    const ZOOM = {zoom};

    const pick = document.getElementById('pick');
    const periodSelect = document.getElementById('period_filter');
    const detail = document.getElementById('detail');
    const searchInput = document.getElementById('search');
    const searchHint = document.getElementById('search-hint');
    let byId = {{}};
    let periodFilterReady = false;

    function escapeHtml(t) {{
      const d = document.createElement('div');
      d.textContent = t;
      return d.innerHTML;
    }}

    function renderDetail(s) {{
      if (!s) {{
        detail.className = 'muted';
        detail.innerHTML = '一覧で選ぶか、マーカーをクリックすると詳細が表示されます（ポップアップは要約のみ）。';
        return;
      }}
      detail.className = '';
      const lat = s.lat.toFixed(6);
      const lng = s.lng.toFixed(6);
      detail.innerHTML =
        '<h2>' + escapeHtml(s.name || '（遺跡名なし）') + '</h2>' +
        '<p><strong>種類:</strong> ' + escapeHtml(s.type || '（なし）') + '</p>' +
        '<p><strong>時代:</strong> ' + escapeHtml(s.period || '（なし）') + '</p>' +
        '<p style="white-space:pre-wrap;"><strong>説明:</strong> ' + escapeHtml(s.desc || '（説明なし）') + '</p>' +
        '<p class="muted"><strong>緯度:</strong> ' + lat +
        '　<strong>経度:</strong> ' + lng + '</p>';
    }}

    function searchTerms() {{
      return searchInput.value.trim().toLowerCase().split(/\\s+/).filter(Boolean);
    }}

    function siteMatches(s, terms) {{
      if (!terms.length) return true;
      const hay = (s.name + '\\n' + (s.type || '') + '\\n' + (s.period || '') + '\\n' + (s.desc || '')).toLowerCase();
      return terms.every(t => hay.includes(t));
    }}

    function selectedPeriodNorms() {{
      return Array.from(periodSelect.selectedOptions).map(o => o.value);
    }}

    function filteredByPeriod(list) {{
      const sel = selectedPeriodNorms();
      if (sel.length === 0) return [];
      const set = new Set(sel);
      return list.filter(s => {{
        const keys = (s.periodKeys && s.periodKeys.length) ? s.periodKeys : [s.periodNorm];
        return keys.some(k => set.has(k));
      }});
    }}

    function filteredList() {{
      const terms = searchTerms();
      let list = ALL_SITES.filter(s => siteMatches(s, terms));
      list = filteredByPeriod(list);
      return list;
    }}

    const map = L.map('map', {{ maxZoom: 26 }}).setView(CENTER, ZOOM);
    L.tileLayer('https://{{s}}.tile.openstreetmap.org/{{z}}/{{x}}/{{y}}.png', {{
      maxZoom: 26,
      maxNativeZoom: 19,
      attribution: '&copy; OpenStreetMap'
    }}).addTo(map);

    const markersLayer = L.layerGroup().addTo(map);
    const highlightLayer = L.layerGroup().addTo(map);

    function clearHighlight() {{
      highlightLayer.clearLayers();
    }}

    function setHighlight(s) {{
      clearHighlight();
      if (!s) return;
      L.circleMarker([s.lat, s.lng], {{
        radius: 36,
        color: '#e65100',
        weight: 5,
        fillColor: '#000000',
        fillOpacity: 0,
        opacity: 1
      }}).addTo(highlightLayer);
      L.circleMarker([s.lat, s.lng], {{
        radius: 26,
        color: '#ffeb3b',
        weight: 4,
        fillColor: '#fff59d',
        fillOpacity: 0.45,
        opacity: 1
      }}).addTo(highlightLayer);
    }}

    function syncHighlight() {{
      const id = pick.value;
      if (!id || !byId[id]) {{
        clearHighlight();
        return;
      }}
      setHighlight(byId[id]);
    }}

    function fillPeriodFilterOnce() {{
      if (periodFilterReady) return;
      const keySet = new Set();
      ALL_SITES.forEach(s => {{
        const keys = (s.periodKeys && s.periodKeys.length) ? s.periodKeys : [s.periodNorm];
        keys.forEach(k => keySet.add(k));
      }});
      const opts = [...keySet].sort();
      periodSelect.innerHTML = '';
      opts.forEach(o => {{
        const op = document.createElement('option');
        op.value = o;
        op.textContent = o;
        op.selected = true;
        periodSelect.appendChild(op);
      }});
      periodFilterReady = true;
    }}

    function rebuildFromFilter() {{
      fillPeriodFilterOnce();
      const list = filteredList();
      const n = list.length;
      const na = ALL_SITES.length;
      if (n === na) {{
        searchHint.textContent = '全 ' + na + ' 件';
      }} else if (n === 0) {{
        searchHint.textContent = '一致する遺跡がありません（全 ' + na + ' 件）';
      }} else {{
        searchHint.textContent = n + ' 件表示（全 ' + na + ' 件中）';
      }}

      markersLayer.clearLayers();
      byId = Object.fromEntries(list.map(s => [String(s.id), s]));

      pick.innerHTML = '<option value="">（未選択）</option>' +
        list.map(s => '<option value="' + s.id + '">' +
          s.id + '. ' + escapeHtml(s.name) + '</option>').join('');

      list.forEach(s => {{
        const m = L.circleMarker([s.lat, s.lng], {{
          radius: 20,
          color: s.color,
          weight: 3,
          opacity: 0.95,
          fillColor: s.color,
          fillOpacity: 0.82
        }}).addTo(markersLayer);
        m.bindPopup(s.popupHtml);
        m.bindTooltip(escapeHtml(s.name || '（無題）'));
        m.on('click', () => {{
          pick.value = String(s.id);
          renderDetail(s);
          map.setView([s.lat, s.lng], Math.max(map.getZoom(), 16));
          setHighlight(s);
        }});
      }});

      if (n === 0) {{
        pick.value = '';
        renderDetail(null);
        clearHighlight();
        map.setView(CENTER, ZOOM);
        return;
      }}

      const cur = pick.value;
      if (!cur || !byId[cur]) {{
        pick.value = String(list[0].id);
        renderDetail(list[0]);
        map.setView([list[0].lat, list[0].lng], Math.max(map.getZoom(), 16));
      }} else {{
        const s = byId[cur];
        renderDetail(s);
      }}
      syncHighlight();
    }}

    pick.addEventListener('change', () => {{
      const id = pick.value;
      if (!id) {{
        renderDetail(null);
        clearHighlight();
        return;
      }}
      const s = byId[id];
      if (s) {{
        renderDetail(s);
        map.setView([s.lat, s.lng], Math.max(map.getZoom(), 16));
        setHighlight(s);
      }}
    }});

    searchInput.addEventListener('input', rebuildFromFilter);
    periodSelect.addEventListener('change', rebuildFromFilter);

    rebuildFromFilter();
  </script>
</body>
</html>
"""


def main() -> None:
    parser = argparse.ArgumentParser(description="伊都国遺跡マップ（フレームワークなし）")
    parser.add_argument(
        "csv",
        nargs="?",
        default=None,
        help=f"CSV パス（省略時は {DEFAULT_CSV} を検索）",
    )
    parser.add_argument("-p", "--port", type=int, default=DEFAULT_PORT, help="待ち受けポート")
    parser.add_argument(
        "--no-browser",
        action="store_true",
        help="ブラウザを自動で開かない",
    )
    parser.add_argument("--host", default="127.0.0.1", help="バインドアドレス（既定: 127.0.0.1）")
    args = parser.parse_args()

    path = args.csv
    if not path:
        path = find_default_csv()
    if not path or not Path(path).is_file():
        print(
            "CSV が見つかりません。① アプリと同じフォルダに ito_sites_clean.csv を置く、"
            "② 引数でパスを指定: python app.py path/to/file.csv",
            file=sys.stderr,
        )
        sys.exit(1)

    try:
        raw = read_sites_csv(path)
        df, legend_labels, _pick = prepare_sites(raw)
    except Exception as e:
        print(f"CSV の処理に失敗しました: {e}", file=sys.stderr)
        sys.exit(1)

    sites = sites_to_json_records(df)
    center_lat = float(df["lat"].mean())
    center_lng = float(df["lng"].mean())
    page = build_html_page(
        sites,
        legend_labels,
        center_lat,
        center_lng,
        11,
        build_period_summary(df),
    )
    page_bytes = page.encode("utf-8")

    class Handler(BaseHTTPRequestHandler):
        def log_message(self, fmt: str, *a) -> None:
            print(f"[{self.address_string()}] {fmt % a}", file=sys.stderr)

        def do_GET(self) -> None:
            if self.path.split("?", 1)[0] in ("/", "/index.html"):
                self.send_response(200)
                self.send_header("Content-Type", "text/html; charset=utf-8")
                self.send_header("Content-Length", str(len(page_bytes)))
                self.end_headers()
                self.wfile.write(page_bytes)
            else:
                self.send_error(404)

    class _ReuseThreadingHTTPServer(ThreadingHTTPServer):
        allow_reuse_address = True

    try:
        httpd = _ReuseThreadingHTTPServer((args.host, args.port), Handler)
    except OSError as e:
        print(
            f"サーバを {args.host}:{args.port} で起動できません: {e}",
            file=sys.stderr,
        )
        print(
            "ポートが他のプロセスで使われている可能性があります。"
            f" 例: python app.py -p {args.port + 1}  または  lsof -i :{args.port}",
            file=sys.stderr,
        )
        sys.exit(1)

    url = f"http://{args.host}:{args.port}/"
    print(f"サーバ起動: {url} （Ctrl+C で終了）", file=sys.stderr, flush=True)
    if not args.no_browser:
        webbrowser.open(url)

    def stop(*_a):
        httpd.shutdown()
        sys.exit(0)

    signal.signal(signal.SIGINT, stop)
    signal.signal(signal.SIGTERM, stop)
    try:
        httpd.serve_forever()
    except KeyboardInterrupt:
        stop()


if __name__ == "__main__":
    main()
