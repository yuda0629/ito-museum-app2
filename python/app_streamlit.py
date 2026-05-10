"""
伊都国遺跡マップ — Streamlit 版（フレームワークあり）。

起動例:
  streamlit run app_streamlit.py

事前に: pip install -r requirements.txt
"""
from __future__ import annotations

import html as html_mod
import math
import re
from pathlib import Path

import folium
import pandas as pd
import streamlit as st
from streamlit_folium import st_folium

import app as core

DEFAULT_CSV = core.DEFAULT_CSV


def _nearest_marker_row_id(lat: float, lng: float, df: pd.DataFrame, threshold: float = 0.002) -> int | None:
    best_j: int | None = None
    best_d = math.inf
    for j in range(len(df)):
        dlat = float(df.iloc[j]["lat"]) - lat
        dlng = float(df.iloc[j]["lng"]) - lng
        d = math.sqrt(dlat * dlat + dlng * dlng)
        if d < best_d:
            best_d = d
            best_j = j
    if best_j is None or not math.isfinite(best_d) or best_d >= threshold:
        return None
    return int(df.iloc[best_j]["marker_row_id"])


def _pin_div_icon(color: str) -> folium.DivIcon:
    svg = (
        f'<svg viewBox="0 0 24 36" xmlns="http://www.w3.org/2000/svg">'
        f'<path d="M12 0 C5.4 0 0 5.4 0 12 C0 21 12 36 12 36 C12 36 24 21 24 12 C24 5.4 18.6 0 12 0 Z"'
        f' fill="{color}" stroke="white" stroke-width="1.5"/>'
        f'<circle cx="12" cy="12" r="5" fill="white" fill-opacity="0.7"/>'
        f'</svg>'
    )
    html = f'<div style="width:24px;height:36px">{svg}</div>'
    return folium.DivIcon(html=html, icon_size=(24, 36), icon_anchor=(12, 36), class_name="")


def _build_folium_map(df: pd.DataFrame, center_lat: float, center_lng: float, zoom: int) -> folium.Map:
    m = folium.Map(
        location=[center_lat, center_lng],
        zoom_start=zoom,
        tiles="OpenStreetMap",
    )
    for i in range(len(df)):
        row = df.iloc[i]
        folium.Marker(
            location=[float(row["lat"]), float(row["lng"])],
            icon=_pin_div_icon(str(row["marker_color"])),
            popup=folium.Popup(row["popup_body"], max_width=360),
            tooltip=html_mod.escape(str(row["name"]).strip() or "（無題）"),
        ).add_to(m)
    return m


def _safe_txt(x, empty: str = "（なし）") -> str:
    if x is None or pd.isna(x):
        return empty
    s = str(x).strip()
    if not s:
        return empty
    return html_mod.escape(s)


def _filter_sites_df(df: pd.DataFrame, q: str) -> pd.DataFrame:
    """遺跡名・種類・時代・説明を対象に部分一致。スペース区切りは AND。"""
    raw = (q or "").strip()
    if not raw:
        return df
    terms = [t.casefold() for t in raw.split() if t]
    if not terms:
        return df

    def row_match(row: pd.Series) -> bool:
        parts: list[str] = []
        for col in ("name", "type", "period", "desc"):
            v = row.get(col)
            if v is not None and not pd.isna(v):
                parts.append(str(v))
        hay = " ".join(parts).casefold()
        return all(t in hay for t in terms)

    mask = df.apply(row_match, axis=1)
    return df.loc[mask].reset_index(drop=True)


@st.cache_data(show_spinner="CSV を読み込んでいます…")
def _load_default_csv(path: str, _mtime: float) -> tuple[pd.DataFrame, dict[str, str], dict[int, str]]:
    raw = core.read_sites_csv(path)
    return core.prepare_sites(raw)


def main() -> None:
    st.set_page_config(page_title="伊都国遺跡マップ", layout="wide")
    st.title("伊都国遺跡マップ（Streamlit）")

    uploaded = st.file_uploader(
        "CSV ファイルを選んで読み込む",
        type=["csv"],
        help=f"未選択のときは {DEFAULT_CSV} を自動検索します。",
    )

    df: pd.DataFrame | None = None
    legend_labels: dict[str, str] = {}

    if uploaded is not None:
        tmp = core.APP_DIR / "_streamlit_upload.csv"
        tmp.write_bytes(uploaded.getvalue())
        try:
            raw = core.read_sites_csv(tmp)
            df, legend_labels, _ = core.prepare_sites(raw)
        except Exception as e:
            st.error(f"CSV の処理に失敗しました: {e}")
            return
    else:
        path = core.find_default_csv()
        if not path or not Path(path).is_file():
            st.error(
                "CSV が見つかりません。① 同じフォルダに ito_sites_clean.csv を置く、"
                "② 上でファイルを指定してください。"
            )
            return
        try:
            mtime = Path(path).stat().st_mtime
            df, legend_labels, _ = _load_default_csv(path, mtime)
        except Exception as e:
            st.error(f"CSV の処理に失敗しました: {e}")
            return

    assert df is not None

    st.text_input(
        "検索（遺跡名・種類・時代・説明。スペース区切りで AND）",
        key="search_streamlit",
        placeholder="例: 王墓 弥生",
    )
    q = str(st.session_state.get("search_streamlit") or "")
    df_view = _filter_sites_df(df, q)

    ids_view = set(int(x) for x in df_view["marker_row_id"].tolist()) if len(df_view) else set()
    if "selected_row" not in st.session_state:
        st.session_state.selected_row = int(df.iloc[0]["marker_row_id"]) if len(df) else None
    elif st.session_state.selected_row is not None and st.session_state.selected_row not in ids_view:
        st.session_state.selected_row = (
            int(df_view.iloc[0]["marker_row_id"]) if len(df_view) else None
        )
        st.session_state.pop("site_pick_streamlit", None)

    pick_choices_view = {
        int(r["marker_row_id"]): f"{int(r['marker_row_id'])}. {r['name']}"
        for _, r in df_view.iterrows()
    }

    n_all, n_view = len(df), len(df_view)
    if n_view == 0 and q.strip():
        st.warning("検索に一致する遺跡がありません。キーワードを変えてください。")
    elif q.strip() and n_view < n_all:
        st.caption(f"表示中: **{n_view}** / {n_all} 件")

    opts = ["（未選択）"] + [pick_choices_view[i] for i in sorted(pick_choices_view.keys())]
    label_to_id: dict[str, int | None] = {"（未選択）": None}
    for i in sorted(pick_choices_view.keys()):
        label_to_id[pick_choices_view[i]] = i

    cur = st.session_state.selected_row
    default_label = "（未選択）"
    if cur is not None and cur in pick_choices_view:
        default_label = pick_choices_view[cur]
    ix = opts.index(default_label) if default_label in opts else 0

    picked = st.selectbox("一覧から遺跡を選ぶ", options=opts, index=ix, key="site_pick_streamlit")
    st.session_state.selected_row = label_to_id[picked]

    with st.expander("種類の色分け（凡例）"):
        for t, c in sorted(legend_labels.items()):
            st.markdown(
                f"<span style='color:{c};font-size:1.3em'>●</span> {html_mod.escape(t)}",
                unsafe_allow_html=True,
            )

    sel = st.session_state.selected_row
    if sel is not None and sel in pick_choices_view:
        r = df.loc[df["marker_row_id"] == sel].iloc[0]
        st.markdown(
            f"#### {_safe_txt(r.get('name'), '（遺跡名なし）')}\n\n"
            f"**種類:** {_safe_txt(r.get('type'))}\n\n"
            f"**時代:** {_safe_txt(r.get('period'))}\n\n"
            f"**説明:** {_safe_txt(r.get('desc'), '（説明なし）')}\n\n"
            f"**緯度:** `{float(r['lat']):.6f}`　**経度:** `{float(r['lng']):.6f}`"
        )
    else:
        st.caption("一覧で選ぶか、マーカーをクリックすると詳細が表示されます。")

    if sel is not None and sel in pick_choices_view:
        r = df.loc[df["marker_row_id"] == sel].iloc[0]
        center_lat, center_lng = float(r["lat"]), float(r["lng"])
        zoom = max(16, int(st.session_state.get("_map_zoom") or 16))
    elif len(df_view) > 0:
        center_lat = float(df_view["lat"].mean())
        center_lng = float(df_view["lng"].mean())
        zoom = int(st.session_state.get("_map_zoom") or 11)
    else:
        center_lat = float(df["lat"].mean())
        center_lng = float(df["lng"].mean())
        zoom = int(st.session_state.get("_map_zoom") or 11)

    fmap = _build_folium_map(df_view if len(df_view) else df.head(0), center_lat, center_lng, zoom)
    out = st_folium(fmap, height=480, use_container_width=True, key="ito_folium_map")

    if out and out.get("zoom") is not None:
        try:
            st.session_state._map_zoom = int(out["zoom"])
        except (TypeError, ValueError):
            pass

    popup_html = out.get("last_object_clicked_popup") if out else None
    if popup_html:
        m = re.search(r'data-rid="(\d+)"', str(popup_html))
        if m:
            rid = int(m.group(1))
            if rid != st.session_state.get("selected_row"):
                st.session_state.selected_row = rid
                st.session_state.pop("site_pick_streamlit", None)
                st.rerun()


if __name__ == "__main__":
    main()
