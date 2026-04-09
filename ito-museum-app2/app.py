from __future__ import annotations

import base64
import io
import math
from pathlib import Path
from typing import Any

import dash
import dash_leaflet as dl
import pandas as pd
from dash import ALL, Input, Output, State, callback_context, dcc, html
from plotly.colors import qualitative


APP_TITLE = "伊都国遺跡マップ (Python版)"
DEFAULT_CSV_NAME = "ito_sites_clean.csv"

TYPE_FALLBACK = "(未分類)"
EMPTY_NAME = "（遺跡名なし）"
EMPTY_TEXT = "（なし）"

PALETTE = (
    qualitative.Set1
    + qualitative.Set2
    + qualitative.Set3
    + qualitative.Bold
    + qualitative.Plotly
)


def read_sites_csv(path: str | Path) -> pd.DataFrame:
    for enc in ("utf-8", "cp932"):
        try:
            return pd.read_csv(path, encoding=enc)
        except Exception:
            pass
    raise ValueError("CSV の読み込みに失敗しました（UTF-8 / CP932 を試行）")


def strip_bom_names(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()
    out.columns = [str(c).lstrip("\ufeff") for c in out.columns]
    return out


def safe_text(x: Any, empty_label: str = EMPTY_TEXT) -> str:
    if x is None:
        return empty_label
    s = str(x).strip()
    if not s or s.lower() == "nan":
        return empty_label
    return s


def find_first_col(df: pd.DataFrame, candidates: list[str]) -> str | None:
    existing = {str(c): c for c in df.columns}
    for c in candidates:
        if c in existing:
            return existing[c]
    return None


def pick_desc_column(df: pd.DataFrame, excluded: set[str]) -> str | None:
    best_col = None
    best_mean = 0.0
    for col in df.columns:
        if col in excluded:
            continue
        s = df[col]
        if not (pd.api.types.is_string_dtype(s) or pd.api.types.is_object_dtype(s)):
            continue
        lengths = s.astype(str).str.len()
        avg = float(lengths[lengths.notna()].mean() or 0.0)
        if avg >= 8 and avg > best_mean:
            best_mean = avg
            best_col = col
    return best_col


def prepare_sites(raw: pd.DataFrame) -> pd.DataFrame:
    data = strip_bom_names(raw)

    col_map = {
        "name": ["name", "site_name", "遺跡名", "サイト名"],
        "type": ["type", "種類", "種別"],
        "period": ["period", "年代", "時代"],
        "desc": ["desc", "description", "説明", "概要", "備考", "詳細", "テキスト"],
        "lng": ["lng", "lon", "longitude", "経度", "LON", "Lng"],
        "lat": ["lat", "latitude", "緯度", "LAT", "Lat"],
    }

    renamed: dict[str, Any] = {}
    for dest, candidates in col_map.items():
        src = find_first_col(data, candidates)
        if src is not None:
            renamed[dest] = data[src]

    df = pd.DataFrame(renamed)

    for col in ("type", "period", "desc"):
        if col not in df.columns:
            df[col] = None

    required = {"name", "lng", "lat"}
    missing = [c for c in required if c not in df.columns]
    if missing:
        raise ValueError(f"必須列がありません: {', '.join(missing)}")

    desc_empty = df["desc"].isna() | (df["desc"].astype(str).str.strip() == "")
    if bool(desc_empty.all()):
        excluded = {"name", "lng", "lat", "type", "period", "desc"}
        src_col = pick_desc_column(data, excluded=excluded)
        if src_col is not None:
            df["desc"] = data[src_col].astype(str)

    df["lng"] = pd.to_numeric(df["lng"], errors="coerce")
    df["lat"] = pd.to_numeric(df["lat"], errors="coerce")
    df["has_geo"] = ~(df["lng"].isna() | df["lat"].isna())

    if not bool(df["has_geo"].any()):
        raise ValueError("有効な緯度・経度がある行がありません")

    df = df.reset_index(drop=True)
    df["marker_row_id"] = df.index + 1
    df["type_plot"] = df["type"].fillna("").astype(str).str.strip()
    df.loc[df["type_plot"] == "", "type_plot"] = TYPE_FALLBACK
    return df


def type_colors(df: pd.DataFrame) -> dict[str, str]:
    types = sorted(df["type_plot"].dropna().astype(str).unique().tolist())
    if not types:
        return {TYPE_FALLBACK: "#d62728"}
    return {t: PALETTE[i % len(PALETTE)] for i, t in enumerate(types)}


def default_csv_path() -> Path | None:
    here = Path(__file__).resolve().parent
    p = here / DEFAULT_CSV_NAME
    return p if p.exists() else None


def parse_uploaded_csv(contents: str) -> pd.DataFrame:
    _, b64 = contents.split(",", 1)
    raw = base64.b64decode(b64)
    for enc in ("utf-8", "cp932"):
        try:
            return pd.read_csv(io.StringIO(raw.decode(enc)))
        except Exception:
            pass
    raise ValueError("アップロード CSV の読み込みに失敗しました（UTF-8 / CP932）")


def detail_panel(row: dict[str, Any] | None) -> html.Div:
    if row is None:
        return html.P(
            "一覧で遺跡を選ぶか、マーカーをクリックすると詳細が表示されます。",
            style={"color": "#555", "margin": 0},
        )

    lat = row.get("lat")
    lng = row.get("lng")
    lat_txt = f"{lat:.6f}" if isinstance(lat, (int, float)) and math.isfinite(lat) else "（不明）"
    lng_txt = f"{lng:.6f}" if isinstance(lng, (int, float)) and math.isfinite(lng) else "（不明）"

    return html.Div(
        [
            html.H4(safe_text(row.get("name"), EMPTY_NAME), style={"marginTop": 0}),
            html.P([html.Strong("種類: "), safe_text(row.get("type"))]),
            html.P([html.Strong("時代: "), safe_text(row.get("period"))]),
            html.P([html.Strong("緯度: "), lat_txt]),
            html.P([html.Strong("経度: "), lng_txt]),
            html.P(
                [html.Strong("説明: "), safe_text(row.get("desc"), "（説明なし）")],
                style={"whiteSpace": "pre-wrap", "marginBottom": 0},
            ),
        ]
    )


app = dash.Dash(__name__, title=APP_TITLE)

app.layout = html.Div(
    [
        html.H2(APP_TITLE),
        html.Div(
            [
                dcc.Upload(
                    id="user-csv",
                    children=html.Button("CSV ファイルを選んで読み込む"),
                    accept=".csv,text/csv",
                ),
                html.P(
                    [
                        "未選択のときは ",
                        html.Code(DEFAULT_CSV_NAME),
                        " をアプリと同じフォルダから自動読込します。",
                    ],
                    style={"color": "#666", "fontSize": "0.9em"},
                ),
                html.Div(id="load-status", style={"marginBottom": "8px", "color": "#444"}),
                dcc.Dropdown(id="pick-site", placeholder="一覧から遺跡を選ぶ", options=[]),
                html.Div(id="tap-detail", style={"marginTop": "12px"}),
            ],
            style={
                "padding": "12px",
                "border": "1px solid #ddd",
                "borderRadius": "8px",
                "marginBottom": "12px",
            },
        ),
        dcc.Store(id="sites-store"),
        dcc.Store(id="selected-row-id"),
        dl.Map(
            id="map",
            children=[dl.TileLayer()],
            center=[33.6, 130.4],
            zoom=11,
            style={"height": "55vh", "width": "100%"},
        ),
    ],
    style={"maxWidth": "1080px", "margin": "0 auto", "padding": "12px"},
)


@app.callback(
    Output("sites-store", "data"),
    Output("load-status", "children"),
    Output("pick-site", "options"),
    Output("pick-site", "value"),
    Input("user-csv", "contents"),
    prevent_initial_call=False,
)
def load_sites(upload_contents: str | None):
    try:
        if upload_contents:
            raw = parse_uploaded_csv(upload_contents)
            src = "アップロード CSV"
        else:
            p = default_csv_path()
            if p is None:
                raise FileNotFoundError(
                    f"{DEFAULT_CSV_NAME} が見つかりません。CSV をアップロードしてください。"
                )
            raw = read_sites_csv(p)
            src = str(p)

        df = prepare_sites(raw)
        options = [
            {"label": f"{i + 1}. {safe_text(r['name'], EMPTY_NAME)}", "value": int(r["marker_row_id"])}
            for i, r in df.iterrows()
        ]
        default_val = int(df.iloc[0]["marker_row_id"]) if len(df) else None
        status = f"{len(df)} 件を読み込みました（{src}）"
        return df.to_dict("records"), status, options, default_val
    except Exception as e:
        return None, f"CSV の処理に失敗しました: {e}", [], None


@app.callback(
    Output("selected-row-id", "data"),
    Input("pick-site", "value"),
    Input({"type": "site-marker", "index": ALL}, "n_clicks"),
    State("selected-row-id", "data"),
    prevent_initial_call=True,
)
def update_selected_from_ui(picked: int | None, _marker_clicks: list[int], current: int | None):
    trig = callback_context.triggered_id
    if isinstance(trig, dict) and trig.get("type") == "site-marker":
        idx = trig.get("index")
        return int(idx) if idx is not None else current
    if picked is not None:
        return int(picked)
    return current


@app.callback(
    Output("map", "children"),
    Output("map", "center"),
    Output("map", "zoom"),
    Output("tap-detail", "children"),
    Input("sites-store", "data"),
    Input("selected-row-id", "data"),
)
def render_map(records: list[dict[str, Any]] | None, selected_row_id: int | None):
    if not records:
        return [dl.TileLayer()], [33.6, 130.4], 11, detail_panel(None)

    df = pd.DataFrame(records)
    colors = type_colors(df)
    markers = []
    for _, r in df[df["has_geo"]].iterrows():
        rid = int(r["marker_row_id"])
        name = safe_text(r["name"], EMPTY_NAME)
        marker = dl.CircleMarker(
            id={"type": "site-marker", "index": rid},
            center=[float(r["lat"]), float(r["lng"])],
            radius=20,
            color=colors.get(str(r["type_plot"]), "#d62728"),
            weight=3,
            opacity=0.95,
            fillOpacity=0.82,
            children=[
                dl.Tooltip(name),
                dl.Popup(
                    html.Div(
                        [
                            html.Div(name, style={"fontWeight": 700, "marginBottom": "8px"}),
                            html.Div([html.Span("種類: ", style={"color": "#555"}), safe_text(r["type"])]),
                            html.Div([html.Span("時代: ", style={"color": "#555"}), safe_text(r["period"])]),
                            html.Hr(),
                            html.Div(safe_text(r["desc"], "（説明なし）"), style={"whiteSpace": "pre-wrap"}),
                        ],
                        style={"lineHeight": "1.5", "maxWidth": "320px"},
                    )
                ),
            ],
        )
        markers.append(marker)

    selected = None
    if selected_row_id is not None:
        hit = df[df["marker_row_id"] == int(selected_row_id)]
        if len(hit):
            selected = hit.iloc[0].to_dict()

    if selected is None:
        hit = df[df["has_geo"]]
        if len(hit):
            selected = hit.iloc[0].to_dict()

    if selected and bool(selected.get("has_geo")):
        center = [float(selected["lat"]), float(selected["lng"])]
        zoom = 16
    else:
        geo = df[df["has_geo"]].iloc[0]
        center = [float(geo["lat"]), float(geo["lng"])]
        zoom = 11

    legend_items = [
        html.Li(
            [
                html.Span(
                    style={
                        "display": "inline-block",
                        "width": "12px",
                        "height": "12px",
                        "marginRight": "6px",
                        "backgroundColor": c,
                        "borderRadius": "50%",
                    }
                ),
                t,
            ],
            style={"listStyle": "none", "margin": 0, "padding": 0},
        )
        for t, c in colors.items()
    ]
    legend = html.Div(
        [html.Div("種類", style={"fontWeight": 700}), html.Ul(legend_items, style={"paddingLeft": 0, "margin": 0})],
        style={
            "position": "absolute",
            "right": "12px",
            "bottom": "12px",
            "background": "rgba(255,255,255,0.9)",
            "padding": "8px",
            "borderRadius": "6px",
            "zIndex": 1000,
        },
    )

    map_children = [dl.TileLayer(), dl.LayerGroup(markers), legend]
    return map_children, center, zoom, detail_panel(selected)


if __name__ == "__main__":
    app.run(debug=True)
