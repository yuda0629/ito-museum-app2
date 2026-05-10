"""
伊都国遺跡マップ — コアロジック + スタンドアロン Streamlit 版。

スタンドアロン起動:
  streamlit run app.py

app_streamlit.py からは以下をインポートして使用:
  from app import read_sites_csv, find_default_csv, prepare_sites, APP_DIR, DEFAULT_CSV
"""
from __future__ import annotations

import html as html_mod
import re
from pathlib import Path

import pandas as pd

# ---------------------------------------------------------------------------
# 定数
# ---------------------------------------------------------------------------

APP_DIR = Path(__file__).parent
DEFAULT_CSV = "ito_sites_clean.csv"

_RENAME_MAP: dict[str, str] = {
    "遺跡名": "name", "site_name": "name",
    "種類": "type",   "種別": "type",
    "時代": "period", "年代": "period",
    "経度": "lng",    "lon": "lng",
    "緯度": "lat",
    "説明": "desc",   "概要": "desc",
}

# 種類 → ピン色（CSS 色名: SVG と folium.Icon の両方で動作）
TYPE_COLORS: dict[str, str] = {
    "王墓":   "red",
    "首長墓": "orange",
    "集落":   "blue",
    "祭祀":   "purple",
    "寺社":   "darkpurple",
    "工房":   "cadetblue",
    "港湾":   "lightblue",
    "行政":   "darkgreen",
    "防衛":   "gray",
}
_DEFAULT_COLOR = "green"

# ---------------------------------------------------------------------------
# 純粋データ関数（Streamlit に依存しない）
# ---------------------------------------------------------------------------

def get_marker_color(site_type: str) -> str:
    s = str(site_type)
    if "王墓" in s or "墳" in s:
        return TYPE_COLORS["王墓"]
    if "首長墓" in s:
        return TYPE_COLORS["首長墓"]
    if "集落" in s:
        return TYPE_COLORS["集落"]
    if "祭祀" in s:
        return TYPE_COLORS["祭祀"]
    if "寺" in s or "社" in s:
        return TYPE_COLORS["寺社"]
    if "工房" in s:
        return TYPE_COLORS["工房"]
    if "港湾" in s:
        return TYPE_COLORS["港湾"]
    if "行政" in s:
        return TYPE_COLORS["行政"]
    if "防衛" in s:
        return TYPE_COLORS["防衛"]
    return _DEFAULT_COLOR


def find_default_csv() -> str | None:
    for p in [APP_DIR / DEFAULT_CSV, Path(DEFAULT_CSV)]:
        if p.is_file():
            return str(p)
    return None


def read_sites_csv(path: str | Path) -> pd.DataFrame:
    path = str(path)
    df: pd.DataFrame | None = None
    for enc in ("utf-8-sig", "utf-8", "cp932"):
        try:
            df = pd.read_csv(path, encoding=enc)
            break
        except UnicodeDecodeError:
            continue
    if df is None:
        raise ValueError(f"CSV を読み込めませんでした: {path}")

    df.columns = [re.sub(r"^﻿", "", str(c)) for c in df.columns]

    actual_rename: dict[str, str] = {}
    for k, v in _RENAME_MAP.items():
        for col in df.columns:
            if k.lower() in col.lower() and col not in actual_rename:
                actual_rename[col] = v
    df = df.rename(columns=actual_rename)

    for col in ["type", "period", "desc"]:
        if col not in df.columns:
            df[col] = "不明"

    df["lat"] = pd.to_numeric(df["lat"], errors="coerce")
    df["lng"] = pd.to_numeric(df["lng"], errors="coerce")
    return df.dropna(subset=["lat", "lng"]).reset_index(drop=True)


def prepare_sites(
    raw: pd.DataFrame,
) -> tuple[pd.DataFrame, dict[str, str], dict[int, str]]:
    """marker_color / marker_row_id / popup_body 列を付与して返す。

    Returns:
        df: 加工済み DataFrame
        legend_labels: {種類名: 色} の凡例辞書
        row_id_to_popup: {marker_row_id: popup_html}
    """
    df = raw.copy()
    df["marker_row_id"] = range(1, len(df) + 1)
    df["marker_color"] = df["type"].apply(get_marker_color)

    def _popup(row: pd.Series) -> str:
        esc = html_mod.escape
        rid    = int(row["marker_row_id"])
        name   = esc(str(row.get("name",   "")).strip() or "（無題）")
        typ    = esc(str(row.get("type",   "")).strip() or "（なし）")
        period = esc(str(row.get("period", "")).strip() or "（なし）")
        desc   = esc(str(row.get("desc",   "")).strip() or "（なし）")
        return (
            f'<span data-rid="{rid}" style="display:none"></span>'
            f"<b>{name}</b><br>種類: {typ}<br>時代: {period}<br>{desc}"
        )

    df["popup_body"] = df.apply(_popup, axis=1)

    legend_labels: dict[str, str] = {}
    for _, row in df.iterrows():
        t = str(row["type"]).strip()
        if t and t not in legend_labels:
            legend_labels[t] = str(row["marker_color"])

    row_id_to_popup: dict[int, str] = {
        int(r["marker_row_id"]): str(r["popup_body"]) for _, r in df.iterrows()
    }

    return df, legend_labels, row_id_to_popup


# ---------------------------------------------------------------------------
# スタンドアロン Streamlit UI（import 時は実行されない）
# ---------------------------------------------------------------------------

if __name__ == "__main__":
    import folium
    import streamlit as st
    from streamlit_folium import st_folium

    st.set_page_config(page_title="伊都国遺跡マップ", layout="wide")
    st.title("伊都国遺跡マップ")

    path = find_default_csv()
    if not path:
        st.error("CSV が見つかりません。ito_sites_clean.csv を同じフォルダに置いてください。")
        st.stop()

    try:
        raw = read_sites_csv(path)
        df, legend_labels, _ = prepare_sites(raw)
    except Exception as e:
        st.error(f"CSVの読み込みに失敗しました: {e}")
        st.stop()

    st.sidebar.header("フィルタ")
    search_query = st.sidebar.text_input("遺跡名で検索", "")
    all_periods = sorted(df["period"].unique().tolist())
    selected_periods = st.sidebar.multiselect("表示する時代", all_periods, default=all_periods)

    with st.sidebar.expander("種類の色分け（凡例）"):
        for t, c in sorted(legend_labels.items()):
            st.markdown(
                f"<span style='color:{c};font-size:1.3em'>●</span> {html_mod.escape(t)}",
                unsafe_allow_html=True,
            )

    filtered_df = df[df["period"].isin(selected_periods)]
    if search_query:
        filtered_df = filtered_df[filtered_df["name"].str.contains(search_query, na=False)]

    if not filtered_df.empty:
        center_lat = float(filtered_df["lat"].mean())
        center_lng = float(filtered_df["lng"].mean())
        m = folium.Map(location=[center_lat, center_lng], zoom_start=12)

        for _, row in filtered_df.iterrows():
            folium.Marker(
                location=[float(row["lat"]), float(row["lng"])],
                popup=folium.Popup(row["popup_body"], max_width=300),
                tooltip=str(row["name"]),
                icon=folium.Icon(color=str(row["marker_color"]), icon="info-sign"),
            ).add_to(m)

        st_folium(m, width="100%", height=600)
        st.subheader("遺跡詳細一覧")
        st.write(filtered_df[["name", "type", "period", "desc"]])
    else:
        st.warning("該当する遺跡がありません。")
