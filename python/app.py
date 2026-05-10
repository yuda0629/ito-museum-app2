import streamlit as st
import pandas as pd
import folium
from streamlit_folium import st_folium
import re
import html

# --- 設定 ---
st.set_page_config(page_title="伊都国遺跡マップ", layout="wide")

# --- 1. データの読み込み ---
@st.cache_data
def load_data():
    path = "ito_sites_clean.csv"
    try:
        # 複数のエンコーディングを試行
        for enc in ("utf-8-sig", "utf-8", "cp932"):
            try:
                df = pd.read_csv(path, encoding=enc)
                break
            except UnicodeDecodeError:
                continue
        
        # BOM削除と列名正規化
        df.columns = [re.sub(r"^\ufeff", "", str(c)) for c in df.columns]
        
        # 列名のマッピング
        rename_map = {
            "遺跡名": "name", "site_name": "name",
            "種類": "type", "種別": "type",
            "時代": "period", "年代": "period",
            "経度": "lng", "lng": "lng", "lon": "lng",
            "緯度": "lat", "lat": "lat",
            "説明": "desc", "概要": "desc"
        }
        # 既存の列名からマッピングに合うものを探してリネーム
        actual_rename = {}
        for k, v in rename_map.items():
            for col in df.columns:
                if k.lower() in col.lower():
                    actual_rename[col] = v
        df = df.rename(columns=actual_rename)
        
        # 必須列の補完
        for col in ["type", "period", "desc"]:
            if col not in df.columns:
                df[col] = "不明"
        
        # 数値変換と欠損値削除
        df["lat"] = pd.to_numeric(df["lat"], errors="coerce")
        df["lng"] = pd.to_numeric(df["lng"], errors="coerce")
        df = df.dropna(subset=["lat", "lng"])
        
        return df
    except Exception as e:
        st.error(f"CSVの読み込みに失敗しました: {e}")
        return None

# --- 2. ピンの色を決定する関数 (ここを編集して色を変えられます) ---
def get_marker_color(site_type):
    site_type = str(site_type)
    if "王墓" in site_type or "墳" in site_type:
        return "red"        # 古墳系は赤
    elif "集落" in site_type:
        return "blue"       # 集落系は青
    elif "寺" in site_type or "社" in site_type:
        return "purple"     # 寺社系は紫
    else:
        return "green"      # その他は緑

# --- 3. メイン処理 ---
df = load_data()

if df is not None:
    st.title("伊都国遺跡マップ")

    # サイドバー：フィルタリング
    st.sidebar.header("フィルタ")
    
    # 検索機能
    search_query = st.sidebar.text_input("遺跡名で検索", "")
    
    # 時代で絞り込み
    all_periods = sorted(df["period"].unique().tolist())
    selected_periods = st.sidebar.multiselect("表示する時代", all_periods, default=all_periods)
    
    # データの抽出
    filtered_df = df[df["period"].isin(selected_periods)]
    if search_query:
        filtered_df = filtered_df[filtered_df["name"].str.contains(search_query, na=False)]

    # 地図の作成
    if not filtered_df.empty:
        center_lat = filtered_df["lat"].mean()
        center_lng = filtered_df["lng"].mean()
        m = folium.Map(location=[center_lat, center_lng], zoom_start=12)

        for _, row in filtered_df.iterrows():
            color = get_marker_color(row["type"])
            folium.Marker(
                location=[row["lat"], row["lng"]],
                popup=folium.Popup(f"<b>{row['name']}</b><br>{row['desc']}", max_width=300),
                tooltip=row["name"],
                icon=folium.Icon(color=color, icon="info-sign")
            ).add_to(m)

        # 地図の表示
        st_folium(m, width="100%", height=600)
        
        # 詳細データテーブル
        st.subheader("遺跡詳細一覧")
        st.write(filtered_df[["name", "type", "period", "desc"]])
    else:
        st.warning("該当する遺跡がありません。")