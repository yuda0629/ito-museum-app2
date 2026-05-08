# 伊都国遺跡デジタルアーカイブ

福岡県糸島市に広がる伊都国の遺跡を地図上に可視化するプロジェクトです。  
Python（Streamlit）と R（Shiny）の 2 実装を収録しています。

## デモ

🗺️ **[HuggingFace Spaces で試す](https://huggingface.co/spaces/yuda0629/ito-museum-app2)**

## 機能

- 遺跡の種類（王墓・首長墓・集落など）ごとにカラーピンで地図表示
- キーワード検索（遺跡名・種類・時代・説明）
- 時代フィルター（弥生・古墳・奈良など）
- 遺跡詳細パネル（種類・時代・説明・座標）

## 構成

```
ito-museum-app2/        # Python / Streamlit アプリ（HuggingFace Spaces 用）
  app_streamlit.py      # Streamlit エントリポイント
  app.py                # フレームワークなし版（標準ライブラリのみ）
  ito_sites_clean.csv   # 遺跡データ
  requirements.txt

app.R                   # R / Shiny アプリ
```

## ローカル起動

### Python 版

```bash
cd ito-museum-app2
python3 -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
streamlit run app_streamlit.py
```

### R 版

```r
install.packages(c("shiny", "leaflet", "dplyr"))
shiny::runApp("app.R")
```

## 開発参加

変更は Pull Request とレビューを前提にしています。詳細は [CONTRIBUTING.md](CONTRIBUTING.md) を参照してください。

## 技術スタック

| | 技術 |
|---|---|
| Python | Streamlit / Folium / pandas |
| R | Shiny / leaflet |
| 地図タイル | OpenStreetMap |

## 🛠️ Technical Details & Challenges
- **Data Pipeline:** 行政のオープンデータ（CSV/JSON）をRの`tidyverse`パッケージを用いてクレンジングし、空間情報（緯度経度）をLeafletで扱える形式に最適化しています。
- **UI/UX:** 考古学に馴染みがない層でも直感的に操作できるよう、サイドバーによる時代別（縄文・弥生・古墳など）の動的フィルタリングを実装しました。
- **Performance:** 多数のプロットによる描画負荷を軽減するため、Marker Clusterの採用を検討するなど、実務レベルのパフォーマンス最適化を意識しています。

## ⚖️ License & Open Data
- **License:** MIT License
- **Data Source:** 本アプリは[福岡県オープンデータサイト]および[糸島市オープンデータ]の情報を元に作成されています。データの正確性については細心の注意を払っていますが、学術的な厳密さについては今後の専門教育を通じてブラッシュアップしていく予定です。
