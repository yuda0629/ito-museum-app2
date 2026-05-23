"""app.py のコアロジック（Streamlit 非依存部分）のテスト。

対象: get_marker_color / find_default_csv / read_sites_csv / prepare_sites
これらは app_streamlit.py からインポートして使われる純粋データ関数。
"""
from __future__ import annotations

from pathlib import Path
import sys

import pandas as pd
import pytest

APP_DIR = Path(__file__).resolve().parents[1]
if str(APP_DIR) not in sys.path:
    sys.path.insert(0, str(APP_DIR))

import app  # noqa: E402


# ---------------------------------------------------------------------------
# get_marker_color
# ---------------------------------------------------------------------------

def test_get_marker_color_known_types() -> None:
    assert app.get_marker_color("王墓") == app.TYPE_COLORS["王墓"]
    assert app.get_marker_color("首長墓") == app.TYPE_COLORS["首長墓"]
    assert app.get_marker_color("集落") == app.TYPE_COLORS["集落"]
    assert app.get_marker_color("祭祀") == app.TYPE_COLORS["祭祀"]


def test_get_marker_color_partial_match() -> None:
    # 「古墳」は「墳」を含むため王墓カテゴリの色になる
    assert app.get_marker_color("古墳") == app.TYPE_COLORS["王墓"]
    # 「防衛施設」は「防衛」を含む
    assert app.get_marker_color("防衛施設") == app.TYPE_COLORS["防衛"]
    # 「行政拠点」は「行政」を含む
    assert app.get_marker_color("行政拠点") == app.TYPE_COLORS["行政"]


def test_get_marker_color_unknown_type_uses_default() -> None:
    assert app.get_marker_color("未知のカテゴリ") == app._DEFAULT_COLOR
    assert app.get_marker_color("") == app._DEFAULT_COLOR


# ---------------------------------------------------------------------------
# find_default_csv
# ---------------------------------------------------------------------------

def test_find_default_csv_locates_bundled_file() -> None:
    path = app.find_default_csv()
    assert path is not None
    assert Path(path).is_file()
    assert Path(path).name == app.DEFAULT_CSV


# ---------------------------------------------------------------------------
# read_sites_csv
# ---------------------------------------------------------------------------

def test_read_sites_csv_loads_bundled_data() -> None:
    path = app.find_default_csv()
    assert path is not None
    df = app.read_sites_csv(path)
    # 必須列が揃っている
    for col in ("name", "lat", "lng", "type", "period", "desc"):
        assert col in df.columns
    # 緯度経度は数値化されている
    assert pd.api.types.is_numeric_dtype(df["lat"])
    assert pd.api.types.is_numeric_dtype(df["lng"])
    # 1 件以上読み込めている
    assert len(df) > 0


def test_read_sites_csv_renames_japanese_headers(tmp_path: Path) -> None:
    csv = tmp_path / "jp.csv"
    csv.write_text(
        "遺跡名,緯度,経度,種類,時代,説明\n"
        "テスト遺跡,33.5,130.2,集落,弥生時代,説明文\n",
        encoding="utf-8",
    )
    df = app.read_sites_csv(csv)
    assert list(df.columns)[:6] == ["name", "lat", "lng", "type", "period", "desc"]
    assert df.iloc[0]["name"] == "テスト遺跡"


def test_read_sites_csv_drops_rows_without_coordinates(tmp_path: Path) -> None:
    csv = tmp_path / "bad.csv"
    csv.write_text(
        "name,lat,lng,type\n"
        "良い遺跡,33.5,130.2,集落\n"
        "座標なし遺跡,,,集落\n",
        encoding="utf-8",
    )
    df = app.read_sites_csv(csv)
    # 座標が欠損した行は除外される
    assert len(df) == 1
    assert df.iloc[0]["name"] == "良い遺跡"


def test_read_sites_csv_fills_missing_optional_columns(tmp_path: Path) -> None:
    csv = tmp_path / "minimal.csv"
    csv.write_text(
        "name,lat,lng\n"
        "最小遺跡,33.5,130.2\n",
        encoding="utf-8",
    )
    df = app.read_sites_csv(csv)
    # type / period / desc が無くても補完される
    for col in ("type", "period", "desc"):
        assert col in df.columns


# ---------------------------------------------------------------------------
# prepare_sites
# ---------------------------------------------------------------------------

def test_prepare_sites_adds_marker_columns() -> None:
    path = app.find_default_csv()
    raw = app.read_sites_csv(path)
    df, legend_labels, row_id_to_popup = app.prepare_sites(raw)

    # 付与される列
    for col in ("marker_row_id", "marker_color", "popup_body"):
        assert col in df.columns

    # marker_row_id は 1 始まりの連番
    assert list(df["marker_row_id"]) == list(range(1, len(df) + 1))

    # 凡例は {種類: 色} の辞書
    assert isinstance(legend_labels, dict)
    assert len(legend_labels) > 0

    # popup 辞書のキーは marker_row_id と一致
    assert set(row_id_to_popup.keys()) == set(df["marker_row_id"])


def test_prepare_sites_popup_embeds_row_id() -> None:
    path = app.find_default_csv()
    raw = app.read_sites_csv(path)
    df, _, _ = app.prepare_sites(raw)
    first = df.iloc[0]
    # popup_body にはクリック連動用の data-rid が埋め込まれる
    assert f'data-rid="{int(first["marker_row_id"])}"' in first["popup_body"]


def test_prepare_sites_escapes_html_in_popup(tmp_path: Path) -> None:
    csv = tmp_path / "xss.csv"
    csv.write_text(
        "name,lat,lng,type,period,desc\n"
        '<script>alert(1)</script>,33.5,130.2,集落,弥生時代,説明\n',
        encoding="utf-8",
    )
    raw = app.read_sites_csv(csv)
    df, _, _ = app.prepare_sites(raw)
    popup = df.iloc[0]["popup_body"]
    # 生の <script> タグは popup に含まれない（エスケープ済み）
    assert "<script>" not in popup
    assert "&lt;script&gt;" in popup


if __name__ == "__main__":
    raise SystemExit(pytest.main([__file__, "-v"]))
