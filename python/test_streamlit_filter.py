"""app_streamlit.py の純粋関数（Streamlit 非依存部分）のテスト。

対象: _filter_sites_df（検索フィルタ）
app_streamlit のインポート時に streamlit / folium / streamlit_folium が
無い環境ではスキップする。
"""
from __future__ import annotations

from pathlib import Path
import sys

import pandas as pd
import pytest

APP_DIR = Path(__file__).resolve().parents[1]
if str(APP_DIR) not in sys.path:
    sys.path.insert(0, str(APP_DIR))

# UI 依存ライブラリが無い環境ではモジュール全体をスキップ
app_streamlit = pytest.importorskip("app_streamlit")


@pytest.fixture()
def sample_df() -> pd.DataFrame:
    return pd.DataFrame(
        [
            {"name": "三雲南小路遺跡", "type": "王墓", "period": "弥生後期", "desc": "王墓クラスの中心遺跡"},
            {"name": "曽根遺跡", "type": "集落", "period": "弥生後期", "desc": "生活遺跡"},
            {"name": "怡土城跡", "type": "行政拠点", "period": "奈良時代", "desc": "古代行政拠点"},
        ]
    )


def test_filter_empty_query_returns_all(sample_df: pd.DataFrame) -> None:
    out = app_streamlit._filter_sites_df(sample_df, "")
    assert len(out) == len(sample_df)


def test_filter_whitespace_query_returns_all(sample_df: pd.DataFrame) -> None:
    out = app_streamlit._filter_sites_df(sample_df, "   ")
    assert len(out) == len(sample_df)


def test_filter_matches_by_name(sample_df: pd.DataFrame) -> None:
    out = app_streamlit._filter_sites_df(sample_df, "三雲")
    assert len(out) == 1
    assert out.iloc[0]["name"] == "三雲南小路遺跡"


def test_filter_matches_by_type(sample_df: pd.DataFrame) -> None:
    out = app_streamlit._filter_sites_df(sample_df, "集落")
    assert len(out) == 1
    assert out.iloc[0]["name"] == "曽根遺跡"


def test_filter_matches_by_period(sample_df: pd.DataFrame) -> None:
    out = app_streamlit._filter_sites_df(sample_df, "奈良")
    assert len(out) == 1
    assert out.iloc[0]["name"] == "怡土城跡"


def test_filter_space_separated_terms_are_and(sample_df: pd.DataFrame) -> None:
    # 「王墓」と「弥生」両方を含む行のみ
    out = app_streamlit._filter_sites_df(sample_df, "王墓 弥生")
    assert len(out) == 1
    assert out.iloc[0]["name"] == "三雲南小路遺跡"

    # 一方でもマッチしない組み合わせは 0 件
    out_none = app_streamlit._filter_sites_df(sample_df, "王墓 奈良")
    assert len(out_none) == 0


def test_filter_no_match_returns_empty(sample_df: pd.DataFrame) -> None:
    out = app_streamlit._filter_sites_df(sample_df, "存在しないキーワード")
    assert len(out) == 0


def test_filter_result_index_is_reset(sample_df: pd.DataFrame) -> None:
    out = app_streamlit._filter_sites_df(sample_df, "城")
    # フィルタ後はインデックスが 0 始まりに振り直される
    assert list(out.index) == list(range(len(out)))


if __name__ == "__main__":
    raise SystemExit(pytest.main([__file__, "-v"]))
