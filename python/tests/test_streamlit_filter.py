"""app_streamlit.py の純粋関数（Streamlit 非依存部分）のテスト。"""
from __future__ import annotations
from pathlib import Path
import sys
import pandas as pd
import pytest

APP_DIR = Path(__file__).resolve().parents[1]
if str(APP_DIR) not in sys.path:
    sys.path.insert(0, str(APP_DIR))

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


def test_filter_empty_query_returns_all(sample_df):
    assert len(app_streamlit._filter_sites_df(sample_df, "")) == len(sample_df)


def test_filter_whitespace_query_returns_all(sample_df):
    assert len(app_streamlit._filter_sites_df(sample_df, "   ")) == len(sample_df)


def test_filter_matches_by_name(sample_df):
    out = app_streamlit._filter_sites_df(sample_df, "三雲")
    assert len(out) == 1 and out.iloc[0]["name"] == "三雲南小路遺跡"


def test_filter_matches_by_type(sample_df):
    out = app_streamlit._filter_sites_df(sample_df, "集落")
    assert len(out) == 1 and out.iloc[0]["name"] == "曽根遺跡"


def test_filter_matches_by_period(sample_df):
    out = app_streamlit._filter_sites_df(sample_df, "奈良")
    assert len(out) == 1 and out.iloc[0]["name"] == "怡土城跡"


def test_filter_space_separated_terms_are_and(sample_df):
    assert len(app_streamlit._filter_sites_df(sample_df, "王墓 弥生")) == 1
    assert len(app_streamlit._filter_sites_df(sample_df, "王墓 奈良")) == 0


def test_filter_no_match_returns_empty(sample_df):
    assert len(app_streamlit._filter_sites_df(sample_df, "存在しないキーワード")) == 0


def test_filter_result_index_is_reset(sample_df):
    out = app_streamlit._filter_sites_df(sample_df, "城")
    assert list(out.index) == list(range(len(out)))
