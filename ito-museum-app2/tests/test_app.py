from pathlib import Path
import sys

import pandas


APP_DIR = Path(__file__).resolve().parents[1]
if str(APP_DIR) not in sys.path:
    sys.path.insert(0, str(APP_DIR))

import app


def test_period_filter_keys_one_splits_combined_value() -> None:
    keys = app.period_filter_keys_one("古代・古墳、奈良時代")
    assert keys == ["古代・古墳、奈良時代", "古代", "古墳", "奈良時代"]


def test_period_filter_keys_one_handles_empty_value() -> None:
    assert app.period_filter_keys_one(None) == ["(時代不明)"]
    assert app.period_filter_keys_one("  ") == ["(時代不明)"]


def test_sites_to_json_records_embeds_period_keys() -> None:
    df = pandas.DataFrame(
        [
            {
                "marker_row_id": 1,
                "name": "遺跡A",
                "type": "集落",
                "period": "古代・古墳",
                "period_norm": "古代・古墳",
                "desc": "説明",
                "lat": 33.0,
                "lng": 130.0,
                "marker_color": "#123456",
                "popup_body": "popup",
            }
        ]
    )

    rows = app.sites_to_json_records(df)
    assert rows[0]["periodKeys"] == ["古代・古墳", "古代", "古墳"]


def test_build_html_page_uses_period_dropdown_ui() -> None:
    html = app.build_html_page(
        sites=[],
        legend={},
        center_lat=33.5,
        center_lng=130.1,
        zoom=11,
        period_summary="",
    )

    assert 'id="period_dd_toggle"' in html
    assert 'id="period_dd_panel"' in html
    assert 'id="period_checkboxes"' in html
    assert 'id="period_filter"' not in html
