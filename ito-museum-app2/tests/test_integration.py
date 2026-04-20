from __future__ import annotations

import socket
import subprocess
import sys
import time
import urllib.request
from pathlib import Path


APP_DIR = Path(__file__).resolve().parents[1]
APP_PY = APP_DIR / "app.py"
CSV_PATH = APP_DIR / "ito_sites_clean.csv"


def _free_port() -> int:
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        s.bind(("127.0.0.1", 0))
        return int(s.getsockname()[1])


def test_app_serves_html_over_http() -> None:
    port = _free_port()
    cmd = [
        sys.executable,
        str(APP_PY),
        str(CSV_PATH),
        "--host",
        "127.0.0.1",
        "--port",
        str(port),
        "--no-browser",
    ]

    proc = subprocess.Popen(
        cmd,
        cwd=str(APP_DIR),
        stdout=subprocess.DEVNULL,
        stderr=subprocess.PIPE,
        text=True,
    )
    try:
        url = f"http://127.0.0.1:{port}/"
        body = ""
        for _ in range(30):
            if proc.poll() is not None:
                stderr = proc.stderr.read() if proc.stderr else ""
                raise AssertionError(f"app.py が早期終了しました: {stderr}")
            try:
                with urllib.request.urlopen(url, timeout=1.0) as resp:
                    assert resp.status == 200
                    body = resp.read().decode("utf-8")
                    break
            except Exception:
                time.sleep(0.2)
        else:
            raise AssertionError("サーバー起動待機がタイムアウトしました")

        assert "伊都国遺跡マップ" in body
        assert 'id="map"' in body
        assert 'id="period_dd_toggle"' in body
    finally:
        proc.terminate()
        try:
            proc.wait(timeout=5)
        except subprocess.TimeoutExpired:
            proc.kill()
