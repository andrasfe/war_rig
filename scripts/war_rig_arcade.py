#!/usr/bin/env python3
"""Mainframe Agentic Documentation — browser-based real-time agent visualization.

A tiny HTTP server that serves a dashboard for monitoring the documentation
pipeline. Same data as war_rig_status.py but rendered as a colorful,
animated visualization in the browser.

Usage:
    python scripts/war_rig_arcade.py output/.war_rig_tickets.json
    python scripts/war_rig_arcade.py output/.war_rig_tickets.json --port 8080
"""

from __future__ import annotations

import argparse
import json
import sys
from http.server import HTTPServer, SimpleHTTPRequestHandler
from pathlib import Path

# Resolve HTML file path relative to this script
_SCRIPT_DIR = Path(__file__).resolve().parent
_HTML_FILE = _SCRIPT_DIR / "war_rig_arcade.html"

# Will be set by main() before server starts
_tickets_path: Path = Path(".")


class ArcadeHandler(SimpleHTTPRequestHandler):
    """Serves the arcade HTML and tickets JSON API."""

    def do_GET(self) -> None:
        if self.path == "/" or self.path == "/index.html":
            self._serve_html()
        elif self.path == "/api/tickets":
            self._serve_tickets()
        else:
            self.send_error(404)

    def _serve_html(self) -> None:
        try:
            content = _HTML_FILE.read_bytes()
        except OSError:
            self.send_error(500, f"Cannot read {_HTML_FILE}")
            return
        self.send_response(200)
        self.send_header("Content-Type", "text/html; charset=utf-8")
        self.send_header("Content-Length", str(len(content)))
        self.end_headers()
        self.wfile.write(content)

    def _serve_tickets(self) -> None:
        try:
            content = _tickets_path.read_bytes()
            # Validate it's parseable JSON
            json.loads(content)
        except (OSError, json.JSONDecodeError):
            payload = b'{"tickets":[],"ticket_count":0,"error":"no_file"}'
            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.send_header("Content-Length", str(len(payload)))
            self.end_headers()
            self.wfile.write(payload)
            return
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(content)))
        self.send_header("Cache-Control", "no-cache")
        self.end_headers()
        self.wfile.write(content)

    def log_message(self, format: str, *args: object) -> None:
        # Suppress per-request logs; only show startup message
        pass


def main() -> None:
    global _tickets_path

    parser = argparse.ArgumentParser(
        description="Mainframe Agentic Documentation — browser-based visualization"
    )
    parser.add_argument("file", type=Path, help="Path to .war_rig_tickets.json")
    parser.add_argument("--port", type=int, default=8080, help="HTTP port (default 8080)")
    args = parser.parse_args()

    _tickets_path = args.file if args.file.is_absolute() else Path.cwd() / args.file

    if not _HTML_FILE.exists():
        print(f"Error: HTML file not found: {_HTML_FILE}", file=sys.stderr)
        sys.exit(1)

    server = HTTPServer(("0.0.0.0", args.port), ArcadeHandler)
    print(f"Mainframe Agentic Documentation dashboard on http://localhost:{args.port}")
    print(f"Tickets file: {_tickets_path}")
    print("Press Ctrl+C to stop")
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        print("\nShutting down.")
        server.shutdown()


if __name__ == "__main__":
    main()
