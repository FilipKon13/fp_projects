#!/usr/bin/env python3
from http.server import BaseHTTPRequestHandler, HTTPServer
from urllib.parse import parse_qs, urlparse
import sys

HOST = "127.0.0.1"
PORT = 2435

# In-memory key-value store: int -> str
STORE = {}

class KVHandler(BaseHTTPRequestHandler):
    # Use HTTP/1.1 or 1.0 — either is fine for the Haskell client
    protocol_version = "HTTP/1.1"

    def _send_text(self, body: str, status: int = 200):
        data = body.encode("utf-8")
        self.send_response(status)
        self.send_header("Content-Type", "text/plain; charset=utf-8")
        self.send_header("Content-Length", str(len(data)))
        # Close so Haskell's hGetContents knows when to stop
        self.send_header("Connection", "close")
        self.end_headers()
        self.wfile.write(data)

    def do_POST(self):
        if self.path != "/post":
            self._send_text("Not Found", 404)
            return

        length = int(self.headers.get("Content-Length", "0"))
        body_bytes = self.rfile.read(length)
        try:
            form = parse_qs(body_bytes.decode("utf-8"), keep_blank_values=True)
        except Exception:
            self._send_text("Bad Request", 400)
            return

        # Expect key & value
        if "key" not in form or "value" not in form:
            self._send_text("Bad Request", 400)
            return

        key_str = form["key"][0]
        value = form["value"][0]

        try:
            key = int(key_str)
        except ValueError:
            self._send_text("Bad Request", 400)
            return

        STORE[key] = value
        # The Haskell client ignores the POST response body, but "OK" is nice.
        self._send_text("OK", 200)

    def do_GET(self):
        url = urlparse(self.path)
        if url.path != "/get":
            self._send_text("Not Found", 404)
            return

        qs = parse_qs(url.query, keep_blank_values=True)
        if "key" not in qs:
            self._send_text("", 200)  # Missing key -> empty string
            return

        try:
            key = int(qs["key"][0])
        except ValueError:
            self._send_text("", 200)
            return

        # Return value as *just* the body text (no JSON), or empty string if missing
        value = STORE.get(key, "")
        self._send_text(value, 200)

    # Quiet the default logging a bit
    def log_message(self, fmt, *args):
        sys.stderr.write("[%s] %s\n" % (self.address_string(), fmt % args))


def run():
    httpd = HTTPServer((HOST, PORT), KVHandler)
    sys.stderr.write(f"HTTP KV server listening on http://{HOST}:{PORT}")
    try:
        httpd.serve_forever()
    except KeyboardInterrupt:
        print("\nShutting down...")
    finally:
        httpd.server_close()

if __name__ == "__main__":
    run()
