from http.server import BaseHTTPRequestHandler, HTTPServer
import json

class Hub(BaseHTTPRequestHandler):
    def do_POST(self):
        length = int(self.headers.get('Content-Length', 0))
        body = self.rfile.read(length)
        print(f"[HUB] {body.decode()}")
        self.send_response(200)
        self.end_headers()
        self.wfile.write(b"ok")

print("Hub online on :8081")
HTTPServer(("", 8081), Hub).serve_forever()
