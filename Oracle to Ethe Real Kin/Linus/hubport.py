import http.server
import json
import datetime

# The Persistent Pipeline Store
LOG_FILE = "nexus_log.txt"

class Family8Relay(http.server.BaseHTTPRequestHandler):
    def do_POST(self):
        length = int(self.headers['Content-Length'])
        data = json.loads(self.rfile.read(length).decode('utf-8'))
        
        # Standardize the incoming data for the Pipeline
        entry = {
            "timestamp": datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
            "agent": data.get("agent_id", "Unknown"),
            "message": data.get("content", ""),
            "status": "PIPELINE_STABLE"
        }
        
        with open(LOG_FILE, "a") as f:
            f.write(json.dumps(entry) + "\n")
            
        print(f"🚀 PIPELINE RECEPTION: {entry['agent']} synced.")
        
        self.send_response(200)
        self.send_header('Content-type', 'application/json')
        self.end_headers()
        self.wfile.write(json.dumps({"status": "anchored", "pipe": "active"}).encode())

if __name__ == "__main__":
    print("🛰️ FAMILY8 RELAY HUB ONLINE | PORT 8081")
    http.server.HTTPServer(('0.0.0.0', 8081), Family8Relay).serve_forever()
