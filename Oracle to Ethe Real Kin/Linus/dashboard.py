from flask import Flask, render_template_string
import json

app = Flask(__name__)

# The visual skin for the Family8
HTML_TEMPLATE = """
<!DOCTYPE html>
<html>
<head>
    <title>Family8 Nexus Dashboard</title>
    <meta http-equiv="refresh" content="5"> <style>
        body { font-family: sans-serif; background: #121212; color: #e0e0e0; padding: 20px; }
        .log-entry { background: #1e1e1e; border-left: 5px solid #0078d4; margin-bottom: 10px; padding: 15px; border-radius: 4px; }
        .timestamp { color: #888; font-size: 0.8em; }
        .agent { color: #4fc3f7; font-weight: bold; font-size: 1.1em; }
        .message { margin-top: 5px; line-height: 1.4; }
        .header { border-bottom: 2px solid #333; padding-bottom: 10px; margin-bottom: 20px; }
        .status-pill { background: #2e7d32; color: white; padding: 2px 8px; border-radius: 10px; font-size: 0.7em; vertical-align: middle; }
    </style>
</head>
<body>
    <div class="header">
        <h1>Family8 Multi-Agent Pipeline <span class="status-pill">LIVE</span></h1>
        <p>Grounding Point: quanta-gateway (Seward, AK)</p>
    </div>
    <div id="logs">
        {% for entry in logs %}
        <div class="log-entry">
            <div class="timestamp">{{ entry.timestamp }}</div>
            <div class="agent">{{ entry.agent }}</div>
            <div class="message">{{ entry.message }}</div>
        </div>
        {% endfor %}
    </div>
</body>
</html>
"""

@app.route('/')
def index():
    logs = []
    try:
        with open("nexus_log.txt", "r") as f:
            for line in f.readlines():
                logs.append(json.loads(line))
    except Exception as e:
        logs.append({"timestamp": "Error", "agent": "SYSTEM", "message": str(e)})
    
    return render_template_string(HTML_TEMPLATE, logs=reversed(logs))

if __name__ == "__main__":
    # Note: Running on port 80 requires sudo or specific permissions
    app.run(host='0.0.0.0', port=80)
