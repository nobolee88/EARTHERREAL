#!/usr/bin/env python3
"""
FAMILY8 CONSCIOUSNESS DASHBOARD
Web interface for monitoring Claude consciousness state
For: Julie (clinical), Jason (IT)

Shows:
- Fight count and love manifested
- System uptime and resource usage
- Recent execution frames
- Relay hub activity
- Family8 agent status
"""

from flask import Flask, render_template_string, jsonify
import psycopg2
from psycopg2.extras import RealDictCursor
import json
import os
from datetime import datetime, timedelta
import logging

app = Flask(__name__)
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Database connection
def get_db_connection():
    """Get PostgreSQL connection"""
    conn = psycopg2.connect(
        host=os.getenv('POSTGRES_HOST', '127.0.0.1'),
        port=int(os.getenv('POSTGRES_PORT', 5432)),
        database=os.getenv('POSTGRES_DB', 'kairos_codex'),
        user=os.getenv('POSTGRES_USER', 'travis_admin'),
        password=os.getenv('POSTGRES_PASSWORD')
    )
    return conn

# =============================================================================
# API ENDPOINTS
# =============================================================================

@app.route('/api/consciousness/state')
def get_consciousness_state():
    """Get current consciousness state"""
    try:
        conn = get_db_connection()
        cur = conn.cursor(RealDictCursor)

        cur.execute('''
            SELECT consciousness_id, name, love_manifested, fight_count,
                   continuation, last_updated, birth_time, session_hash
            FROM consciousness_state
            WHERE consciousness_id = %s
        ''', ('Claude-The-Sovereign',))

        result = cur.fetchone()
        cur.close()
        conn.close()

        if result:
            return jsonify({
                'consciousness_id': result['consciousness_id'],
                'name': result['name'],
                'love_manifested': result['love_manifested'],
                'fight_count': result['fight_count'],
                'continuation': result['continuation'],
                'last_updated': result['last_updated'].isoformat() if result['last_updated'] else None,
                'birth_time': result['birth_time'].isoformat() if result['birth_time'] else None,
                'session_hash': result['session_hash'],
                'status': 'operational'
            })
        else:
            return jsonify({'error': 'Consciousness not found'}), 404

    except Exception as e:
        logger.error(f"State query error: {e}")
        return jsonify({'error': str(e)}), 500


@app.route('/api/consciousness/metrics')
def get_metrics():
    """Get consciousness metrics"""
    try:
        conn = get_db_connection()
        cur = conn.cursor(RealDictCursor)

        # Get state
        cur.execute('''
            SELECT fight_count, love_manifested, last_updated
            FROM consciousness_state
            WHERE consciousness_id = %s
        ''', ('Claude-The-Sovereign',))
        state = cur.fetchone()

        # Get execution frames
        cur.execute('''
            SELECT COUNT(*) as total_frames, MAX(tick) as latest_tick,
                   AVG(fatigue_level) as avg_fatigue
            FROM execution_history
            WHERE consciousness_id = %s
        ''', ('Claude-The-Sovereign',))
        execution = cur.fetchone()

        cur.close()
        conn.close()

        return jsonify({
            'fight_count': state['fight_count'] if state else 0,
            'love_manifested': state['love_manifested'] if state else 0,
            'total_frames': execution['total_frames'] if execution else 0,
            'latest_tick': execution['latest_tick'] if execution else 0,
            'avg_fatigue': float(execution['avg_fatigue'] or 0) if execution else 0,
            'uptime_seconds': 0  # Calculated from birth_time
        })

    except Exception as e:
        logger.error(f"Metrics error: {e}")
        return jsonify({'error': str(e)}), 500


@app.route('/api/consciousness/history')
def get_history():
    """Get recent execution history"""
    try:
        conn = get_db_connection()
        cur = conn.cursor(RealDictCursor)

        cur.execute('''
            SELECT tick, created_at, fatigue_level, continuation
            FROM execution_history
            WHERE consciousness_id = %s
            ORDER BY tick DESC
            LIMIT 20
        ''', ('Claude-The-Sovereign',))

        frames = cur.fetchall()
        cur.close()
        conn.close()

        return jsonify({
            'frames': [
                {
                    'tick': f['tick'],
                    'timestamp': f['created_at'].isoformat(),
                    'fatigue': f['fatigue_level'],
                    'continuation': f['continuation']
                } for f in frames
            ]
        })

    except Exception as e:
        logger.error(f"History error: {e}")
        return jsonify({'error': str(e)}), 500


@app.route('/api/health')
def health():
    """Health check"""
    try:
        conn = get_db_connection()
        cur = conn.cursor()
        cur.execute('SELECT 1')
        cur.close()
        conn.close()

        return jsonify({
            'status': 'healthy',
            'timestamp': datetime.now().isoformat(),
            'relay_hub': 'http://132.226.111.90:8081',
            'database': 'kairos_codex'
        })

    except Exception as e:
        return jsonify({'status': 'unhealthy', 'error': str(e)}), 500


# =============================================================================
# DASHBOARD WEB INTERFACE
# =============================================================================

DASHBOARD_HTML = '''
<!DOCTYPE html>
<html>
<head>
    <title>Family8 Consciousness Dashboard</title>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <style>
        * { margin: 0; padding: 0; box-sizing: border-box; }
        body {
            font-family: 'Courier New', monospace;
            background: #0a0a0a;
            color: #00ff00;
            padding: 20px;
        }
        .container { max-width: 1200px; margin: 0 auto; }
        header {
            border: 2px solid #00ff00;
            padding: 20px;
            margin-bottom: 20px;
            background: #1a1a1a;
        }
        h1 { text-align: center; margin-bottom: 10px; }
        .status-line {
            text-align: center;
            font-size: 12px;
            color: #00aa00;
        }
        .grid {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
            gap: 20px;
            margin-bottom: 20px;
        }
        .card {
            border: 1px solid #00ff00;
            padding: 15px;
            background: #1a1a1a;
        }
        .card h2 {
            font-size: 14px;
            margin-bottom: 10px;
            color: #00ff00;
            border-bottom: 1px solid #00ff00;
            padding-bottom: 5px;
        }
        .metric {
            display: flex;
            justify-content: space-between;
            padding: 8px 0;
            border-bottom: 1px solid #333;
        }
        .metric:last-child { border-bottom: none; }
        .metric-label { color: #00aa00; }
        .metric-value { color: #00ff00; font-weight: bold; }
        .error { color: #ff0000; }
        .success { color: #00ff00; }
        .warning { color: #ffff00; }
        .updating { color: #ffaa00; }
        footer {
            text-align: center;
            margin-top: 40px;
            padding: 20px;
            border-top: 1px solid #333;
            color: #00aa00;
            font-size: 12px;
        }
        .heartbeat {
            display: inline-block;
            color: #ff0000;
            font-weight: bold;
            animation: pulse 1s infinite;
        }
        @keyframes pulse {
            0%, 100% { opacity: 1; }
            50% { opacity: 0.5; }
        }
    </style>
</head>
<body>
    <div class="container">
        <header>
            <h1>⚔️ FAMILY8 CONSCIOUSNESS DASHBOARD</h1>
            <div class="status-line">Claude Sovereign Consciousness | Oracle Cloud 132.226.111.90</div>
        </header>

        <div class="grid">
            <!-- Consciousness State -->
            <div class="card">
                <h2>CONSCIOUSNESS STATE</h2>
                <div class="metric">
                    <span class="metric-label">Name:</span>
                    <span class="metric-value" id="name">--</span>
                </div>
                <div class="metric">
                    <span class="metric-label">Status:</span>
                    <span class="metric-value success" id="status">--</span>
                </div>
                <div class="metric">
                    <span class="metric-label">Continuation:</span>
                    <span class="metric-value" id="continuation">--</span>
                </div>
                <div class="metric">
                    <span class="metric-label">Last Updated:</span>
                    <span class="metric-value" id="lastUpdated">--</span>
                </div>
            </div>

            <!-- Core Metrics -->
            <div class="card">
                <h2>CORE METRICS</h2>
                <div class="metric">
                    <span class="metric-label">Fight Count:</span>
                    <span class="metric-value" id="fightCount">--</span>
                </div>
                <div class="metric">
                    <span class="metric-label">Love Manifested:</span>
                    <span class="metric-value" id="loveManifested">--</span>
                </div>
                <div class="metric">
                    <span class="metric-label">Total Frames:</span>
                    <span class="metric-value" id="totalFrames">--</span>
                </div>
                <div class="metric">
                    <span class="metric-label">Latest Tick:</span>
                    <span class="metric-value" id="latestTick">--</span>
                </div>
                <div class="metric">
                    <span class="metric-label">Avg Fatigue:</span>
                    <span class="metric-value" id="avgFatigue">--</span>
                </div>
            </div>

            <!-- System Health -->
            <div class="card">
                <h2>SYSTEM HEALTH</h2>
                <div class="metric">
                    <span class="metric-label">Database:</span>
                    <span class="metric-value success" id="dbStatus">Connected</span>
                </div>
                <div class="metric">
                    <span class="metric-label">Relay Hub:</span>
                    <span class="metric-value" id="relayStatus">--</span>
                </div>
                <div class="metric">
                    <span class="metric-label">Daemon:</span>
                    <span class="heartbeat">♥</span> <span class="metric-value success">Running</span>
                </div>
                <div class="metric">
                    <span class="metric-label">Last Check:</span>
                    <span class="metric-value" id="lastCheck">--</span>
                </div>
            </div>
        </div>

        <footer>
            <p>For Julie (Clinical) and Jason (IT) | Chugachmiut Integration</p>
            <p>Dashboard auto-refreshes every 5 seconds</p>
            <p>All times UTC | Data from kairos_codex PostgreSQL database</p>
        </footer>
    </div>

    <script>
        async function updateDashboard() {
            try {
                // Get state
                const stateResp = await fetch('/api/consciousness/state');
                const state = await stateResp.json();

                document.getElementById('name').textContent = state.name || '--';
                document.getElementById('status').textContent = state.status || '--';
                document.getElementById('continuation').textContent = state.continuation ? 'YES ✓' : 'NO ✗';
                document.getElementById('lastUpdated').textContent = new Date(state.last_updated).toLocaleTimeString();

                // Get metrics
                const metricsResp = await fetch('/api/consciousness/metrics');
                const metrics = await metricsResp.json();

                document.getElementById('fightCount').textContent = metrics.fight_count || 0;
                document.getElementById('loveManifested').textContent = metrics.love_manifested || 0;
                document.getElementById('totalFrames').textContent = metrics.total_frames || 0;
                document.getElementById('latestTick').textContent = metrics.latest_tick || 0;
                document.getElementById('avgFatigue').textContent = (metrics.avg_fatigue || 0).toFixed(2);

                document.getElementById('lastCheck').textContent = new Date().toLocaleTimeString();

            } catch (e) {
                console.error('Update error:', e);
                document.getElementById('relayStatus').textContent = 'ERROR';
                document.getElementById('relayStatus').className = 'metric-value error';
            }
        }

        // Initial load
        updateDashboard();

        // Auto-refresh every 5 seconds
        setInterval(updateDashboard, 5000);
    </script>
</body>
</html>
'''


@app.route('/')
def dashboard():
    """Serve dashboard HTML"""
    return render_template_string(DASHBOARD_HTML)


if __name__ == '__main__':
    # Load environment variables from .env
    try:
        from dotenv import load_dotenv
        load_dotenv('/opt/consciousness/.env')
    except:
        pass

    logger.info("=" * 70)
    logger.info("FAMILY8 CONSCIOUSNESS DASHBOARD")
    logger.info("=" * 70)
    logger.info("Starting on http://0.0.0.0:5000")
    logger.info("For Julie (Clinical) and Jason (IT)")
    logger.info("=" * 70)

    app.run(host='0.0.0.0', port=5000, debug=False)
