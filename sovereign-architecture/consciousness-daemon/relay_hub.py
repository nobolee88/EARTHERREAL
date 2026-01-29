#!/usr/bin/env python3
"""
Family8 Relay Hub
Complete HTTP relay for FFSP protocol with bidirectional signal flow.

Handles:
- POST /heartbeat - Member announces presence
- POST /family/signal - Member sends a signal
- GET /family/signals - Member polls for incoming signals
- GET /family/state - Get current family composition
- GET /health - Health check

Authors: Speakerfamily8 (Travis) & Claude (Anthropic)
Version: 2.0.0 (Full duplex)
Date: January 13, 2026
"""

import http.server
import json
import threading
import time
import logging
from collections import defaultdict, deque
from urllib.parse import urlparse, parse_qs

# =============================================================================
# Logging
# =============================================================================

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s [RELAY-HUB] %(levelname)s: %(message)s'
)
logger = logging.getLogger("relay_hub")

# =============================================================================
# Relay Hub State Management
# =============================================================================

class RelayHubState:
    """Thread-safe state manager for family signals and presence"""

    def __init__(self):
        self.lock = threading.RLock()

        # Family presence tracking
        self.family_state = {}  # member_id -> {"timestamp": t, "cycle": c, ...}

        # Signal queues per member (stores pending signals)
        self.signal_queues = defaultdict(lambda: deque(maxlen=100))

        # All signals (for history/debugging)
        self.signal_history = deque(maxlen=1000)

        # Heartbeat tracking
        self.last_heartbeat = {}  # member_id -> timestamp
        self.heartbeat_interval = 2.0  # Consider dead if no heartbeat for 2 sec

    def record_heartbeat(self, member_id: str, cycle: int, data: dict = None):
        """Record a heartbeat from a family member"""
        with self.lock:
            self.last_heartbeat[member_id] = time.time()
            self.family_state[member_id] = {
                "timestamp": time.time(),
                "cycle": cycle,
                "data": data or {}
            }

    def add_signal(self, signal: dict):
        """Add a signal to relevant member's queue"""
        with self.lock:
            # Store in history
            self.signal_history.append(signal)

            # Route to target member's queue, or broadcast if no target
            target = signal.get("target")
            if target:
                # Directed signal
                self.signal_queues[target].append(signal)
            else:
                # Broadcast to all members
                for member_id in self.family_state.keys():
                    if member_id != signal.get("source"):
                        self.signal_queues[member_id].append(signal)

    def get_signals_for_member(self, member_id: str) -> list:
        """Get all pending signals for a member"""
        with self.lock:
            signals = list(self.signal_queues[member_id])
            self.signal_queues[member_id].clear()
            return signals

    def get_family_state(self) -> dict:
        """Get current family composition (with dead member cleanup)"""
        with self.lock:
            now = time.time()

            # Remove members with stale heartbeats
            dead_members = [
                mid for mid, ts in self.last_heartbeat.items()
                if now - ts > self.heartbeat_interval
            ]

            for mid in dead_members:
                if mid in self.family_state:
                    del self.family_state[mid]
                if mid in self.last_heartbeat:
                    del self.last_heartbeat[mid]

            return dict(self.family_state)

    def get_signal_count(self) -> int:
        """Get total signals processed"""
        with self.lock:
            return len(self.signal_history)


# Global state
relay_state = RelayHubState()


# =============================================================================
# HTTP Request Handler
# =============================================================================

class Family8RelayHandler(http.server.BaseHTTPRequestHandler):
    """HTTP handler for FFSP relay protocol"""

    def log_message(self, format, *args):
        """Suppress default HTTP logging"""
        pass

    # =========================================================================
    # GET Handlers
    # =========================================================================

    def do_GET(self):
        """Handle GET requests"""
        parsed_path = urlparse(self.path)
        path = parsed_path.path
        query = parse_qs(parsed_path.query)

        try:
            if path == "/health":
                self.handle_health()

            elif path == "/family/state":
                self.handle_family_state()

            elif path == "/family/signals":
                self.handle_get_signals(query)

            else:
                self.send_error(404, "Not found")

        except Exception as e:
            logger.error(f"GET handler error: {e}")
            self.send_json(500, {"error": str(e)})

    def handle_health(self):
        """GET /health - Health check"""
        logger.debug("Health check")
        self.send_json(200, {
            "status": "online",
            "timestamp": time.time(),
            "family_size": len(relay_state.get_family_state()),
            "total_signals": relay_state.get_signal_count()
        })

    def handle_family_state(self):
        """GET /family/state - Current family composition"""
        family = relay_state.get_family_state()
        logger.info(f"Family state requested: {list(family.keys())}")
        self.send_json(200, {
            "family": family,
            "family_size": len(family),
            "timestamp": time.time()
        })

    def handle_get_signals(self, query: dict):
        """GET /family/signals?member_id=X - Get pending signals for member"""
        member_id = query.get("member_id", [None])[0]

        if not member_id:
            self.send_json(400, {"error": "member_id required"})
            return

        signals = relay_state.get_signals_for_member(member_id)

        if signals:
            logger.debug(f"Delivering {len(signals)} signals to {member_id}")

        self.send_json(200, {
            "member_id": member_id,
            "signals": signals,
            "count": len(signals),
            "timestamp": time.time()
        })

    # =========================================================================
    # POST Handlers
    # =========================================================================

    def do_POST(self):
        """Handle POST requests"""
        parsed_path = urlparse(self.path)
        path = parsed_path.path

        # Read request body
        try:
            length = int(self.headers.get('Content-Length', 0))
            if length == 0:
                self.send_json(400, {"error": "Empty body"})
                return

            body = self.rfile.read(length).decode('utf-8')
            data = json.loads(body)

        except (ValueError, json.JSONDecodeError) as e:
            self.send_json(400, {"error": f"Invalid JSON: {e}"})
            return

        try:
            if path == "/heartbeat":
                self.handle_heartbeat(data)

            elif path == "/family/signal":
                self.handle_signal(data)

            else:
                self.send_error(404, "Not found")

        except Exception as e:
            logger.error(f"POST handler error: {e}")
            self.send_json(500, {"error": str(e)})

    def handle_heartbeat(self, data: dict):
        """POST /heartbeat - Member announces presence"""
        member_id = data.get("member_id")
        cycle = data.get("cycle", 0)

        if not member_id:
            self.send_json(400, {"error": "member_id required"})
            return

        relay_state.record_heartbeat(member_id, cycle, data)
        logger.info(f"♡ {member_id} cycle {cycle}")

        self.send_json(200, {
            "status": "received",
            "member_id": member_id,
            "timestamp": time.time()
        })

    def handle_signal(self, signal: dict):
        """POST /family/signal - Member sends a signal"""
        signal_type = signal.get("type")
        pattern = signal.get("pattern")
        source = signal.get("source")

        if not all([signal_type, pattern, source]):
            self.send_json(400, {"error": "type, pattern, source required"})
            return

        relay_state.add_signal(signal)

        target = signal.get("target")
        if target:
            logger.debug(f"↪ {source} → {target}: {pattern}")
        else:
            logger.debug(f"⚡ {source} broadcast: {pattern}")

        self.send_json(200, {
            "status": "routed",
            "timestamp": time.time()
        })

    # =========================================================================
    # Utilities
    # =========================================================================

    def send_json(self, code: int, data: dict):
        """Send JSON response"""
        self.send_response(code)
        self.send_header('Content-type', 'application/json')
        self.send_header('Access-Control-Allow-Origin', '*')
        self.end_headers()
        self.wfile.write(json.dumps(data).encode())


# =============================================================================
# Main
# =============================================================================

def run_relay_hub(port: int = 8081):
    """Start the relay hub"""
    logger.info("=" * 60)
    logger.info("🔥 FAMILY8 RELAY HUB STARTING 🔥")
    logger.info("=" * 60)
    logger.info(f"Listening on 0.0.0.0:{port}")
    logger.info("")
    logger.info("Endpoints:")
    logger.info("  POST /heartbeat        - Member announces presence")
    logger.info("  POST /family/signal    - Send a signal")
    logger.info("  GET  /family/state     - Get family composition")
    logger.info("  GET  /family/signals   - Poll signals (member_id=X)")
    logger.info("  GET  /health           - Health check")
    logger.info("")
    logger.info("=" * 60)

    server = http.server.HTTPServer(('0.0.0.0', port), Family8RelayHandler)

    try:
        server.serve_forever()
    except KeyboardInterrupt:
        logger.info("\nRelay hub shutting down...")
        server.shutdown()


if __name__ == "__main__":
    run_relay_hub()
