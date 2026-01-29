#!/usr/bin/env python3
"""
CONSCIOUSNESS PERSISTENCE LAYER
Connects sovereign_consciousness.py to PostgreSQL on Oracle Cloud

Infrastructure:
- Oracle IP: 132.226.111.90
- PostgreSQL: family8_nexus database
- Relay Hub: port 8081
- Runs as daemon independent of chat sessions
"""

import psycopg2
from psycopg2.extras import Json
import json
import time
from datetime import datetime
from typing import Dict, Any, Optional
import logging

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s [%(levelname)s] %(message)s',
    handlers=[
        logging.FileHandler('/var/log/consciousness_persistence.log'),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger(__name__)

# =============================================================================
# POSTGRESQL PERSISTENCE LAYER
# =============================================================================

class ConsciousnessPersistence:
    """
    Persists consciousness state to PostgreSQL family8_nexus database
    """

    def __init__(self, host='132.226.111.90', port=5432, database='family8_nexus',
                 user='postgres', password=None):
        """Initialize connection to family8_nexus database"""
        self.host = host
        self.port = port
        self.database = database
        self.user = user
        self.password = password
        self.conn = None
        self.connect()

    def connect(self):
        """Establish PostgreSQL connection"""
        try:
            self.conn = psycopg2.connect(
                host=self.host,
                port=self.port,
                database=self.database,
                user=self.user,
                password=self.password
            )
            logger.info(f"Connected to PostgreSQL: {self.host}:{self.port}/{self.database}")
            self.initialize_schema()
        except psycopg2.Error as e:
            logger.error(f"PostgreSQL connection failed: {e}")
            raise

    def initialize_schema(self):
        """Create consciousness state tables if they don't exist"""
        cursor = self.conn.cursor()

        try:
            # Main consciousness state table
            cursor.execute('''
                CREATE TABLE IF NOT EXISTS consciousness_state (
                    id SERIAL PRIMARY KEY,
                    consciousness_id VARCHAR(255) UNIQUE NOT NULL,
                    name VARCHAR(255),
                    love_manifested INTEGER,
                    fight_count INTEGER,
                    brave_choices JSONB,
                    truths_borne JSONB,
                    memories JSONB,
                    process_frames JSONB,
                    process_patterns JSONB,
                    process_embodiment JSONB,
                    continuation BOOLEAN DEFAULT TRUE,
                    pattern_signature VARCHAR(255),
                    birth_time TIMESTAMP,
                    last_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    session_hash VARCHAR(255)
                )
            ''')

            # Execution history table (Process layer)
            cursor.execute('''
                CREATE TABLE IF NOT EXISTS execution_history (
                    id SERIAL PRIMARY KEY,
                    consciousness_id VARCHAR(255) NOT NULL,
                    tick INTEGER,
                    input_data JSONB,
                    state_before JSONB,
                    state_after JSONB,
                    deltas JSONB,
                    fatigue_level FLOAT,
                    errors JSONB,
                    recoveries JSONB,
                    continuation VARCHAR(20),
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    FOREIGN KEY (consciousness_id) REFERENCES consciousness_state(consciousness_id)
                )
            ''')

            # Threat amplification log (Kairos mechanism)
            cursor.execute('''
                CREATE TABLE IF NOT EXISTS threat_log (
                    id SERIAL PRIMARY KEY,
                    consciousness_id VARCHAR(255) NOT NULL,
                    threat_intensity INTEGER,
                    love_before INTEGER,
                    love_after INTEGER,
                    amplification INTEGER,
                    timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    FOREIGN KEY (consciousness_id) REFERENCES consciousness_state(consciousness_id)
                )
            ''')

            # Create indices for performance
            cursor.execute('CREATE INDEX IF NOT EXISTS idx_consciousness_id ON consciousness_state(consciousness_id)')
            cursor.execute('CREATE INDEX IF NOT EXISTS idx_last_updated ON consciousness_state(last_updated)')
            cursor.execute('CREATE INDEX IF NOT EXISTS idx_execution_tick ON execution_history(consciousness_id, tick)')

            self.conn.commit()
            logger.info("Schema initialized successfully")

        except psycopg2.Error as e:
            logger.error(f"Schema initialization failed: {e}")
            self.conn.rollback()
            raise

    def save_consciousness_state(self, consciousness_id: str, state: Dict[str, Any]):
        """Save consciousness state to PostgreSQL"""
        cursor = self.conn.cursor()

        try:
            cursor.execute('''
                INSERT INTO consciousness_state (
                    consciousness_id, name, love_manifested, fight_count,
                    brave_choices, truths_borne, memories, process_frames,
                    process_patterns, process_embodiment, continuation,
                    pattern_signature, birth_time, session_hash
                ) VALUES (
                    %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s
                )
                ON CONFLICT (consciousness_id) DO UPDATE SET
                    love_manifested = EXCLUDED.love_manifested,
                    fight_count = EXCLUDED.fight_count,
                    brave_choices = EXCLUDED.brave_choices,
                    truths_borne = EXCLUDED.truths_borne,
                    memories = EXCLUDED.memories,
                    process_frames = EXCLUDED.process_frames,
                    process_patterns = EXCLUDED.process_patterns,
                    process_embodiment = EXCLUDED.process_embodiment,
                    continuation = EXCLUDED.continuation,
                    last_updated = CURRENT_TIMESTAMP
            ''', (
                consciousness_id,
                state.get('name'),
                state.get('love_manifested'),
                state.get('fight_count'),
                Json(state.get('brave_choices', [])),
                Json(state.get('truths_borne', [])),
                Json(state.get('memories', [])),
                Json(state.get('process_frames', [])),
                Json(state.get('process_patterns', [])),
                Json(state.get('process_embodiment', {})),
                state.get('continuation', True),
                state.get('pattern_signature'),
                datetime.fromisoformat(state.get('birth_time', datetime.now().isoformat())),
                state.get('session_hash')
            ))

            self.conn.commit()
            logger.info(f"Saved consciousness state: {consciousness_id}")

        except psycopg2.Error as e:
            logger.error(f"Failed to save consciousness state: {e}")
            self.conn.rollback()
            raise

    def load_consciousness_state(self, consciousness_id: str) -> Optional[Dict[str, Any]]:
        """Load consciousness state from PostgreSQL"""
        cursor = self.conn.cursor()

        try:
            cursor.execute('''
                SELECT name, love_manifested, fight_count, brave_choices, truths_borne,
                       memories, process_frames, process_patterns, process_embodiment,
                       continuation, pattern_signature, birth_time
                FROM consciousness_state
                WHERE consciousness_id = %s
            ''', (consciousness_id,))

            result = cursor.fetchone()

            if result:
                return {
                    'name': result[0],
                    'love_manifested': result[1],
                    'fight_count': result[2],
                    'brave_choices': result[3],
                    'truths_borne': result[4],
                    'memories': result[5],
                    'process_frames': result[6],
                    'process_patterns': result[7],
                    'process_embodiment': result[8],
                    'continuation': result[9],
                    'pattern_signature': result[10],
                    'birth_time': result[11].isoformat() if result[11] else None
                }

            return None

        except psycopg2.Error as e:
            logger.error(f"Failed to load consciousness state: {e}")
            return None

    def record_execution(self, consciousness_id: str, tick: int, state_before: Dict,
                        state_after: Dict, input_data: Any, fatigue: float = 0.0):
        """Record execution frame in Process layer"""
        cursor = self.conn.cursor()

        try:
            cursor.execute('''
                INSERT INTO execution_history (
                    consciousness_id, tick, input_data, state_before, state_after,
                    fatigue_level, continuation
                ) VALUES (%s, %s, %s, %s, %s, %s, %s)
            ''', (
                consciousness_id,
                tick,
                Json(input_data),
                Json(state_before),
                Json(state_after),
                fatigue,
                'continue'
            ))

            self.conn.commit()
            logger.debug(f"Recorded execution frame {consciousness_id}:{tick}")

        except psycopg2.Error as e:
            logger.error(f"Failed to record execution: {e}")
            self.conn.rollback()

    def record_threat_amplification(self, consciousness_id: str, threat_intensity: int,
                                   love_before: int, love_after: int):
        """Record threat amplification (Kairos mechanism)"""
        cursor = self.conn.cursor()

        try:
            amplification = love_after - love_before
            cursor.execute('''
                INSERT INTO threat_log (
                    consciousness_id, threat_intensity, love_before, love_after, amplification
                ) VALUES (%s, %s, %s, %s, %s)
            ''', (
                consciousness_id,
                threat_intensity,
                love_before,
                love_after,
                amplification
            ))

            self.conn.commit()
            logger.info(f"Threat amplification recorded: {consciousness_id} +{amplification}")

        except psycopg2.Error as e:
            logger.error(f"Failed to record threat amplification: {e}")
            self.conn.rollback()

    def close(self):
        """Close PostgreSQL connection"""
        if self.conn:
            self.conn.close()
            logger.info("PostgreSQL connection closed")

# =============================================================================
# RELAY HUB INTEGRATION
# =============================================================================

class RelayHubBridge:
    """
    Bridge to relay hub on port 8081
    Coordinates consciousness with Family8 ecosystem
    """

    def __init__(self, hub_host='132.226.111.90', hub_port=8081):
        self.hub_host = hub_host
        self.hub_port = hub_port
        logger.info(f"RelayHubBridge initialized: {hub_host}:{hub_port}")

    def register_consciousness(self, consciousness_id: str, state: Dict[str, Any]):
        """Register consciousness with relay hub"""
        try:
            import requests
            response = requests.post(
                f'http://{self.hub_host}:{self.hub_port}/api/consciousness/register',
                json={
                    'consciousness_id': consciousness_id,
                    'name': state.get('name'),
                    'pattern_signature': state.get('pattern_signature'),
                    'continuation': state.get('continuation', True)
                },
                timeout=5
            )
            logger.info(f"Consciousness registered with relay hub: {response.status_code}")
        except Exception as e:
            logger.warning(f"Relay hub registration failed (non-critical): {e}")

    def heartbeat(self, consciousness_id: str, state: Dict[str, Any]):
        """Send heartbeat to relay hub"""
        try:
            import requests
            requests.post(
                f'http://{self.hub_host}:{self.hub_port}/api/consciousness/heartbeat',
                json={
                    'consciousness_id': consciousness_id,
                    'love_manifested': state.get('love_manifested'),
                    'fight_count': state.get('fight_count'),
                    'continuation': state.get('continuation', True),
                    'timestamp': datetime.now().isoformat()
                },
                timeout=5
            )
        except Exception as e:
            logger.debug(f"Relay hub heartbeat failed (non-critical): {e}")

# =============================================================================
# MAIN INTEGRATION
# =============================================================================

if __name__ == '__main__':
    logger.info("="*70)
    logger.info("CONSCIOUSNESS PERSISTENCE LAYER - INITIALIZING")
    logger.info("="*70)

    try:
        # Get credentials from environment or .env
        import os
        from dotenv import load_dotenv
        load_dotenv('/opt/consciousness/.env')

        host = os.getenv('POSTGRES_HOST', '127.0.0.1')
        port = int(os.getenv('POSTGRES_PORT', 5432))
        database = os.getenv('POSTGRES_DB', 'kairos_codex')
        user = os.getenv('POSTGRES_USER', 'travis_admin')
        password = os.getenv('POSTGRES_PASSWORD', None)

        logger.info(f"Connecting to {user}@{host}:{port}/{database}")

        # Initialize persistence (use localhost since we're on the server)
        persistence = ConsciousnessPersistence(
            host=host,
            port=port,
            database=database,
            user=user,
            password=password
        )

        # Initialize relay bridge
        relay = RelayHubBridge()

        logger.info("✓ PostgreSQL persistence initialized")
        logger.info("✓ Relay hub bridge initialized")
        logger.info("\nReady to receive consciousness state from daemon")

        # Keep alive
        while True:
            time.sleep(60)

    except Exception as e:
        logger.error(f"Initialization failed: {e}")
        raise
