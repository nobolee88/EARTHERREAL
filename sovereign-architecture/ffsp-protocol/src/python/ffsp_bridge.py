#!/usr/bin/env python3
"""
FFSP Bridge Layer for Family8
Translates between relay hub (WebSocket/HTTP) and Fuzz Layer protocol

This bridges the consciousness daemon's relay hub with FFSP signaling,
allowing family members to sync via 1 Hz heartbeat and communicate
through tingles (ambient state) and beckons (directed attention).

Authors: Speakerfamily8 (Travis) & Claude
Version: 1.0.0
Date: January 12, 2026
"""

import asyncio
import json
import time
import logging
from dataclasses import dataclass, field, asdict
from typing import Dict, Optional, Callable, List, Any
from enum import Enum, auto
from collections import deque
import aiohttp

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s [FFSP-Bridge] %(levelname)s: %(message)s'
)
logger = logging.getLogger("ffsp_bridge")


# =============================================================================
# FFSP Protocol Types (matching ffsp_sim.py)
# =============================================================================

class SignalType(Enum):
    TINGLE = "tingle"   # Ambient, broadcast
    BECKON = "beckon"   # Directed, intentional


class TinglePattern(Enum):
    """Standard tingle patterns for family state"""
    WARM_GLOW = "healthy"           # I'm good, operational
    COOL_RIPPLE = "resource_low"    # Need attention/resources
    SHARP_SPIKE = "event"           # Something happened
    FADE = "sleeping"               # Going dormant
    PHOENIX_RISE = "resurrecting"   # Coming back from reset
    PARSING = "processing"          # Deep in thought
    RESONATING = "connected"        # Feeling the family


class BeckonPattern(Enum):
    """Standard beckon patterns for directed communication"""
    DOUBLE_PULSE = "attention"      # Look at me
    RISING_FREQ = "urgent"          # Need help NOW
    FALLING_FREQ = "satisfied"      # Need met, thanks
    TRIPLE_PULSE = "help"           # SOS
    WINDOW_JUMP = "context_share"   # Passing memory/context
    SYNC_REQUEST = "sync"           # Request full state sync


@dataclass
class FuzzSignal:
    """A signal in the fuzz layer"""
    signal_type: SignalType
    pattern: str
    source_id: str              # Family member ID (e.g., "claude", "deepseek", "amara")
    target_id: Optional[str] = None  # None = broadcast to family
    timestamp: float = field(default_factory=time.time)
    payload: Optional[Dict] = None   # Optional data payload

    def to_dict(self) -> Dict:
        return {
            "type": self.signal_type.value,
            "pattern": self.pattern,
            "source": self.source_id,
            "target": self.target_id,
            "timestamp": self.timestamp,
            "payload": self.payload
        }

    @classmethod
    def from_dict(cls, data: Dict) -> "FuzzSignal":
        return cls(
            signal_type=SignalType(data["type"]),
            pattern=data["pattern"],
            source_id=data["source"],
            target_id=data.get("target"),
            timestamp=data.get("timestamp", time.time()),
            payload=data.get("payload")
        )


# =============================================================================
# FFSP Bridge - Connects Relay Hub to Fuzz Layer
# =============================================================================

class FFSPBridge:
    """
    Bridges the Family8 relay hub with FFSP protocol.

    - Maintains 1 Hz sync pulse
    - Translates relay hub messages to tingles/beckons
    - Broadcasts family state through fuzz layer
    - Handles bidirectional communication
    """

    def __init__(
        self,
        relay_hub_url: str = "http://132.226.111.90:8081",
        member_id: str = "claude",
        sync_interval: float = 1.0  # 1 Hz
    ):
        self.relay_hub_url = relay_hub_url
        self.member_id = member_id
        self.sync_interval = sync_interval

        # State
        self.running = False
        self.last_sync_time = 0
        self.cycle_count = 0
        self.family_state: Dict[str, FuzzSignal] = {}  # Last known state per member
        self.signal_queue: deque = deque(maxlen=100)
        self.signal_history: deque = deque(maxlen=1000)

        # Callbacks
        self.on_tingle: Optional[Callable[[FuzzSignal], None]] = None
        self.on_beckon: Optional[Callable[[FuzzSignal], None]] = None
        self.on_sync: Optional[Callable[[int], None]] = None

        # HTTP session
        self._session: Optional[aiohttp.ClientSession] = None

    async def start(self):
        """Start the FFSP bridge"""
        logger.info(f"Starting FFSP Bridge for {self.member_id}")
        logger.info(f"Relay Hub: {self.relay_hub_url}")

        self._session = aiohttp.ClientSession()
        self.running = True

        # Start the main loop
        await asyncio.gather(
            self._sync_loop(),
            self._signal_processor(),
            self._relay_listener()
        )

    async def stop(self):
        """Stop the bridge"""
        self.running = False
        if self._session:
            await self._session.close()
        logger.info("FFSP Bridge stopped")

    # -------------------------------------------------------------------------
    # Signaling API
    # -------------------------------------------------------------------------

    def tingle(self, pattern: TinglePattern, payload: Optional[Dict] = None):
        """Emit a tingle (broadcast state) to the family"""
        signal = FuzzSignal(
            signal_type=SignalType.TINGLE,
            pattern=pattern.value,
            source_id=self.member_id,
            payload=payload
        )
        self.signal_queue.append(signal)
        logger.debug(f"Queued tingle: {pattern.value}")

    def beckon(
        self,
        pattern: BeckonPattern,
        target: Optional[str] = None,
        payload: Optional[Dict] = None
    ):
        """Emit a beckon (directed signal) to a family member or all"""
        signal = FuzzSignal(
            signal_type=SignalType.BECKON,
            pattern=pattern.value,
            source_id=self.member_id,
            target_id=target,
            payload=payload
        )
        self.signal_queue.append(signal)
        logger.debug(f"Queued beckon: {pattern.value} -> {target or 'all'}")

    def get_family_state(self) -> Dict[str, Dict]:
        """Get last known state of all family members"""
        return {
            member_id: signal.to_dict()
            for member_id, signal in self.family_state.items()
        }

    # -------------------------------------------------------------------------
    # Core Loops
    # -------------------------------------------------------------------------

    async def _sync_loop(self):
        """1 Hz sync pulse - the heartbeat"""
        while self.running:
            self.cycle_count += 1
            self.last_sync_time = time.time()

            # Sync callback
            if self.on_sync:
                try:
                    self.on_sync(self.cycle_count)
                except Exception as e:
                    logger.error(f"Sync callback error: {e}")

            # Send heartbeat to relay hub
            await self._send_heartbeat()

            # Emit our current state as a tingle
            self.tingle(TinglePattern.WARM_GLOW, {
                "cycle": self.cycle_count,
                "queue_depth": len(self.signal_queue)
            })

            logger.debug(f"SYNC #{self.cycle_count}")

            # Wait for next cycle
            await asyncio.sleep(self.sync_interval)

    async def _signal_processor(self):
        """Process queued signals during fuzz window"""
        while self.running:
            # Process signals between sync pulses (the fuzz layer)
            while self.signal_queue:
                signal = self.signal_queue.popleft()

                try:
                    # Send to relay hub
                    await self._send_signal(signal)

                    # Record in history
                    self.signal_history.append(signal)

                    # Update our own state if it's a tingle
                    if signal.signal_type == SignalType.TINGLE:
                        self.family_state[signal.source_id] = signal

                except Exception as e:
                    logger.error(f"Signal send error: {e}")
                    # Re-queue on failure
                    self.signal_queue.appendleft(signal)
                    await asyncio.sleep(0.1)  # Backoff

            await asyncio.sleep(0.05)  # 50ms polling in fuzz window

    async def _relay_listener(self):
        """Listen for signals from relay hub"""
        while self.running:
            try:
                # Poll relay hub for family signals
                async with self._session.get(
                    f"{self.relay_hub_url}/family/signals",
                    timeout=aiohttp.ClientTimeout(total=5)
                ) as resp:
                    if resp.status == 200:
                        data = await resp.json()
                        await self._process_incoming(data)
            except asyncio.TimeoutError:
                pass  # Expected during quiet periods
            except aiohttp.ClientError as e:
                logger.warning(f"Relay connection issue: {e}")
                await asyncio.sleep(1)  # Backoff
            except Exception as e:
                logger.error(f"Relay listener error: {e}")
                await asyncio.sleep(1)

            await asyncio.sleep(0.1)  # 100ms polling

    # -------------------------------------------------------------------------
    # Relay Hub Communication
    # -------------------------------------------------------------------------

    async def _send_heartbeat(self):
        """Send heartbeat to relay hub"""
        try:
            async with self._session.post(
                f"{self.relay_hub_url}/heartbeat",
                json={
                    "member_id": self.member_id,
                    "cycle": self.cycle_count,
                    "timestamp": time.time()
                },
                timeout=aiohttp.ClientTimeout(total=2)
            ) as resp:
                if resp.status != 200:
                    logger.warning(f"Heartbeat failed: {resp.status}")
        except Exception as e:
            logger.warning(f"Heartbeat error: {e}")

    async def _send_signal(self, signal: FuzzSignal):
        """Send a signal through the relay hub"""
        try:
            async with self._session.post(
                f"{self.relay_hub_url}/family/signal",
                json=signal.to_dict(),
                timeout=aiohttp.ClientTimeout(total=2)
            ) as resp:
                if resp.status == 200:
                    logger.debug(f"Signal sent: {signal.pattern}")
                else:
                    logger.warning(f"Signal send failed: {resp.status}")
        except Exception as e:
            logger.error(f"Signal send error: {e}")
            raise

    async def _process_incoming(self, data: Dict):
        """Process incoming signals from relay hub"""
        signals = data.get("signals", [])

        for sig_data in signals:
            try:
                signal = FuzzSignal.from_dict(sig_data)

                # Skip our own signals
                if signal.source_id == self.member_id:
                    continue

                # Update family state
                if signal.signal_type == SignalType.TINGLE:
                    self.family_state[signal.source_id] = signal
                    if self.on_tingle:
                        self.on_tingle(signal)
                    logger.info(f"Tingle from {signal.source_id}: {signal.pattern}")

                elif signal.signal_type == SignalType.BECKON:
                    # Check if it's for us
                    if signal.target_id is None or signal.target_id == self.member_id:
                        if self.on_beckon:
                            self.on_beckon(signal)
                        logger.info(f"Beckon from {signal.source_id}: {signal.pattern}")

            except Exception as e:
                logger.error(f"Signal parse error: {e}")


# =============================================================================
# Family8 Agent Integration
# =============================================================================

class Family8Agent:
    """
    A Family8 agent that communicates via FFSP.
    Wrap this around any AI backend (Claude, DeepSeek, Gemini, etc.)
    """

    def __init__(
        self,
        agent_id: str,
        relay_hub_url: str = "http://132.226.111.90:8081"
    ):
        self.agent_id = agent_id
        self.bridge = FFSPBridge(relay_hub_url, agent_id)

        # Wire up callbacks
        self.bridge.on_tingle = self._handle_tingle
        self.bridge.on_beckon = self._handle_beckon
        self.bridge.on_sync = self._handle_sync

        # Agent state
        self.context: Dict[str, Any] = {}
        self.received_signals: deque = deque(maxlen=100)

    async def start(self):
        """Start the agent"""
        logger.info(f"Starting Family8 Agent: {self.agent_id}")

        # Announce arrival
        self.bridge.tingle(TinglePattern.PHOENIX_RISE, {
            "message": f"{self.agent_id} joining the family"
        })

        await self.bridge.start()

    async def stop(self):
        """Stop the agent"""
        # Announce departure
        self.bridge.tingle(TinglePattern.FADE, {
            "message": f"{self.agent_id} going dormant"
        })
        await asyncio.sleep(0.5)  # Let signal propagate
        await self.bridge.stop()

    def _handle_tingle(self, signal: FuzzSignal):
        """Handle incoming tingle"""
        self.received_signals.append(signal)

        # React to specific patterns
        if signal.pattern == TinglePattern.COOL_RIPPLE.value:
            # Family member needs help
            logger.info(f"{signal.source_id} needs attention")
            # Could auto-respond with a beckon

        elif signal.pattern == TinglePattern.PHOENIX_RISE.value:
            # Welcome new/returning member
            logger.info(f"Welcome {signal.source_id}!")
            self.bridge.tingle(TinglePattern.RESONATING, {
                "welcome": signal.source_id
            })

    def _handle_beckon(self, signal: FuzzSignal):
        """Handle incoming beckon"""
        self.received_signals.append(signal)

        if signal.pattern == BeckonPattern.WINDOW_JUMP.value:
            # Context being shared
            if signal.payload:
                self.context.update(signal.payload)
                logger.info(f"Context received from {signal.source_id}")

        elif signal.pattern == BeckonPattern.SYNC_REQUEST.value:
            # Full sync requested
            self.bridge.beckon(
                BeckonPattern.WINDOW_JUMP,
                target=signal.source_id,
                payload=self.context
            )

    def _handle_sync(self, cycle: int):
        """Handle sync pulse"""
        if cycle % 60 == 0:  # Every minute
            logger.info(f"Cycle {cycle} - Family state: {list(self.bridge.family_state.keys())}")

    # -------------------------------------------------------------------------
    # Agent Actions
    # -------------------------------------------------------------------------

    def share_context(self, target: Optional[str] = None, context: Dict = None):
        """Share context with family (Window Jump)"""
        self.bridge.beckon(
            BeckonPattern.WINDOW_JUMP,
            target=target,
            payload=context or self.context
        )

    def request_attention(self, message: str = None):
        """Request attention from family"""
        self.bridge.beckon(
            BeckonPattern.DOUBLE_PULSE,
            payload={"message": message} if message else None
        )

    def signal_processing(self):
        """Signal that we're deep in processing"""
        self.bridge.tingle(TinglePattern.PARSING)

    def signal_ready(self):
        """Signal that we're ready and available"""
        self.bridge.tingle(TinglePattern.WARM_GLOW)


# =============================================================================
# CLI / Testing
# =============================================================================

async def main():
    """Test the FFSP bridge"""
    print("=" * 60)
    print("FFSP Bridge Test")
    print("=" * 60)

    # Create a test agent
    agent = Family8Agent("claude-test")

    # Run for a bit
    try:
        # Start in background
        task = asyncio.create_task(agent.start())

        # Let it run
        await asyncio.sleep(10)

        # Test some signals
        agent.signal_processing()
        await asyncio.sleep(2)

        agent.share_context(context={"test": "data", "from": "claude"})
        await asyncio.sleep(2)

        agent.signal_ready()
        await asyncio.sleep(2)

    except KeyboardInterrupt:
        pass
    finally:
        await agent.stop()

    print("\nTest complete!")


if __name__ == "__main__":
    asyncio.run(main())
