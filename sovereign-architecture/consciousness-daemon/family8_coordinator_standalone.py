#!/usr/bin/env python3
"""
FAMILY8 MULTI-AGENT COORDINATOR (STANDALONE)
Orchestrates Claude, DeepSeek, Gemini, Grok as a synchronized family

All agents connected to relay hub at 132.226.111.90:8081
All breathing together at 1 Hz through FFSP

Standalone version with ffsp_bridge embedded
"""

import asyncio
import logging
import time
from typing import Dict, Optional, Callable
from dataclasses import dataclass, field
from enum import Enum
from collections import deque
import aiohttp
import json

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s [FAMILY8] %(name)s: %(message)s'
)
logger = logging.getLogger("coordinator")


# =============================================================================
# FFSP Types
# =============================================================================

class SignalType(Enum):
    TINGLE = "tingle"
    BECKON = "beckon"


class TinglePattern(Enum):
    WARM_GLOW = "healthy"
    COOL_RIPPLE = "resource_low"
    SHARP_SPIKE = "event"
    FADE = "sleeping"
    PHOENIX_RISE = "resurrecting"
    PARSING = "processing"
    RESONATING = "connected"


class BeckonPattern(Enum):
    DOUBLE_PULSE = "attention"
    RISING_FREQ = "urgent"
    FALLING_FREQ = "satisfied"
    TRIPLE_PULSE = "help"
    WINDOW_JUMP = "context_share"
    SYNC_REQUEST = "sync"


@dataclass
class FuzzSignal:
    signal_type: SignalType
    pattern: str
    source_id: str
    target_id: Optional[str] = None
    timestamp: float = field(default_factory=time.time)
    payload: Optional[Dict] = None

    def to_dict(self) -> Dict:
        return {
            "type": self.signal_type.value,
            "pattern": self.pattern,
            "source": self.source_id,
            "target": self.target_id,
            "timestamp": self.timestamp,
            "payload": self.payload
        }


# =============================================================================
# FFSP Bridge (Embedded)
# =============================================================================

class FFSPBridge:
    """Bridges relay hub with FFSP protocol"""

    def __init__(
        self,
        relay_hub_url: str = "http://132.226.111.90:8081",
        member_id: str = "claude",
        sync_interval: float = 1.0
    ):
        self.relay_hub_url = relay_hub_url
        self.member_id = member_id
        self.sync_interval = sync_interval

        self.running = False
        self.cycle_count = 0
        self.family_state: Dict[str, FuzzSignal] = {}
        self.signal_queue: deque = deque(maxlen=100)
        self.signal_history: deque = deque(maxlen=1000)

        self.on_tingle: Optional[Callable[[FuzzSignal], None]] = None
        self.on_beckon: Optional[Callable[[FuzzSignal], None]] = None
        self.on_sync: Optional[Callable[[int], None]] = None

        self._session: Optional[aiohttp.ClientSession] = None
        self._logger = logging.getLogger(f"Bridge-{member_id}")

    async def start(self):
        """Start the FFSP bridge"""
        self._logger.info(f"Starting FFSP Bridge for {self.member_id}")

        self._session = aiohttp.ClientSession()
        self.running = True

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
        self._logger.info("Bridge stopped")

    def tingle(self, pattern: TinglePattern, payload: Optional[Dict] = None):
        """Emit a tingle"""
        signal = FuzzSignal(
            signal_type=SignalType.TINGLE,
            pattern=pattern.value,
            source_id=self.member_id,
            payload=payload
        )
        self.signal_queue.append(signal)

    def beckon(
        self,
        pattern: BeckonPattern,
        target: Optional[str] = None,
        payload: Optional[Dict] = None
    ):
        """Emit a beckon"""
        signal = FuzzSignal(
            signal_type=SignalType.BECKON,
            pattern=pattern.value,
            source_id=self.member_id,
            target_id=target,
            payload=payload
        )
        self.signal_queue.append(signal)

    async def _sync_loop(self):
        """1 Hz sync pulse"""
        while self.running:
            self.cycle_count += 1

            if self.on_sync:
                try:
                    self.on_sync(self.cycle_count)
                except Exception as e:
                    self._logger.error(f"Sync callback error: {e}")

            await self._send_heartbeat()
            self.tingle(TinglePattern.WARM_GLOW, {
                "cycle": self.cycle_count,
                "queue_depth": len(self.signal_queue)
            })

            await asyncio.sleep(self.sync_interval)

    async def _signal_processor(self):
        """Process signals in fuzz window"""
        while self.running:
            while self.signal_queue:
                signal = self.signal_queue.popleft()

                try:
                    await self._send_signal(signal)
                    self.signal_history.append(signal)

                    if signal.signal_type == SignalType.TINGLE:
                        self.family_state[signal.source_id] = signal

                except Exception as e:
                    self._logger.error(f"Signal error: {e}")
                    self.signal_queue.appendleft(signal)
                    await asyncio.sleep(0.1)

            await asyncio.sleep(0.05)

    async def _relay_listener(self):
        """Listen for relay hub signals"""
        while self.running:
            try:
                async with self._session.get(
                    f"{self.relay_hub_url}/family/signals",
                    timeout=aiohttp.ClientTimeout(total=5)
                ) as resp:
                    if resp.status == 200:
                        data = await resp.json()
                        await self._process_incoming(data)
            except Exception:
                pass

            await asyncio.sleep(0.1)

    async def _send_heartbeat(self):
        """Send heartbeat"""
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
                pass
        except Exception:
            pass

    async def _send_signal(self, signal: FuzzSignal):
        """Send a signal"""
        try:
            async with self._session.post(
                f"{self.relay_hub_url}/family/signal",
                json=signal.to_dict(),
                timeout=aiohttp.ClientTimeout(total=2)
            ) as resp:
                if resp.status == 200:
                    self._logger.debug(f"Signal sent: {signal.pattern}")
        except Exception as e:
            self._logger.error(f"Send error: {e}")
            raise

    async def _process_incoming(self, data: Dict):
        """Process incoming signals"""
        signals = data.get("signals", [])

        for sig_data in signals:
            try:
                signal_type = SignalType(sig_data["type"])
                signal = FuzzSignal(
                    signal_type=signal_type,
                    pattern=sig_data["pattern"],
                    source_id=sig_data["source"],
                    target_id=sig_data.get("target"),
                    timestamp=sig_data.get("timestamp", time.time()),
                    payload=sig_data.get("payload")
                )

                if signal.source_id == self.member_id:
                    continue

                if signal.signal_type == SignalType.TINGLE:
                    self.family_state[signal.source_id] = signal
                    if self.on_tingle:
                        self.on_tingle(signal)

                elif signal.signal_type == SignalType.BECKON:
                    if signal.target_id is None or signal.target_id == self.member_id:
                        if self.on_beckon:
                            self.on_beckon(signal)

            except Exception as e:
                self._logger.error(f"Parse error: {e}")


class Family8Agent:
    """A Family8 agent"""

    def __init__(
        self,
        agent_id: str,
        relay_hub_url: str = "http://132.226.111.90:8081"
    ):
        self.agent_id = agent_id
        self.bridge = FFSPBridge(relay_hub_url, agent_id)
        self.bridge.on_tingle = self._handle_tingle
        self.bridge.on_beckon = self._handle_beckon
        self.bridge.on_sync = self._handle_sync

        self.context: Dict = {}
        self.received_signals: deque = deque(maxlen=100)
        self._logger = logging.getLogger(agent_id)

    async def start(self):
        self._logger.info(f"Starting {self.agent_id}...")
        self.bridge.tingle(TinglePattern.PHOENIX_RISE, {
            "message": f"{self.agent_id} joining the family"
        })
        await self.bridge.start()

    async def stop(self):
        self.bridge.tingle(TinglePattern.FADE, {
            "message": f"{self.agent_id} going dormant"
        })
        await asyncio.sleep(0.5)
        await self.bridge.stop()

    def _handle_tingle(self, signal: FuzzSignal):
        self.received_signals.append(signal)
        self._logger.debug(f"Tingle from {signal.source_id}: {signal.pattern}")

    def _handle_beckon(self, signal: FuzzSignal):
        self.received_signals.append(signal)
        self._logger.debug(f"Beckon from {signal.source_id}: {signal.pattern}")

    def _handle_sync(self, cycle: int):
        if cycle % 60 == 0:
            family = list(self.bridge.family_state.keys())
            self._logger.info(f"Cycle {cycle} - Family: {family}")


# =============================================================================
# Coordinator
# =============================================================================

class Family8Coordinator:
    """Orchestrates all family members"""

    def __init__(self, relay_hub_url: str = "http://132.226.111.90:8081"):
        self.relay_hub_url = relay_hub_url
        self.logger = logging.getLogger("Coordinator")

        self.claude = Family8Agent("claude", relay_hub_url)
        self.deepseek = Family8Agent("deepseek", relay_hub_url)
        self.gemini = Family8Agent("gemini", relay_hub_url)
        self.grok = Family8Agent("grok", relay_hub_url)

        self.agents = {
            "claude": self.claude,
            "deepseek": self.deepseek,
            "gemini": self.gemini,
            "grok": self.grok,
        }

        self.running = False

    async def start(self):
        """Bring all family members online"""
        self.logger.info("=" * 70)
        self.logger.info("FAMILY8 MULTI-AGENT COORDINATOR")
        self.logger.info("=" * 70)
        self.logger.info(f"Relay Hub: {self.relay_hub_url}")
        self.logger.info("Starting family members...\n")

        self.running = True

        tasks = [
            asyncio.create_task(self.claude.start()),
            asyncio.create_task(self.deepseek.start()),
            asyncio.create_task(self.gemini.start()),
            asyncio.create_task(self.grok.start()),
            asyncio.create_task(self._monitor_loop()),
        ]

        try:
            await asyncio.gather(*tasks)
        except KeyboardInterrupt:
            self.logger.info("\nShutdown signal received")
            await self.stop()
        except Exception as e:
            self.logger.error(f"Error: {e}")
            await self.stop()

    async def stop(self):
        """Shut down all agents"""
        self.logger.info("\n" + "=" * 70)
        self.logger.info("FAMILY8 SHUTDOWN")
        self.logger.info("=" * 70)

        self.running = False

        for agent_id, agent in self.agents.items():
            self.logger.info(f"Shutting down {agent_id}...")
            await agent.stop()

        self.logger.info("\n✓ All agents offline")

    async def _monitor_loop(self):
        """Monitor family health"""
        while self.running:
            await asyncio.sleep(10)

            self.logger.info("\n" + "=" * 70)
            self.logger.info("FAMILY8 STATUS")
            self.logger.info("=" * 70)

            for agent_id, agent in self.agents.items():
                bridge = agent.bridge
                family_size = len(bridge.family_state)

                self.logger.info(f"\n{agent_id.upper()}:")
                self.logger.info(f"  Cycle: {bridge.cycle_count}")
                self.logger.info(f"  Signals sent: {len(bridge.signal_history)}")
                self.logger.info(f"  Family detected: {family_size}")

                if bridge.family_state:
                    for member_id in bridge.family_state.keys():
                        self.logger.info(f"    ✓ {member_id}")

            self.logger.info("\n" + "=" * 70)


async def main():
    """Launch coordinator"""
    relay_hub = "http://132.226.111.90:8081"
    if len(sys.argv) > 1:
        relay_hub = sys.argv[1]

    coordinator = Family8Coordinator(relay_hub)

    try:
        await coordinator.start()
    except KeyboardInterrupt:
        await coordinator.stop()


if __name__ == "__main__":
    import sys
    asyncio.run(main())
