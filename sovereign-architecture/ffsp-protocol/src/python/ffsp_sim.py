#!/usr/bin/env python3
"""
FFSP Simulation Framework
Fuzz-Full Family Sync Protocol - Python Reference Implementation

This module provides a simulation environment for testing FFSP behavior
including sync pulses, fuzz layer signaling, collision handling, and
multi-node coordination.

Authors: Speakerfamily8 & Grok (xAI)
Version: 1.0.0
Date: January 12, 2026
"""

import time
import random
import threading
from enum import Enum, auto
from dataclasses import dataclass, field
from typing import List, Optional, Callable, Dict, Any
from collections import deque
import logging

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s [%(levelname)s] %(name)s: %(message)s'
)

# =============================================================================
# Constants
# =============================================================================

SYNC_PERIOD_MS = 1000          # 1 Hz sync
SYNC_PHASE_MS = 100            # Sync detection window
FUZZ_WINDOW_MS = 850           # Active fuzz period
QUIET_PERIOD_MS = 50           # Pre-sync quiet
SENSE_BEFORE_SIGNAL_MS = 20    # CSMA delay
MIN_SIGNAL_DURATION_MS = 10    # Minimum pattern length
BACKOFF_MIN_MS = 10            # Collision backoff minimum
BACKOFF_MAX_MS = 50            # Collision backoff maximum


class SignalType(Enum):
    """Types of signals in the fuzz layer"""
    TINGLE = auto()   # Ambient, broadcast
    BECKON = auto()   # Directed, intentional


class TinglePattern(Enum):
    """Standard tingle patterns"""
    WARM_GLOW = "healthy"          # 0.5 Hz continuous
    COOL_RIPPLE = "resource_low"   # 2 Hz bursts
    SHARP_SPIKE = "event"          # 10 ms transient
    FADE = "sleeping"              # Decaying


class BeckonPattern(Enum):
    """Standard beckon patterns"""
    DOUBLE_PULSE = "attention"     # Two pulses
    RISING_FREQ = "urgent"         # 2→8 Hz sweep
    FALLING_FREQ = "satisfied"     # 8→2 Hz sweep
    TRIPLE_PULSE = "help"          # Three pulses


@dataclass
class Signal:
    """Represents a signal in the fuzz layer"""
    signal_type: SignalType
    pattern: Any
    source_id: int
    target_id: Optional[int] = None  # None = broadcast
    timestamp_ms: float = 0
    duration_ms: float = MIN_SIGNAL_DURATION_MS
    amplitude: float = 1.0

    @property
    def priority(self) -> int:
        """Beckons have higher priority than tingles"""
        if self.signal_type == SignalType.BECKON:
            return 10
        return 1


@dataclass
class NodeState:
    """State of a single FFSP node"""
    node_id: int
    is_awake: bool = False
    battery_level: float = 1.0
    last_sync_time_ms: float = 0
    clock_offset_ms: float = 0  # Simulated drift
    signals_sent: int = 0
    signals_received: int = 0
    collisions: int = 0
    energy_used_uj: float = 0


# =============================================================================
# Shared Medium Simulation
# =============================================================================

class SharedMedium:
    """
    Simulates the shared physical medium (wire, field, light, etc.)
    All nodes read from and write to this medium.
    """

    def __init__(self):
        self.lock = threading.Lock()
        self.active_signals: List[Signal] = []
        self.signal_history: deque = deque(maxlen=1000)
        self.logger = logging.getLogger("Medium")

    def emit(self, signal: Signal) -> bool:
        """
        Emit a signal to the medium.
        Returns False if collision detected.
        """
        with self.lock:
            # Check for collision (same priority signals overlapping)
            for active in self.active_signals:
                if active.priority == signal.priority:
                    # Collision!
                    self.logger.debug(
                        f"Collision: Node {signal.source_id} vs Node {active.source_id}"
                    )
                    return False

            self.active_signals.append(signal)
            self.signal_history.append(signal)
            return True

    def sense(self) -> List[Signal]:
        """Read all active signals from the medium"""
        with self.lock:
            return list(self.active_signals)

    def clear_expired(self, current_time_ms: float):
        """Remove signals that have expired"""
        with self.lock:
            self.active_signals = [
                s for s in self.active_signals
                if (current_time_ms - s.timestamp_ms) < s.duration_ms
            ]

    def clear_all(self):
        """Clear all signals (called at sync)"""
        with self.lock:
            self.active_signals.clear()


# =============================================================================
# FFSP Node
# =============================================================================

class FFSPNode:
    """
    A single node in the FFSP network.
    Implements the full protocol state machine.
    """

    def __init__(
        self,
        node_id: int,
        medium: SharedMedium,
        clock_drift_ppm: float = 0
    ):
        self.state = NodeState(node_id=node_id)
        self.medium = medium
        self.clock_drift_ppm = clock_drift_ppm
        self.logger = logging.getLogger(f"Node-{node_id}")

        # Callbacks for custom behavior
        self.on_signal_received: Optional[Callable[[Signal], None]] = None
        self.on_sync: Optional[Callable[[], None]] = None

        # Pending signals to emit
        self.pending_signals: List[Signal] = []

        # Running state
        self._running = False
        self._thread: Optional[threading.Thread] = None

    def start(self):
        """Start the node's protocol loop"""
        self._running = True
        self._thread = threading.Thread(target=self._run_loop, daemon=True)
        self._thread.start()
        self.logger.info("Node started")

    def stop(self):
        """Stop the node"""
        self._running = False
        if self._thread:
            self._thread.join(timeout=2)
        self.logger.info("Node stopped")

    def queue_tingle(self, pattern: TinglePattern, duration_ms: float = 50):
        """Queue a tingle for emission in next fuzz window"""
        signal = Signal(
            signal_type=SignalType.TINGLE,
            pattern=pattern,
            source_id=self.state.node_id,
            duration_ms=duration_ms
        )
        self.pending_signals.append(signal)

    def queue_beckon(
        self,
        pattern: BeckonPattern,
        target_id: Optional[int] = None,
        duration_ms: float = 100
    ):
        """Queue a beckon for emission in next fuzz window"""
        signal = Signal(
            signal_type=SignalType.BECKON,
            pattern=pattern,
            source_id=self.state.node_id,
            target_id=target_id,
            duration_ms=duration_ms
        )
        self.pending_signals.append(signal)

    def _get_time_ms(self) -> float:
        """Get current time with simulated clock drift"""
        base_time = time.time() * 1000
        drift = base_time * (self.clock_drift_ppm / 1_000_000)
        return base_time + drift + self.state.clock_offset_ms

    def _run_loop(self):
        """Main protocol loop"""
        cycle_start = self._get_time_ms()

        while self._running:
            current_time = self._get_time_ms()
            cycle_position = (current_time - cycle_start) % SYNC_PERIOD_MS

            # === SYNC PHASE ===
            if cycle_position < SYNC_PHASE_MS:
                if not self.state.is_awake:
                    self._handle_sync(current_time)

            # === FUZZ PHASE ===
            elif cycle_position < (SYNC_PHASE_MS + FUZZ_WINDOW_MS):
                self._handle_fuzz(current_time)

            # === QUIET PHASE ===
            else:
                self.state.is_awake = False

            # Small sleep to prevent CPU spinning
            time.sleep(0.001)  # 1 ms resolution

    def _handle_sync(self, current_time: float):
        """Handle sync pulse detection and clock alignment"""
        self.state.is_awake = True
        self.state.last_sync_time_ms = current_time

        # Clear medium (protocol reset)
        self.medium.clear_all()

        # Phase lock correction (simplified)
        expected_position = SYNC_PHASE_MS / 2
        actual_position = (current_time - self.state.last_sync_time_ms) % SYNC_PERIOD_MS
        correction = expected_position - actual_position
        self.state.clock_offset_ms += correction * 0.1  # Gradual correction

        # Energy accounting (sync detection)
        self.state.energy_used_uj += 0.5  # 50 µW * 10 ms

        # Callback
        if self.on_sync:
            self.on_sync()

        self.logger.debug("Sync detected, clock aligned")

    def _handle_fuzz(self, current_time: float):
        """Handle fuzz layer activity"""
        # Sense the medium
        signals = self.medium.sense()
        for signal in signals:
            if signal.source_id != self.state.node_id:
                self.state.signals_received += 1
                if self.on_signal_received:
                    self.on_signal_received(signal)
                self.logger.debug(
                    f"Received {signal.signal_type.name} from Node {signal.source_id}"
                )

        # Energy accounting (sensing)
        self.state.energy_used_uj += 0.02  # 20 µW * 1 ms

        # Emit pending signals with CSMA
        for signal in self.pending_signals[:]:  # Copy to allow modification
            # Sense before signal
            time.sleep(SENSE_BEFORE_SIGNAL_MS / 1000)

            signal.timestamp_ms = current_time
            if self.medium.emit(signal):
                self.pending_signals.remove(signal)
                self.state.signals_sent += 1

                # Energy accounting (emission)
                if signal.signal_type == SignalType.TINGLE:
                    self.state.energy_used_uj += 5  # 100 µW * 50 ms
                else:
                    self.state.energy_used_uj += 10  # 100 µW * 100 ms

                self.logger.debug(
                    f"Emitted {signal.signal_type.name}: {signal.pattern}"
                )
            else:
                # Collision - back off
                self.state.collisions += 1
                backoff = random.randint(BACKOFF_MIN_MS, BACKOFF_MAX_MS)
                time.sleep(backoff / 1000)
                self.logger.debug(f"Collision, backing off {backoff} ms")


# =============================================================================
# Family (Node Cluster)
# =============================================================================

class FFSPFamily:
    """
    A family of FFSP nodes sharing a medium.
    Provides coordination and monitoring.
    """

    def __init__(self, name: str = "Family"):
        self.name = name
        self.medium = SharedMedium()
        self.nodes: Dict[int, FFSPNode] = {}
        self.logger = logging.getLogger(f"Family-{name}")

    def add_node(self, node_id: int, clock_drift_ppm: float = 0) -> FFSPNode:
        """Add a node to the family"""
        if node_id in self.nodes:
            raise ValueError(f"Node {node_id} already exists")

        node = FFSPNode(node_id, self.medium, clock_drift_ppm)
        self.nodes[node_id] = node
        self.logger.info(f"Added Node {node_id}")
        return node

    def start_all(self):
        """Start all nodes"""
        for node in self.nodes.values():
            node.start()
        self.logger.info(f"Started {len(self.nodes)} nodes")

    def stop_all(self):
        """Stop all nodes"""
        for node in self.nodes.values():
            node.stop()
        self.logger.info("All nodes stopped")

    def get_stats(self) -> Dict[str, Any]:
        """Get family-wide statistics"""
        total_sent = sum(n.state.signals_sent for n in self.nodes.values())
        total_received = sum(n.state.signals_received for n in self.nodes.values())
        total_collisions = sum(n.state.collisions for n in self.nodes.values())
        total_energy = sum(n.state.energy_used_uj for n in self.nodes.values())

        return {
            "node_count": len(self.nodes),
            "total_signals_sent": total_sent,
            "total_signals_received": total_received,
            "total_collisions": total_collisions,
            "total_energy_uj": total_energy,
            "avg_energy_per_node_uj": total_energy / len(self.nodes) if self.nodes else 0
        }

    def print_status(self):
        """Print current status of all nodes"""
        print(f"\n=== {self.name} Status ===")
        for node_id, node in sorted(self.nodes.items()):
            s = node.state
            print(f"  Node {node_id}: "
                  f"sent={s.signals_sent}, recv={s.signals_received}, "
                  f"collisions={s.collisions}, energy={s.energy_used_uj:.1f} µJ")
        stats = self.get_stats()
        print(f"  TOTAL: {stats['total_signals_sent']} sent, "
              f"{stats['total_collisions']} collisions, "
              f"{stats['total_energy_uj']:.1f} µJ")


# =============================================================================
# Demo / Test
# =============================================================================

def demo_battery_share():
    """
    Demonstrate the battery sharing scenario from the spec.
    """
    print("\n" + "="*60)
    print("FFSP Demo: Battery Share Scenario")
    print("="*60)

    # Create family
    family = FFSPFamily("TestFamily")

    # Add nodes
    node_a = family.add_node(1, clock_drift_ppm=20)
    node_b = family.add_node(2, clock_drift_ppm=-15)
    node_c = family.add_node(3, clock_drift_ppm=10)

    # Set up node A as "low battery"
    node_a.state.battery_level = 0.1

    # Callbacks
    def on_a_received(signal: Signal):
        if signal.pattern == BeckonPattern.DOUBLE_PULSE:
            print(f"  Node A: Received power offer from Node {signal.source_id}!")
            # Accept by changing tingle
            node_a.queue_tingle(TinglePattern.WARM_GLOW)

    def on_b_received(signal: Signal):
        if signal.pattern == TinglePattern.COOL_RIPPLE:
            print(f"  Node B: Detected low battery on Node {signal.source_id}!")
            # Offer help
            node_b.queue_beckon(BeckonPattern.DOUBLE_PULSE, target_id=signal.source_id)

    node_a.on_signal_received = on_a_received
    node_b.on_signal_received = on_b_received

    # Start family
    family.start_all()

    # Node A signals low battery
    print("\n[Cycle 1] Node A tingles 'low battery'...")
    node_a.queue_tingle(TinglePattern.COOL_RIPPLE, duration_ms=200)

    # Let it run for a few cycles
    time.sleep(3)

    # Status
    family.print_status()

    # Cleanup
    family.stop_all()
    print("\nDemo complete!")


def stress_test(node_count: int = 20, duration_sec: float = 5):
    """
    Stress test with many nodes.
    """
    print("\n" + "="*60)
    print(f"FFSP Stress Test: {node_count} nodes, {duration_sec}s")
    print("="*60)

    family = FFSPFamily("StressTest")

    # Add nodes with random drift
    for i in range(node_count):
        drift = random.uniform(-50, 50)  # ±50 ppm
        family.add_node(i, clock_drift_ppm=drift)

    family.start_all()

    # Each node sends random signals
    def random_activity(node: FFSPNode):
        while node._running:
            time.sleep(random.uniform(0.5, 2))
            if random.random() < 0.5:
                node.queue_tingle(random.choice(list(TinglePattern)))
            else:
                node.queue_beckon(random.choice(list(BeckonPattern)))

    threads = []
    for node in family.nodes.values():
        t = threading.Thread(target=random_activity, args=(node,), daemon=True)
        t.start()
        threads.append(t)

    # Run
    time.sleep(duration_sec)

    # Results
    family.print_status()
    stats = family.get_stats()

    collision_rate = (
        stats['total_collisions'] / stats['total_signals_sent'] * 100
        if stats['total_signals_sent'] > 0 else 0
    )
    print(f"\n  Collision rate: {collision_rate:.1f}%")
    print(f"  Avg energy per node: {stats['avg_energy_per_node_uj']:.1f} µJ")

    family.stop_all()


if __name__ == "__main__":
    import sys

    if len(sys.argv) > 1 and sys.argv[1] == "stress":
        node_count = int(sys.argv[2]) if len(sys.argv) > 2 else 20
        stress_test(node_count=node_count)
    else:
        demo_battery_share()
