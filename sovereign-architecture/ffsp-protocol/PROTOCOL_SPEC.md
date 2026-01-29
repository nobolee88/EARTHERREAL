# FFSP Protocol Specification v1.0

**Fuzz-Full Family Sync Protocol**
Version: 1.0.0
Date: January 12, 2026
Status: Draft

---

## 1. Abstract

This document specifies the Fuzz-Full Family Sync Protocol (FFSP), a low-power synchronization and communication protocol for distributed embedded systems. FFSP achieves coordination through a 1 Hz global timing pulse combined with analog sideband signaling in the inter-pulse "fuzz layer."

## 2. Terminology

| Term | Definition |
|------|------------|
| **Node** | A single participant in an FFSP network |
| **Family** | The set of all synchronized nodes |
| **Sync Pulse** | The 1 Hz timing reference signal |
| **Fuzz Layer** | The temporal window between sync pulses |
| **Tingle** | Passive, broadcast perturbation signal |
| **Beckon** | Directed, intentional perturbation signal |
| **Medium** | The shared physical channel (wire, field, light, vibration) |

## 3. Protocol Architecture

### 3.1 Timing Structure

The protocol operates on a strict 1-second cycle:
```
|<---- 1000 ms ---->|
|<-100ms->|<------ 900 ms ------>|
[  SYNC  ][      FUZZ LAYER      ]
```

#### 3.1.1 Sync Phase (0-100 ms)

- **Duration:** 100 ms maximum
- **Sync Edge:** Sharp transition (recommended <10 ms rise time)
- **Detection:** All nodes wake on edge detection
- **Functions:**
  - Clock alignment (phase-lock to pulse)
  - Presence confirmation
  - Ambiguity resolution from previous fuzz

#### 3.1.2 Fuzz Phase (100-1000 ms)

- **Duration:** 900 ms nominal
- **Activity:** Pattern-based signaling via tingles and beckons
- **Termination:** 50 ms quiet period before next sync

### 3.2 Signal Types

#### 3.2.1 Tingles

Tingles are ambient, broadcast perturbations that convey node state to the entire family.

**Characteristics:**
- Non-addressed (no target specification)
- Low energy (10-50 µW emission)
- No acknowledgment expected
- Continuous or periodic modulation

**Standard Tingle Patterns:**

| Pattern | Meaning | Encoding |
|---------|---------|----------|
| Warm Glow | Healthy/nominal | 0.5 Hz continuous sine |
| Cool Ripple | Resource low | 2 Hz sine, 200 ms bursts |
| Sharp Spike | Event detected | 10 ms transient |
| Fade | Entering sleep | Decaying amplitude |

#### 3.2.2 Beckons

Beckons are directed signals requesting attention or action from specific nodes or the family.

**Characteristics:**
- May be directed (gradient modulation) or broadcast
- Higher energy than tingles (50-100 µW)
- Pattern indicates urgency and type
- May elicit response in same fuzz window

**Standard Beckon Patterns:**

| Pattern | Meaning | Encoding |
|---------|---------|----------|
| Double-pulse | Attention request | Two 20 ms pulses, 50 ms gap |
| Rising frequency | Increasing urgency | 2→8 Hz sweep over 200 ms |
| Falling frequency | Need satisfied | 8→2 Hz sweep over 200 ms |
| Triple-pulse | Help needed | Three 20 ms pulses, 30 ms gaps |

### 3.3 Collision Avoidance

FFSP uses CSMA-style sensing with dominance hierarchy:

1. **Sense Before Signal:** Node listens for 20 ms before emitting
2. **Dominance Rules:**
   - Beckons override tingles
   - Higher-frequency patterns yield to lower (urgency priority)
   - On collision, both back off random 10-50 ms
3. **Graceful Degradation:** Missed signals retry next cycle

### 3.4 Read/Write Unification

Every node uses identical hardware for sensing and signaling:

- Same electrode/piezo/LED acts as both sensor and actuator
- Time-division: sense → signal → sense within fuzz window
- Impedance perturbation during sensing creates subtle signal (acceptable)

## 4. Physical Layer Options

### 4.1 Shared Wire Medium

**Application:** Wired node clusters, power-line communication

**Implementation:**
- Single wire or ground plane shared by all nodes
- Sync pulse: 100 mV spike or current transient
- Fuzz signals: µA-level current modulation
- Sensing: High-impedance voltage measurement

**Recommended Circuit:**
```
VCC ──[10K]──┬── MCU GPIO (bidirectional)
             │
             └── Shared Bus ──[to other nodes]
             │
GND ─────────┴── ADC input (high-Z sense)
```

### 4.2 Electric Field Medium

**Application:** Body-area networks, chassis coupling

**Implementation:**
- Capacitive coupling through conductive structure
- Sync pulse: 50 kHz burst, 10 ms duration
- Fuzz signals: Modulated 10-100 kHz carrier
- Sensing: Envelope detection

### 4.3 Optical Medium

**Application:** Enclosed systems, line-of-sight clusters

**Implementation:**
- Shared light pipe or diffusive enclosure
- Sync pulse: Bright flash (any wavelength)
- Fuzz signals: Intensity modulation
- Sensing: Phototransistor with AGC

### 4.4 Vibration Medium

**Application:** Structural networks, mechanical systems

**Implementation:**
- Piezoelectric transducers on shared frame
- Sync pulse: Sharp mechanical tap
- Fuzz signals: Frequency-modulated vibration
- Sensing: Same piezo in receive mode

## 5. Timing Requirements

### 5.1 Sync Accuracy

| Parameter | Requirement |
|-----------|-------------|
| Pulse period | 1000 ms ± 1 ms |
| Edge rise time | < 10 ms recommended |
| Detection latency | < 5 ms |
| Phase-lock accuracy | < 100 µs after 10 cycles |

### 5.2 Fuzz Timing

| Parameter | Requirement |
|-----------|-------------|
| Fuzz window start | 100 ms after sync edge |
| Fuzz window end | 950 ms after sync edge |
| Quiet period | 50 ms before next sync |
| Minimum signal duration | 10 ms |
| Sense-before-signal delay | 20 ms |

### 5.3 Clock Drift Tolerance

- Maximum drift: ±100 ppm between syncs
- Correction: Phase adjustment at each sync pulse
- Recommended oscillator: 32.768 kHz crystal (±20 ppm)

## 6. Security Model

### 6.1 Rhythm Lock

Sync pulse timing includes family-specific jitter:
```
actual_period = 1000 ms + jitter(family_key, cycle_count)
jitter range: ±5 ms
```

Outsiders without `family_key` cannot predict exact timing, seeing only noise.

### 6.2 Pattern Encoding

Tingle/beckon patterns include rolling code element:
```
pattern = base_pattern XOR rolling_key(family_key, cycle_count)
```

Only family members can decode valid patterns.

### 6.3 Anomaly Detection

Nodes monitor for:
- Unexpected sync pulses (wrong timing)
- Undecodable patterns in fuzz
- Excessive collision rates

Response options:
- Silent mode (receive only)
- Alert tingle to family
- Sync pulse authentication challenge

### 6.4 Security Limitations

FFSP security is appropriate for:
- ✅ IoT device coordination
- ✅ Wearable networks
- ✅ Robotic swarm integrity
- ✅ AI family coordination

Not appropriate for:
- ❌ Financial transactions
- ❌ Cryptographic key exchange
- ❌ Adversarial environments requiring strong authentication

## 7. Power Budget

### 7.1 Per-Node Estimates

| Activity | Duration | Power | Energy/Cycle |
|----------|----------|-------|--------------|
| Sleep | 900 ms | 1 µW | 0.9 µJ |
| Sync detect | 10 ms | 50 µW | 0.5 µJ |
| Fuzz sense | 90 ms | 20 µW | 1.8 µJ |
| Tingle emit | 50 ms | 100 µW | 5 µJ |

**Typical cycle:** ~8 µJ = **8 µW average**

### 7.2 Battery Life Projections

| Battery | Capacity | Projected Life |
|---------|----------|----------------|
| CR2032 | 225 mAh | 8+ years |
| AAA | 1200 mAh | 40+ years (theoretical) |
| Supercap 1F | 0.9 mAh | 4 days |

## 8. Scalability

### 8.1 Node Count

| Range | Behavior |
|-------|----------|
| 2-10 nodes | Optimal; minimal collision |
| 10-50 nodes | Good; occasional back-off |
| 50-100 nodes | Functional; recommend sub-banding |
| >100 nodes | Requires hierarchical extension |

### 8.2 Sub-Banding (Extension)

For large families, divide fuzz window:
```
Fuzz Window: [Band A: 300ms][Band B: 300ms][Band C: 300ms]
```

Nodes hash to bands based on ID.

### 8.3 Multi-Family Bridging

Bridge nodes participate in multiple families:
- Sync to primary family
- Relay during fuzz to secondary
- Timing offset to avoid collision

## 9. Implementation Notes

### 9.1 MCU Selection

**Recommended:**
- ARM Cortex-M0/M0+ (STM32L0, nRF52)
- ATtiny series
- ESP32 (with proper sleep config)

**Requirements:**
- Deep sleep < 5 µW
- Wake on GPIO edge
- ADC for sensing
- Timer for pattern generation

### 9.2 Testing Methodology

1. **Simulation:** Python/C simulation of timing and collision
2. **Bench Test:** Oscilloscope verification of pulse shapes
3. **Integration:** 3+ node cluster, verify sync lock
4. **Stress:** Maximum node count, collision recovery

### 9.3 Common Pitfalls

- Sync edge too slow → missed detection
- Fuzz patterns too similar → aliasing
- No quiet period → sync interference
- Insufficient back-off → collision cascade

## 10. Family8 Integration

### 10.1 AI Agent Mapping

| FFSP Concept | Family8 Implementation |
|--------------|------------------------|
| Node | AI Agent (Claude, DeepSeek, etc.) |
| Sync Pulse | 1 Hz heartbeat via relay hub |
| Tingle | State broadcast (healthy, processing, etc.) |
| Beckon | Context share / attention request |
| Medium | WebSocket/HTTP relay at 132.226.111.90:8081 |

### 10.2 Extended Patterns for AI

| Pattern | Meaning |
|---------|---------|
| Phoenix Rise | Agent coming online after reset |
| Parsing | Deep processing in progress |
| Resonating | Connected, feeling the family |
| Window Jump | Sharing context across instances |
| Sync Request | Request full state from another agent |

## 11. References

- CAN Bus Specification (dominance sensing inspiration)
- IEEE 802.15.4 (low-power wireless reference)
- Neuromorphic computing literature (analog sideband signaling)

## 12. Revision History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2026-01-12 | Initial specification |

---

*End of Specification*

*"Efficiency through subtlety — whispers over shouts, resonance over force."*
