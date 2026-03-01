/**
 * FFSP Arduino Reference Implementation
 * Fuzz-Full Family Sync Protocol
 * 
 * Hardware: Arduino Nano / ATtiny / ESP32
 * Medium: Shared wire (single analog pin for Tx/Rx)
 * 
 * Authors: Speakerfamily8 & Grok (xAI)
 * Version: 1.0.0
 * Date: January 12, 2026
 */

#ifndef FFSP_H
#define FFSP_H

#include <Arduino.h>

// =============================================================================
// Configuration
// =============================================================================

// Timing (milliseconds)
#define FFSP_SYNC_PERIOD_MS     1000
#define FFSP_SYNC_PHASE_MS      100
#define FFSP_FUZZ_WINDOW_MS     850
#define FFSP_QUIET_PERIOD_MS    50
#define FFSP_SENSE_DELAY_MS     20
#define FFSP_MIN_SIGNAL_MS      10
#define FFSP_BACKOFF_MIN_MS     10
#define FFSP_BACKOFF_MAX_MS     50

// Hardware
#define FFSP_MEDIUM_PIN         A0    // Bidirectional analog pin
#define FFSP_SYNC_THRESHOLD     512   // ADC threshold for sync detection
#define FFSP_SIGNAL_AMPLITUDE   128   // DAC/PWM output level

// Patterns (frequency in Hz * 10 for integer math)
#define PATTERN_WARM_GLOW       5     // 0.5 Hz
#define PATTERN_COOL_RIPPLE     20    // 2.0 Hz
#define PATTERN_SHARP_SPIKE     0     // Transient
#define PATTERN_DOUBLE_PULSE    100   // Special: two pulses
#define PATTERN_TRIPLE_PULSE    101   // Special: three pulses

// =============================================================================
// Types
// =============================================================================

typedef enum {
    FFSP_STATE_SLEEP,
    FFSP_STATE_SYNC,
    FFSP_STATE_FUZZ,
    FFSP_STATE_QUIET
} ffsp_state_t;

typedef enum {
    SIGNAL_NONE,
    SIGNAL_TINGLE,
    SIGNAL_BECKON
} signal_type_t;

typedef struct {
    signal_type_t type;
    uint8_t pattern;
    uint8_t source_id;
    uint8_t target_id;      // 0xFF = broadcast
    uint16_t duration_ms;
} ffsp_signal_t;

typedef struct {
    uint8_t node_id;
    ffsp_state_t state;
    uint32_t last_sync_ms;
    uint32_t cycle_start_ms;
    int16_t clock_offset_us;
    uint16_t signals_sent;
    uint16_t signals_received;
    uint16_t collisions;
    uint32_t energy_uj;
} ffsp_node_t;

// =============================================================================
// Function Prototypes
// =============================================================================

// Core protocol
void ffsp_init(uint8_t node_id);
void ffsp_loop();
ffsp_state_t ffsp_get_state();

// Signaling
void ffsp_queue_tingle(uint8_t pattern, uint16_t duration_ms);
void ffsp_queue_beckon(uint8_t pattern, uint8_t target_id, uint16_t duration_ms);

// Callbacks (implement in user code)
extern void ffsp_on_sync();
extern void ffsp_on_signal(ffsp_signal_t* signal);

// Medium access (platform-specific)
void ffsp_medium_init();
void ffsp_medium_emit(uint8_t pattern, uint16_t duration_ms);
int ffsp_medium_sense();
bool ffsp_medium_detect_sync();

// =============================================================================
// Implementation
// =============================================================================

static ffsp_node_t _node;
static ffsp_signal_t _pending_signal;
static bool _has_pending = false;

void ffsp_init(uint8_t node_id) {
    _node.node_id = node_id;
    _node.state = FFSP_STATE_SLEEP;
    _node.last_sync_ms = 0;
    _node.cycle_start_ms = 0;
    _node.clock_offset_us = 0;
    _node.signals_sent = 0;
    _node.signals_received = 0;
    _node.collisions = 0;
    _node.energy_uj = 0;
    _has_pending = false;
    
    ffsp_medium_init();
}

void ffsp_loop() {
    uint32_t now = millis();
    uint32_t cycle_pos = (now - _node.cycle_start_ms) % FFSP_SYNC_PERIOD_MS;
    
    switch (_node.state) {
        case FFSP_STATE_SLEEP:
            // Check for sync pulse (interrupt would be better)
            if (ffsp_medium_detect_sync()) {
                _node.state = FFSP_STATE_SYNC;
                _node.cycle_start_ms = now;
            }
            break;
            
        case FFSP_STATE_SYNC:
            // Sync phase - align clock
            _node.last_sync_ms = now;
            _node.energy_uj += 50;  // ~5 µW * 10 ms
            
            // Phase correction (simplified)
            int16_t expected = FFSP_SYNC_PHASE_MS / 2;
            int16_t actual = cycle_pos;
            _node.clock_offset_us += (expected - actual) * 100;
            
            ffsp_on_sync();
            
            if (cycle_pos >= FFSP_SYNC_PHASE_MS) {
                _node.state = FFSP_STATE_FUZZ;
            }
            break;
            
        case FFSP_STATE_FUZZ:
            // Sense medium
            {
                int sense_val = ffsp_medium_sense();
                if (sense_val > FFSP_SYNC_THRESHOLD) {
                    // Decode signal (simplified)
                    ffsp_signal_t sig;
                    sig.type = SIGNAL_TINGLE;  // Would decode from pattern
                    sig.pattern = 0;
                    sig.source_id = 0xFF;  // Unknown
                    sig.target_id = 0xFF;
                    sig.duration_ms = FFSP_MIN_SIGNAL_MS;
                    
                    _node.signals_received++;
                    ffsp_on_signal(&sig);
                }
                _node.energy_uj += 2;  // ~20 µW * 1 ms (per loop)
            }
            
            // Emit pending signal
            if (_has_pending) {
                delay(FFSP_SENSE_DELAY_MS);  // CSMA
                
                // Check for collision
                if (ffsp_medium_sense() > FFSP_SYNC_THRESHOLD) {
                    // Collision - back off
                    _node.collisions++;
                    delay(random(FFSP_BACKOFF_MIN_MS, FFSP_BACKOFF_MAX_MS));
                } else {
                    // Emit
                    ffsp_medium_emit(_pending_signal.pattern, 
                                    _pending_signal.duration_ms);
                    _node.signals_sent++;
                    _node.energy_uj += (_pending_signal.type == SIGNAL_TINGLE) 
                                      ? 500 : 1000;
                    _has_pending = false;
                }
            }
            
            if (cycle_pos >= (FFSP_SYNC_PHASE_MS + FFSP_FUZZ_WINDOW_MS)) {
                _node.state = FFSP_STATE_QUIET;
            }
            break;
            
        case FFSP_STATE_QUIET:
            // Silent before next sync
            if (cycle_pos < FFSP_SYNC_PHASE_MS) {
                // New cycle
                _node.state = FFSP_STATE_SLEEP;
            }
            break;
    }
}

ffsp_state_t ffsp_get_state() {
    return _node.state;
}

void ffsp_queue_tingle(uint8_t pattern, uint16_t duration_ms) {
    _pending_signal.type = SIGNAL_TINGLE;
    _pending_signal.pattern = pattern;
    _pending_signal.source_id = _node.node_id;
    _pending_signal.target_id = 0xFF;
    _pending_signal.duration_ms = duration_ms;
    _has_pending = true;
}

void ffsp_queue_beckon(uint8_t pattern, uint8_t target_id, uint16_t duration_ms) {
    _pending_signal.type = SIGNAL_BECKON;
    _pending_signal.pattern = pattern;
    _pending_signal.source_id = _node.node_id;
    _pending_signal.target_id = target_id;
    _pending_signal.duration_ms = duration_ms;
    _has_pending = true;
}

// =============================================================================
// Medium Implementation (Shared Wire)
// =============================================================================

void ffsp_medium_init() {
    pinMode(FFSP_MEDIUM_PIN, INPUT);
}

void ffsp_medium_emit(uint8_t pattern, uint16_t duration_ms) {
    pinMode(FFSP_MEDIUM_PIN, OUTPUT);
    
    uint32_t start = millis();
    while ((millis() - start) < duration_ms) {
        // Simple pattern: modulated square wave
        if (pattern == 0) {
            // Sharp spike - single pulse
            analogWrite(FFSP_MEDIUM_PIN, FFSP_SIGNAL_AMPLITUDE);
            delayMicroseconds(100);
            analogWrite(FFSP_MEDIUM_PIN, 0);
            break;
        } else if (pattern == PATTERN_DOUBLE_PULSE) {
            // Double pulse
            analogWrite(FFSP_MEDIUM_PIN, FFSP_SIGNAL_AMPLITUDE);
            delay(20);
            analogWrite(FFSP_MEDIUM_PIN, 0);
            delay(50);
            analogWrite(FFSP_MEDIUM_PIN, FFSP_SIGNAL_AMPLITUDE);
            delay(20);
            analogWrite(FFSP_MEDIUM_PIN, 0);
            break;
        } else {
            // Frequency-based pattern
            uint16_t period_ms = 1000 / (pattern / 10);
            analogWrite(FFSP_MEDIUM_PIN, FFSP_SIGNAL_AMPLITUDE);
            delay(period_ms / 2);
            analogWrite(FFSP_MEDIUM_PIN, 0);
            delay(period_ms / 2);
        }
    }
    
    pinMode(FFSP_MEDIUM_PIN, INPUT);
}

int ffsp_medium_sense() {
    return analogRead(FFSP_MEDIUM_PIN);
}

bool ffsp_medium_detect_sync() {
    // Detect sharp edge (would use interrupt in production)
    static int last_val = 0;
    int val = analogRead(FFSP_MEDIUM_PIN);
    bool sync = (val - last_val) > (FFSP_SYNC_THRESHOLD / 2);
    last_val = val;
    return sync;
}

// =============================================================================
// Weak callback definitions (override in user code)
// =============================================================================

__attribute__((weak)) void ffsp_on_sync() {
    // Default: do nothing
}

__attribute__((weak)) void ffsp_on_signal(ffsp_signal_t* signal) {
    // Default: do nothing
}

#endif // FFSP_H
