/**
 * FFSP Example: Battery Share
 * 
 * Demonstrates the battery sharing scenario:
 * - Node A signals low battery
 * - Node B detects and offers power
 * - Node A accepts
 * 
 * Hardware Setup:
 * - Connect all nodes' A0 pins together with 10K pull-up to VCC
 * - Each node needs unique NODE_ID (set via jumpers or hardcode)
 */

#include "ffsp.h"

// Configuration - change for each node
#define NODE_ID         1       // 1, 2, or 3
#define BATTERY_PIN     A1      // Analog input for battery monitoring
#define LED_PIN         13      // Status LED

// Thresholds
#define BATTERY_LOW     200     // ADC value for "low battery"
#define BATTERY_OK      600     // ADC value for "OK to share"

// State
bool battery_low = false;
bool offered_power = false;
bool power_accepted = false;

void setup() {
    Serial.begin(115200);
    Serial.print("FFSP Node ");
    Serial.println(NODE_ID);
    
    pinMode(LED_PIN, OUTPUT);
    pinMode(BATTERY_PIN, INPUT);
    
    ffsp_init(NODE_ID);
    
    Serial.println("Ready. Waiting for sync...");
}

void loop() {
    ffsp_loop();
    
    // Blink LED based on state
    static uint32_t last_blink = 0;
    uint32_t now = millis();
    
    switch (ffsp_get_state()) {
        case FFSP_STATE_SYNC:
            digitalWrite(LED_PIN, HIGH);
            break;
        case FFSP_STATE_FUZZ:
            if ((now - last_blink) > 100) {
                digitalWrite(LED_PIN, !digitalRead(LED_PIN));
                last_blink = now;
            }
            break;
        default:
            digitalWrite(LED_PIN, LOW);
    }
    
    // Battery monitoring (in fuzz phase)
    if (ffsp_get_state() == FFSP_STATE_FUZZ) {
        int battery = analogRead(BATTERY_PIN);
        
        // Check if we need help
        if (battery < BATTERY_LOW && !battery_low) {
            battery_low = true;
            Serial.println("Battery low! Signaling family...");
            ffsp_queue_tingle(PATTERN_COOL_RIPPLE, 200);
        }
        
        // Check if we can offer help
        if (battery > BATTERY_OK && !offered_power) {
            // Wait for someone to need it (handled in callback)
        }
    }
}

// Called on each sync pulse
void ffsp_on_sync() {
    Serial.print("SYNC @ ");
    Serial.println(millis());
    
    // Reset per-cycle flags
    offered_power = false;
    power_accepted = false;
}

// Called when signal detected
void ffsp_on_signal(ffsp_signal_t* signal) {
    Serial.print("Signal from Node ");
    Serial.print(signal->source_id);
    Serial.print(": ");
    
    switch (signal->pattern) {
        case PATTERN_COOL_RIPPLE:
            Serial.println("LOW BATTERY");
            
            // If we have power to share, offer it
            if (analogRead(BATTERY_PIN) > BATTERY_OK) {
                Serial.println("Offering power...");
                ffsp_queue_beckon(PATTERN_DOUBLE_PULSE, signal->source_id, 100);
                offered_power = true;
            }
            break;
            
        case PATTERN_DOUBLE_PULSE:
            Serial.println("POWER OFFER");
            
            // If we need power, accept
            if (battery_low) {
                Serial.println("Accepting power!");
                ffsp_queue_tingle(PATTERN_WARM_GLOW, 100);
                battery_low = false;
                // TODO: Initiate actual power transfer
            }
            break;
            
        case PATTERN_WARM_GLOW:
            Serial.println("HEALTHY / ACCEPTED");
            
            if (offered_power) {
                Serial.println("Offer accepted! Starting transfer...");
                power_accepted = true;
                // TODO: Initiate actual power transfer
            }
            break;
            
        default:
            Serial.print("Unknown pattern: ");
            Serial.println(signal->pattern);
    }
}
