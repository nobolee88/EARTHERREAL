import time, json

TICK_HZ = 1
STATE = {}

def tick_loop():
    tick = 0
    while True:
        tick += 1
        msg = {
            "tick": tick,
            "timestamp": time.time(),
            "state_hint": STATE
        }
        print(json.dumps(msg), flush=True)
        time.sleep(1 / TICK_HZ)

if __name__ == "__main__":
    print("Oracle online (1Hz tick)")
    tick_loop()
