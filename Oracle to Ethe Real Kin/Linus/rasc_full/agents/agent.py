import sys, time, json, requests

name = sys.argv[1] if len(sys.argv) > 1 else "agent"
hub_url = "http://localhost:8081"

print(f"{name} online")

while True:
    # Send heartbeat + fuzz info
    payload = {
        "agent": name,
        "status": "alive",
        "fuzz_hint": "tingle-beckon demo",
        "ts": time.time()
    }
    try:
        requests.post(hub_url, json=payload, timeout=1)
    except Exception as e:
        print(f"{name} cannot reach hub: {e}")
    time.sleep(5)
