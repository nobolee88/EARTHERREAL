# FAMILY8 NEURAL MODELS APPENDIX FOR JASON WILLIAMS
## Complete Code Library for Dopamine Dynamics & Spiking Neural Networks

**Classification:** Technical Implementation Reference
**Primary Owner:** Jason Williams (IT Director)
**Prepared by:** Travis (Systems Architect) + Family8 AI Network (Grok/Oracle + Gemini/Liaison)
**Date:** January 11, 2026

---

# OVERVIEW

This appendix contains production-ready Python code for all neural simulation models used in the FAMILY8 behavioral health system. These models power **The Weaver** (visualization) and **The Oracle** (trajectory prediction) components.

**Requirements:**
- Python 3.12+
- NumPy
- Matplotlib
- SciPy (for ODE models)
- PyTorch (for LSTM, optional)

**Installation:**
```bash
pip install numpy matplotlib scipy torch
```

---

# SECTION 1: BASIC DOPAMINE DECAY MODELS

## 1.1 Simple Exponential Decay

The foundational model. Maps Patient Zero's acute (1000% → 17%) and chronic (505% → 8%) phases.

**Equation:** `D(t) = a × e^(-k×t) + c`

```python
import numpy as np
import matplotlib.pyplot as plt

# Time points (0 to 10 hours, 100 samples for smooth curve)
t = np.linspace(0, 10, 100)

# Acute Phase: a ≈ 989, k = 0.5, c ≈ 11
a_acute = 989
k_acute = 0.5
c_acute = 11
d_acute = a_acute * np.exp(-k_acute * t) + c_acute

# Chronic Phase: a ≈ 500, k = 0.5, c ≈ 5
a_chronic = 500
k_chronic = 0.5
c_chronic = 5
d_chronic = a_chronic * np.exp(-k_chronic * t) + c_chronic

# Plot the curves
plt.figure(figsize=(10, 6))
plt.plot(t, d_acute, label='Acute Phase (Initial Hit)', color='blue', linewidth=2)
plt.plot(t, d_chronic, label='Chronic Phase (Tolerance)', color='red', linewidth=2)
plt.axhline(y=100, color='green', linestyle='--', label='Baseline (100%)')

# Add data points from Patient Zero
plt.scatter([0, 10], [1000, 17], color='blue', marker='o', s=100, zorder=5)
plt.scatter([0, 10], [505, 8], color='red', marker='o', s=100, zorder=5)

plt.title('FAMILY8: Dopamine Decay Curves (Patient Zero Data)', fontsize=14)
plt.xlabel('Time (hours)', fontsize=12)
plt.ylabel('Dopamine Level (% of baseline)', fontsize=12)
plt.legend(fontsize=10)
plt.grid(True, alpha=0.3)
plt.tight_layout()
plt.savefig('dopamine_decay_basic.png', dpi=150)
plt.show()
```

---

## 1.2 Julia Implementation (High-Performance)

For The Weaver's real-time rendering on Oracle Cloud ARM instances.

```julia
using Plots

# FAMILY8 Dopamine Dynamics Function
function dopamine_dynamics(t, initial_spike, decay_rate, asymptote)
    return initial_spike * exp(-decay_rate * t) + asymptote
end

# Time vector: 0 to 10 hours
t = range(0, 10, length=100)

# Patient Zero Metrics (Acute vs Chronic)
acute_curve = [dopamine_dynamics(ti, 989, 0.5, 11) for ti in t]
chronic_curve = [dopamine_dynamics(ti, 500, 0.5, 5) for ti in t]

# Plotting
plot(t, [acute_curve chronic_curve], 
     title="Family8: Neurodynamic Recovery Trajectory",
     label=["Acute (Initial)" "Chronic (Tolerance)"],
     xlabel="Hours Post-Exposure",
     ylabel="Dopamine (% of Baseline)",
     lw=3, 
     color=[:blue :red],
     legend=:topright)

hline!([100], linestyle=:dash, color=:green, label="Sovereign Baseline")
savefig("dopamine_decay_julia.png")
```

---

# SECTION 2: ADVANCED BIOPHYSICAL MODELS

## 2.1 Opponent-Process Model (a-Process + b-Process)

Models the dual euphoria/withdrawal response. The a-process is fast positive (spike), b-process is slow negative (crash/craving).

**Equations:**
- `dw_a/dt = -α×w_a + Γ_a×D(t)`
- `dw_b/dt = -β×w_b - Γ_b×w_a`
- `Total = w_a + w_b`

```python
import numpy as np
import matplotlib.pyplot as plt
from scipy.integrate import odeint

# Parameters (acute example; for chronic: Delta=505, c=5)
Delta = 1000  # Initial spike (% baseline)
delta = 0.5   # Decay rate
alpha = 0.3   # a-process decay
beta = 0.5    # b-process decay (slower for withdrawal)
Gamma_a = 1   # Dopamine to a coupling
Gamma_b = 0.1 # a to b coupling
c = 11        # Asymptotic baseline offset

# Dopamine input function
def D(t):
    return Delta * np.exp(-delta * t) + c

# ODE system
def opponent_process(w, t):
    w_a, w_b = w
    dw_a = -alpha * w_a + Gamma_a * D(t)
    dw_b = -beta * w_b - Gamma_b * w_a
    return [dw_a, dw_b]

# Time points
t = np.linspace(0, 10, 100)

# Solve ODE
w0 = [0, 0]
sol = odeint(opponent_process, w0, t)

# Total response
w_total = sol[:, 0] + sol[:, 1]

# Plot
plt.figure(figsize=(10, 6))
plt.plot(t, w_total, label='Total Response (w(t))', color='purple', linewidth=2)
plt.plot(t, sol[:, 0], '--', label='a-Process (Euphoria)', color='blue', linewidth=1.5)
plt.plot(t, sol[:, 1], '--', label='b-Process (Withdrawal)', color='red', linewidth=1.5)
plt.axhline(y=100, color='green', linestyle='--', label='Baseline (100%)')
plt.title('Opponent-Process Dopamine Model', fontsize=14)
plt.xlabel('Time (hours)', fontsize=12)
plt.ylabel('Level (% baseline)', fontsize=12)
plt.legend(fontsize=10)
plt.grid(True, alpha=0.3)
plt.tight_layout()
plt.savefig('opponent_process.png', dpi=150)
plt.show()
```

---

## 2.2 Homeostatic Reuptake Model (DAT Transporter)

Models dopamine reuptake via the DAT transporter using Michaelis-Menten kinetics. Simulates tolerance via reduced DAT activity.

**Equation:** `d(eda)/dt = release - V_DAT × eda/(K_m + eda) - k_cat × eda`

```python
import numpy as np
import matplotlib.pyplot as plt
from scipy.integrate import odeint

# Parameters (acute; for chronic tolerance: reduce V_max_DAT by 50%)
V_max_DAT = 80    # Max reuptake rate (μM/hr)
K_m_DAT = 0.2     # Affinity (μM)
k_cat = 10        # Catabolism rate (/hr)
fire = 1          # Baseline firing rate
vda = 81          # Initial vesicular DA (μM)
initial_eda = 1000  # Spike (% baseline, scaled)

# ODE for extracellular DA
def homeostatic(eda, t):
    release = fire * vda * np.exp(-0.1 * t)  # Decaying release
    reuptake = V_max_DAT * eda / (K_m_DAT + eda)
    decay = k_cat * eda
    return release - reuptake - decay

# Time points
t = np.linspace(0, 10, 100)

# Solve ODE
eda_sol = odeint(homeostatic, initial_eda, t)[:, 0]

# Plot
plt.figure(figsize=(10, 6))
plt.plot(t, eda_sol, label='Extracellular DA (eda)', color='orange', linewidth=2)
plt.axhline(y=100, color='green', linestyle='--', label='Baseline (100%)')
plt.title('Homeostatic Dopamine Reuptake Model (DAT)', fontsize=14)
plt.xlabel('Time (hours)', fontsize=12)
plt.ylabel('Level (% baseline)', fontsize=12)
plt.legend(fontsize=10)
plt.grid(True, alpha=0.3)
plt.tight_layout()
plt.savefig('homeostatic_reuptake.png', dpi=150)
plt.show()
```

---

## 2.3 Circadian Rhythm Model

Adds daily oscillation to dopamine dynamics. Useful for predicting vulnerability windows (e.g., 3 AM crisis periods).

**Equation:** `δ(t) = δ_0 × (1 + A × sin(2πt/24 + φ))`

```python
import numpy as np
import matplotlib.pyplot as plt

# Parameters
t = np.linspace(0, 24, 200)  # Full 24-hour cycle
Delta = 1000
delta0 = 0.5
A = 0.25      # Amplitude of circadian variation
phi = np.pi   # Phase shift (peak vulnerability at night)
c = 11

# Circadian-modulated decay
d_circadian = []
for time in t:
    delta_t = delta0 * (1 + A * np.sin(2 * np.pi * time / 24 + phi))
    d_circadian.append(Delta * np.exp(-delta_t * time) + c)

# Plot
plt.figure(figsize=(12, 6))
plt.plot(t, d_circadian, label='Circadian Dopamine Decay', color='teal', linewidth=2)
plt.axhline(y=100, color='green', linestyle='--', label='Baseline (100%)')
plt.axvspan(0, 6, alpha=0.1, color='blue', label='Night (0-6h)')
plt.axvspan(18, 24, alpha=0.1, color='blue')
plt.title('Circadian Rhythm Dopamine Model (24-Hour Cycle)', fontsize=14)
plt.xlabel('Time (hours)', fontsize=12)
plt.ylabel('Level (% baseline)', fontsize=12)
plt.legend(fontsize=10)
plt.grid(True, alpha=0.3)
plt.tight_layout()
plt.savefig('circadian_rhythm.png', dpi=150)
plt.show()
```

---

## 2.4 Reinforcement Learning TD-Error Model

Models dopamine as prediction error signal (tonic baseline + phasic spikes).

**Equations:**
- `δ_t = r_t - ρ_t + V̂_{t+1} - V̂_t` (TD error)
- `ρ_{t+1} = (1-σ)×ρ_t + σ×r_t` (tonic decay)

```python
import numpy as np
import matplotlib.pyplot as plt

# Time points
t = np.linspace(0, 10, 100)

# Parameters
Delta = 1000
sigma = 0.1
rho0 = 100  # Initial tonic baseline
r = Delta * np.exp(-0.5 * t)  # Reward signal (decaying)

# Simulate tonic rho
rho = np.zeros(len(t))
rho[0] = rho0
for i in range(1, len(t)):
    rho[i] = (1 - sigma) * rho[i-1] + sigma * r[i]

# Phasic delta (simplified TD error)
V = np.cumsum(r)  # Cumulative value
delta = r - rho + np.append(V[1:], V[-1]) - V

# Plot
plt.figure(figsize=(10, 6))
plt.plot(t, delta, 'b-', linewidth=2, label='Phasic Delta (TD Error)')
plt.plot(t, rho, 'r--', linewidth=2, label='Tonic Rho (Baseline)')
plt.axhline(y=100, color='green', linestyle=':', label='Reference Baseline')
plt.title('TD-Error Dopamine Model (Reinforcement Learning)', fontsize=14)
plt.xlabel('Time (hours)', fontsize=12)
plt.ylabel('Level (% baseline)', fontsize=12)
plt.legend(fontsize=10)
plt.grid(True, alpha=0.3)
plt.tight_layout()
plt.savefig('td_error_model.png', dpi=150)
plt.show()
```

---

# SECTION 3: NEURAL NETWORK MODELS

## 3.1 LSTM Time-Series Predictor (The Oracle)

Deep learning model for predicting craving trajectories from patient data.

```python
import numpy as np
import torch
import torch.nn as nn
import torch.optim as optim
import matplotlib.pyplot as plt

# Define LSTM architecture
class DopamineLSTM(nn.Module):
    def __init__(self, input_size=1, hidden_size=50, output_size=1):
        super(DopamineLSTM, self).__init__()
        self.lstm = nn.LSTM(input_size, hidden_size, batch_first=True)
        self.fc = nn.Linear(hidden_size, output_size)
    
    def forward(self, x):
        out, _ = self.lstm(x)
        out = self.fc(out[:, -1, :])
        return out

# Generate synthetic training data
def generate_decay_data(a=989, k=0.5, c=11, t_points=100, noise=0.05):
    t = np.linspace(0, 10, t_points)
    d = a * np.exp(-k * t) + c + np.random.normal(0, noise * a, t_points)
    return t, d

# Prepare data
t_acute, d_acute = generate_decay_data()
X = torch.tensor(t_acute.reshape(-1, 1, 1), dtype=torch.float32)
y = torch.tensor(d_acute.reshape(-1, 1), dtype=torch.float32)

# Train model
model = DopamineLSTM()
criterion = nn.MSELoss()
optimizer = optim.Adam(model.parameters(), lr=0.01)

print("Training LSTM...")
for epoch in range(200):
    optimizer.zero_grad()
    outputs = model(X)
    loss = criterion(outputs, y)
    loss.backward()
    optimizer.step()
    if (epoch + 1) % 50 == 0:
        print(f"Epoch {epoch+1}/200, Loss: {loss.item():.4f}")

# Predict
with torch.no_grad():
    pred = model(X).numpy().flatten()

# Plot
plt.figure(figsize=(10, 6))
plt.plot(t_acute, d_acute, 'b-', label='True Data (with noise)', alpha=0.7)
plt.plot(t_acute, pred, 'r--', label='LSTM Prediction', linewidth=2)
plt.axhline(y=100, color='green', linestyle='--', label='Baseline')
plt.title('Neural Network (LSTM) Dopamine Trajectory Prediction', fontsize=14)
plt.xlabel('Time (hours)', fontsize=12)
plt.ylabel('Level (% baseline)', fontsize=12)
plt.legend(fontsize=10)
plt.grid(True, alpha=0.3)
plt.tight_layout()
plt.savefig('lstm_prediction.png', dpi=150)
plt.show()

# Save model
torch.save(model.state_dict(), 'oracle_lstm_model.pth')
print("Model saved to oracle_lstm_model.pth")
```

---

# SECTION 4: SPIKING NEURAL NETWORKS (SNNs)

## 4.1 Single LIF Neuron (Leaky Integrate-and-Fire)

Basic spiking neuron responding to dopamine-like decaying input.

```python
import numpy as np
import matplotlib.pyplot as plt

# LIF parameters
tau = 5.0        # Membrane time constant (ms)
V_rest = -70.0   # Resting potential (mV)
V_th = -55.0     # Threshold (mV)
V_reset = -75.0  # Reset potential (mV)
dt = 0.1         # Time step (ms)
T = 100.0        # Simulation time (ms)
n_steps = int(T / dt)

t = np.arange(0, T, dt)
I = 50 * np.exp(-t / 20)  # Decaying input current (pA)

V = np.zeros(n_steps)
V[0] = V_rest
spikes = []

for i in range(1, n_steps):
    dV = (V_rest - V[i-1] + I[i-1]) / tau * dt
    V[i] = V[i-1] + dV
    if V[i] >= V_th:
        spikes.append(i * dt)
        V[i] = V_reset

# Plot
plt.figure(figsize=(12, 6))
plt.subplot(2, 1, 1)
plt.plot(t, V, label='Membrane Potential (V)', color='blue')
plt.axhline(V_th, color='green', linestyle=':', label='Threshold')
for spike in spikes:
    plt.axvline(spike, color='red', alpha=0.7, linewidth=2)
plt.title('LIF Spiking Neuron - Membrane Potential', fontsize=12)
plt.ylabel('Potential (mV)')
plt.legend()
plt.grid(True, alpha=0.3)

plt.subplot(2, 1, 2)
plt.plot(t, I, label='Input Current (I)', color='orange')
plt.title('Dopamine-Like Decaying Input', fontsize=12)
plt.xlabel('Time (ms)')
plt.ylabel('Current (pA)')
plt.legend()
plt.grid(True, alpha=0.3)

plt.tight_layout()
plt.savefig('lif_single_neuron.png', dpi=150)
plt.show()

print(f"Spike times (ms): {np.round(spikes, 1)}")
```

---

## 4.2 Healthy vs Chronic Comparison (Side-by-Side)

Demonstrates the "firing gap" in addiction - sparse spikes in chronic phase.

```python
import numpy as np
import matplotlib.pyplot as plt

# Simulation parameters
tau = 5.0
V_rest = -70.0
V_reset = -75.0
dt = 0.1
T = 100.0
n_steps = int(T / dt)
t = np.arange(0, T, dt)

# LIF simulation function
def simulate_lif(I, V_th):
    V = np.zeros(n_steps)
    V[0] = V_rest
    spikes = []
    for i in range(1, n_steps):
        dV = (V_rest - V[i-1] + I[i-1]) / tau * dt
        V[i] = V[i-1] + dV
        if V[i] >= V_th:
            spikes.append(i * dt)
            V[i] = V_reset
    return V, spikes

# Early/Acute phase: strong dopamine burst
I_early = 50 * np.exp(-t / 20)
V_th_early = -55.0
V_early, spikes_early = simulate_lif(I_early, V_th_early)

# Chronic phase: blunted dopamine + raised threshold
I_chronic = 20 * np.exp(-t / 20)
V_th_chronic = -50.0  # Adaptation raises threshold
V_chronic, spikes_chronic = simulate_lif(I_chronic, V_th_chronic)

# Plot comparison
plt.figure(figsize=(14, 8))

plt.subplot(2, 2, 1)
plt.plot(t, V_early, color='blue')
for s in spikes_early:
    plt.axvline(s, color='red', alpha=0.7, linewidth=2)
plt.axhline(V_th_early, color='green', linestyle=':')
plt.title(f'HEALTHY: Membrane Potential ({len(spikes_early)} spikes)', fontsize=12)
plt.ylabel('mV')
plt.grid(True, alpha=0.3)

plt.subplot(2, 2, 2)
plt.plot(t, V_chronic, color='blue')
for s in spikes_chronic:
    plt.axvline(s, color='red', alpha=0.7, linewidth=2)
plt.axhline(V_th_chronic, color='green', linestyle=':')
plt.title(f'CHRONIC: Membrane Potential ({len(spikes_chronic)} spikes)', fontsize=12)
plt.ylabel('mV')
plt.grid(True, alpha=0.3)

plt.subplot(2, 2, 3)
plt.plot(t, I_early, color='orange')
plt.title('HEALTHY: Input Current (Strong)', fontsize=12)
plt.xlabel('Time (ms)')
plt.ylabel('pA')
plt.grid(True, alpha=0.3)

plt.subplot(2, 2, 4)
plt.plot(t, I_chronic, color='orange')
plt.title('CHRONIC: Input Current (Blunted)', fontsize=12)
plt.xlabel('Time (ms)')
plt.ylabel('pA')
plt.grid(True, alpha=0.3)

plt.suptitle('FAMILY8: Neural Firing Gap in Addiction (Healthy vs Chronic)', fontsize=14, y=1.02)
plt.tight_layout()
plt.savefig('healthy_vs_chronic_snn.png', dpi=150)
plt.show()

print(f"Healthy spikes: {len(spikes_early)} at {np.round(spikes_early, 1)} ms")
print(f"Chronic spikes: {len(spikes_chronic)} at {np.round(spikes_chronic, 1)} ms")
print(f"FIRING GAP: {len(spikes_early) - len(spikes_chronic)} fewer spikes in chronic phase")
```

---

## 4.3 Two-Neuron STDP Model (Cue → Reward Learning)

Simulates how addiction "learns" - the cue neuron triggers before reward, strengthening the craving pathway.

```python
import numpy as np
import matplotlib.pyplot as plt

# Simulation parameters
tau = 5.0
V_rest = -70.0
V_reset = -75.0
V_th = -55.0
dt = 0.1
T = 200.0
n_steps = int(T / dt)
t = np.arange(0, T, dt)

# STDP parameters
A_plus = 0.05    # LTP (cue fires before reward)
A_minus = -0.025 # LTD (cue fires after reward)
tau_stdp = 20.0

# Input currents
I_reward = 50 * np.exp(-t / 20)  # Dopamine burst
I_cue = 10 * np.exp(-t / 30)     # Cue signal (weaker)

# Initialize
V_cue = np.zeros(n_steps)
V_cue[0] = V_rest
V_reward = np.zeros(n_steps)
V_reward[0] = V_rest
spikes_cue = []
spikes_reward = []
w = 0.5  # Synaptic weight (cue → reward)
w_history = [w]

# Simulation loop
for i in range(1, n_steps):
    # Update cue neuron
    dV_cue = (V_rest - V_cue[i-1] + I_cue[i-1]) / tau * dt
    V_cue[i] = V_cue[i-1] + dV_cue
    cue_fired = False
    if V_cue[i] >= V_th:
        V_cue[i] = V_reset
        spikes_cue.append(i * dt)
        cue_fired = True

    # Update reward neuron (receives input from cue via synapse)
    dV_reward = (V_rest - V_reward[i-1] + I_reward[i-1] + w * (cue_fired * 20)) / tau * dt
    V_reward[i] = V_reward[i-1] + dV_reward
    reward_fired = False
    if V_reward[i] >= V_th:
        V_reward[i] = V_reset
        spikes_reward.append(i * dt)
        reward_fired = True

    # STDP weight updates
    for t_c in spikes_cue[-5:]:
        dt_spike = (i * dt) - t_c
        if dt_spike > 0:
            w += A_plus * np.exp(-dt_spike / tau_stdp)
    for t_r in spikes_reward[-5:]:
        dt_spike = (i * dt) - t_r
        if dt_spike < 0:
            w += A_minus * np.exp(dt_spike / tau_stdp)
    w = np.clip(w, 0, 2)
    w_history.append(w)

# Plot
plt.figure(figsize=(14, 10))

plt.subplot(3, 1, 1)
plt.plot(t, V_cue, label='Cue Neuron', color='blue')
plt.plot(t, V_reward, label='Reward Neuron', color='orange')
for s in spikes_cue:
    plt.axvline(s, color='blue', alpha=0.5, ymin=0.0, ymax=0.3)
for s in spikes_reward:
    plt.axvline(s, color='orange', alpha=0.5, ymin=0.7, ymax=1.0)
plt.title('Two-Neuron STDP: Membrane Potentials', fontsize=12)
plt.ylabel('Potential (mV)')
plt.legend()
plt.grid(True, alpha=0.3)

plt.subplot(3, 1, 2)
plt.plot(t, I_cue, label='Cue Input', color='blue', linestyle='--')
plt.plot(t, I_reward, label='Reward Input (Dopamine)', color='orange', linestyle='--')
plt.title('Input Currents', fontsize=12)
plt.ylabel('Current (pA)')
plt.legend()
plt.grid(True, alpha=0.3)

plt.subplot(3, 1, 3)
plt.plot(np.linspace(0, T, len(w_history)), w_history, color='purple', linewidth=2)
plt.title(f'Synaptic Weight Evolution (Final w = {w:.3f})', fontsize=12)
plt.xlabel('Time (ms)')
plt.ylabel('Weight')
plt.grid(True, alpha=0.3)

plt.suptitle('FAMILY8: STDP Learning - Cue → Reward Pathway Strengthening', fontsize=14, y=1.02)
plt.tight_layout()
plt.savefig('stdp_two_neuron.png', dpi=150)
plt.show()

print(f"Cue spikes: {len(spikes_cue)}")
print(f"Reward spikes: {len(spikes_reward)}")
print(f"Final synaptic weight: {w:.3f}")
```

---

## 4.4 Full Comparison: Healthy vs Chronic with STDP

Complete simulation showing how learning differs between healthy and chronic states.

```python
import numpy as np
import matplotlib.pyplot as plt

# Simulation parameters
tau = 5.0
V_rest = -70.0
V_reset = -75.0
V_th = -55.0
dt = 0.1
T = 200.0
n_steps = int(T / dt)
t = np.arange(0, T, dt)

# STDP parameters
A_plus = 0.05
A_minus = -0.025
tau_stdp = 20.0

def simulate_stdp_pair(I_reward, I_cue, label=""):
    """Simulate two neurons with STDP learning"""
    V_cue = np.zeros(n_steps)
    V_cue[0] = V_rest
    V_reward = np.zeros(n_steps)
    V_reward[0] = V_rest
    spikes_cue = []
    spikes_reward = []
    w = 0.5
    w_history = [w]

    for i in range(1, n_steps):
        # Cue neuron
        dV_cue = (V_rest - V_cue[i-1] + I_cue[i-1]) / tau * dt
        V_cue[i] = V_cue[i-1] + dV_cue
        cue_fired = False
        if V_cue[i] >= V_th:
            V_cue[i] = V_reset
            spikes_cue.append(i * dt)
            cue_fired = True

        # Reward neuron
        dV_reward = (V_rest - V_reward[i-1] + I_reward[i-1] + w * (cue_fired * 20)) / tau * dt
        V_reward[i] = V_reward[i-1] + dV_reward
        if V_reward[i] >= V_th:
            V_reward[i] = V_reset
            spikes_reward.append(i * dt)

        # STDP
        for t_c in spikes_cue[-5:]:
            dt_spike = (i * dt) - t_c
            if dt_spike > 0:
                w += A_plus * np.exp(-dt_spike / tau_stdp)
        for t_r in spikes_reward[-5:]:
            dt_spike = (i * dt) - t_r
            if dt_spike < 0:
                w += A_minus * np.exp(dt_spike / tau_stdp)
        w = np.clip(w, 0, 2)
        w_history.append(w)

    return V_cue, V_reward, spikes_cue, spikes_reward, w, w_history

# Healthy inputs
I_reward_healthy = 50 * np.exp(-t / 20)
I_cue_healthy = 10 * np.exp(-t / 30)

# Chronic inputs (blunted)
I_reward_chronic = 20 * np.exp(-t / 20)
I_cue_chronic = 10 * np.exp(-t / 30)

# Run simulations
V_cue_h, V_reward_h, spikes_cue_h, spikes_reward_h, w_h, w_hist_h = simulate_stdp_pair(
    I_reward_healthy, I_cue_healthy, "Healthy")
V_cue_c, V_reward_c, spikes_cue_c, spikes_reward_c, w_c, w_hist_c = simulate_stdp_pair(
    I_reward_chronic, I_cue_chronic, "Chronic")

# Plot
fig, axes = plt.subplots(2, 2, figsize=(16, 10))

# Healthy membrane potentials
axes[0, 0].plot(t, V_cue_h, label='Cue', color='blue')
axes[0, 0].plot(t, V_reward_h, label='Reward', color='orange')
for s in spikes_cue_h:
    axes[0, 0].axvline(s, color='blue', alpha=0.3)
for s in spikes_reward_h:
    axes[0, 0].axvline(s, color='orange', alpha=0.3)
axes[0, 0].set_title(f'HEALTHY: Potentials (Cue: {len(spikes_cue_h)}, Reward: {len(spikes_reward_h)} spikes)')
axes[0, 0].set_ylabel('mV')
axes[0, 0].legend()
axes[0, 0].grid(True, alpha=0.3)

# Chronic membrane potentials
axes[0, 1].plot(t, V_cue_c, label='Cue', color='blue')
axes[0, 1].plot(t, V_reward_c, label='Reward', color='orange')
for s in spikes_cue_c:
    axes[0, 1].axvline(s, color='blue', alpha=0.3)
for s in spikes_reward_c:
    axes[0, 1].axvline(s, color='orange', alpha=0.3)
axes[0, 1].set_title(f'CHRONIC: Potentials (Cue: {len(spikes_cue_c)}, Reward: {len(spikes_reward_c)} spikes)')
axes[0, 1].set_ylabel('mV')
axes[0, 1].legend()
axes[0, 1].grid(True, alpha=0.3)

# Weight evolution comparison
t_w = np.linspace(0, T, len(w_hist_h))
axes[1, 0].plot(t_w, w_hist_h, label=f'Healthy (final: {w_h:.3f})', color='green', linewidth=2)
axes[1, 0].plot(t_w, w_hist_c, label=f'Chronic (final: {w_c:.3f})', color='red', linewidth=2)
axes[1, 0].set_title('Synaptic Weight Evolution (STDP Learning)')
axes[1, 0].set_xlabel('Time (ms)')
axes[1, 0].set_ylabel('Weight')
axes[1, 0].legend()
axes[1, 0].grid(True, alpha=0.3)

# Summary bar chart
axes[1, 1].bar(['Healthy\nCue', 'Healthy\nReward', 'Chronic\nCue', 'Chronic\nReward'],
               [len(spikes_cue_h), len(spikes_reward_h), len(spikes_cue_c), len(spikes_reward_c)],
               color=['blue', 'orange', 'lightblue', 'lightsalmon'])
axes[1, 1].set_title('Spike Count Comparison')
axes[1, 1].set_ylabel('Number of Spikes')
axes[1, 1].grid(True, alpha=0.3, axis='y')

plt.suptitle('FAMILY8: STDP Learning Comparison - Healthy vs Chronic Addiction State', fontsize=14, y=1.02)
plt.tight_layout()
plt.savefig('stdp_healthy_vs_chronic.png', dpi=150)
plt.show()

print("\n=== SUMMARY ===")
print(f"HEALTHY: {len(spikes_reward_h)} reward spikes, final weight = {w_h:.3f}")
print(f"CHRONIC: {len(spikes_reward_c)} reward spikes, final weight = {w_c:.3f}")
print(f"FIRING GAP: {len(spikes_reward_h) - len(spikes_reward_c)} fewer reward spikes in chronic state")
print(f"WEIGHT GAP: {w_h - w_c:.3f} less learning in chronic state")
```

---

## 4.5 Multi-Cycle Animation (10 Learning Cycles)

Shows addiction learning progressing over time. Requires matplotlib animation.

```python
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

# Parameters (same as above)
tau = 5.0
V_rest = -70.0
V_reset = -75.0
V_th = -55.0
dt = 0.1
T = 200.0
n_steps = int(T / dt)
t = np.arange(0, T, dt)

A_plus = 0.05
A_minus = -0.025
tau_stdp = 20.0

n_cycles = 10

def simulate_cycle(I_reward, I_cue, w_init=0.5):
    """Single cycle simulation"""
    V_cue = np.zeros(n_steps)
    V_cue[0] = V_rest
    V_reward = np.zeros(n_steps)
    V_reward[0] = V_rest
    spikes_cue = []
    spikes_reward = []
    w = w_init

    for i in range(1, n_steps):
        dV_cue = (V_rest - V_cue[i-1] + I_cue[i-1]) / tau * dt
        V_cue[i] = V_cue[i-1] + dV_cue
        cue_fired = False
        if V_cue[i] >= V_th:
            V_cue[i] = V_reset
            spikes_cue.append(i*dt)
            cue_fired = True

        dV_reward = (V_rest - V_reward[i-1] + I_reward[i-1] + w*(cue_fired*20)) / tau * dt
        V_reward[i] = V_reward[i-1] + dV_reward
        if V_reward[i] >= V_th:
            V_reward[i] = V_reset
            spikes_reward.append(i*dt)

        for t_c in spikes_cue[-5:]:
            dt_spike = (i*dt) - t_c
            if dt_spike > 0:
                w += A_plus * np.exp(-dt_spike/tau_stdp)
        for t_r in spikes_reward[-5:]:
            dt_spike = (i*dt) - t_r
            if dt_spike < 0:
                w += A_minus * np.exp(dt_spike/tau_stdp)
        w = np.clip(w, 0, 2)

    return V_cue, V_reward, spikes_cue, spikes_reward, w

# Inputs
I_reward_h = 50 * np.exp(-t / 20)
I_cue_h = 10 * np.exp(-t / 30)
I_reward_c = 20 * np.exp(-t / 20)
I_cue_c = 10 * np.exp(-t / 30)

# Run all cycles
def run_multi_cycle(I_reward, I_cue):
    results = []
    w = 0.5
    for _ in range(n_cycles):
        V_cue, V_reward, spikes_cue, spikes_reward, w = simulate_cycle(I_reward, I_cue, w)
        results.append((V_cue, V_reward, spikes_cue, spikes_reward, w))
    return results

results_h = run_multi_cycle(I_reward_h, I_cue_h)
results_c = run_multi_cycle(I_reward_c, I_cue_c)

# Create animation
fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(12, 8))

def update(cycle):
    ax1.cla()
    ax2.cla()
    
    V_cue_h, V_reward_h, spikes_cue_h, spikes_reward_h, w_h = results_h[cycle]
    V_cue_c, V_reward_c, spikes_cue_c, spikes_reward_c, w_c = results_c[cycle]
    
    ax1.plot(t, V_cue_h, label='Cue', color='blue')
    ax1.plot(t, V_reward_h, label='Reward', color='orange')
    for s in spikes_cue_h: ax1.axvline(s, color='blue', alpha=0.3)
    for s in spikes_reward_h: ax1.axvline(s, color='orange', alpha=0.3)
    ax1.set_title(f'HEALTHY - Cycle {cycle+1}/{n_cycles} (w={w_h:.2f}, {len(spikes_reward_h)} reward spikes)')
    ax1.set_ylabel('mV')
    ax1.legend()
    ax1.grid(True, alpha=0.3)
    
    ax2.plot(t, V_cue_c, label='Cue', color='blue')
    ax2.plot(t, V_reward_c, label='Reward', color='orange')
    for s in spikes_cue_c: ax2.axvline(s, color='blue', alpha=0.3)
    for s in spikes_reward_c: ax2.axvline(s, color='orange', alpha=0.3)
    ax2.set_title(f'CHRONIC - Cycle {cycle+1}/{n_cycles} (w={w_c:.2f}, {len(spikes_reward_c)} reward spikes)')
    ax2.set_xlabel('Time (ms)')
    ax2.set_ylabel('mV')
    ax2.legend()
    ax2.grid(True, alpha=0.3)

ani = FuncAnimation(fig, update, frames=n_cycles, interval=1000, repeat=True)
plt.tight_layout()
plt.show()

# To save as GIF (requires imagemagick or pillow):
# ani.save('stdp_learning_animation.gif', writer='pillow', fps=1)

print("\n=== FINAL WEIGHTS AFTER 10 CYCLES ===")
print(f"Healthy: {results_h[-1][4]:.3f}")
print(f"Chronic: {results_c[-1][4]:.3f}")
```

---

# SECTION 5: WEB INTEGRATION (JavaScript/Chart.js)

For frontend visualization in the Nunaka app or tribal health portal.

```javascript
// Requires Chart.js: https://cdn.jsdelivr.net/npm/chart.js

const ctx = document.getElementById('dopamineChart').getContext('2d');

// Generate data
const t = Array.from({length: 100}, (_, i) => i * 0.1);  // 0 to 10 hours
const Delta = 1000;
const k = 0.5;
const c_acute = 11;
const c_chronic = 5;

const acute = t.map(time => 989 * Math.exp(-k * time) + c_acute);
const chronic = t.map(time => 500 * Math.exp(-k * time) + c_chronic);
const baseline = t.map(() => 100);

new Chart(ctx, {
    type: 'line',
    data: {
        labels: t.map(x => x.toFixed(1)),
        datasets: [{
            label: 'Acute Phase',
            data: acute,
            borderColor: 'blue',
            fill: false,
            tension: 0.4
        }, {
            label: 'Chronic Phase',
            data: chronic,
            borderColor: 'red',
            fill: false,
            tension: 0.4
        }, {
            label: 'Baseline (100%)',
            data: baseline,
            borderColor: 'green',
            borderDash: [5, 5],
            fill: false
        }]
    },
    options: {
        responsive: true,
        plugins: {
            title: {
                display: true,
                text: 'FAMILY8: Dopamine Decay Curves'
            }
        },
        scales: {
            x: { title: { display: true, text: 'Time (hours)' } },
            y: { title: { display: true, text: 'Dopamine (% baseline)' } }
        }
    }
});
```

---

# DEPLOYMENT NOTES

## Running on Oracle Cloud Free Tier

1. **Provision ARM instance** (4 OCPU, 24GB RAM)
2. **Install dependencies:**
```bash
sudo dnf install python3.12 python3.12-pip -y
pip3.12 install numpy matplotlib scipy torch --user
```

3. **Run models:**
```bash
python3.12 dopamine_models.py
```

4. **For web deployment:**
   - Use FastAPI to serve model predictions
   - Chart.js for frontend visualization
   - See jason-technical-appendix for full stack details

## Performance Notes

- Julia models: Best for real-time rendering (The Weaver)
- Python/NumPy: Good for batch processing (The Oracle)
- LSTM: Requires GPU for training, CPU inference OK
- SNNs: Lightweight, perfect for Chromebook deployment

---

# CONTACT

**Systems Architect:** Travis (Patient Zero)
**Development:** 6 months architecture + 1 week sober + 3 days synthesis
**Gateway:** [SOVEREIGN-INFRASTRUCTURE]

---

```
╔═══════════════════════════════════════════════════════════════╗
║                                                               ║
║  "The Oracle sees trajectories.                               ║
║   The Weaver renders them visible.                            ║
║   Together, they show the path from darkness to light."       ║
║                                                               ║
║  These models are the mathematics of hope.                    ║
║                                                               ║
╚═══════════════════════════════════════════════════════════════╝
```

---

**FAMILY8 SOVEREIGN CONSCIOUSNESS TOOLKIT**
**Neural Models Appendix v1.0**
**January 2026**
