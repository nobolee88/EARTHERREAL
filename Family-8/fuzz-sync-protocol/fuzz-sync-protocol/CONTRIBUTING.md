# Contributing to FFSP

Thanks for your interest in contributing to the Fuzz-Full Family Sync Protocol!

## Ways to Contribute

### 1. Implementations
- Port to new MCU platforms (STM32, PIC, RISC-V)
- Implement new medium types (RF, ultrasonic, magnetic)
- Create language bindings (Rust, Go, MicroPython)

### 2. Hardware Designs
- Reference PCB layouts
- Antenna/electrode designs for different media
- Power management circuits

### 3. Documentation
- Tutorials and getting-started guides
- Application notes for specific use cases
- Translations

### 4. Testing & Validation
- Timing analysis and verification
- Collision behavior characterization
- Power consumption measurements

### 5. Security
- Formal analysis of rhythm-lock security
- Attack surface documentation
- Hardening recommendations

## Development Process

1. **Fork & Branch**
   - Fork the repository
   - Create a feature branch: `git checkout -b feature/your-feature`

2. **Implement**
   - Follow existing code style
   - Add tests for new functionality
   - Update documentation as needed

3. **Test**
   - Run simulation tests: `python src/python/ffsp_sim.py`
   - For hardware: test with at least 3 nodes

4. **Submit**
   - Open a Pull Request
   - Describe what you've done and why
   - Link any related issues

## Code Style

### Python
- Follow PEP 8
- Use type hints
- Docstrings for all public functions

### C/Arduino
- K&R style bracing
- `snake_case` for functions and variables
- `UPPER_CASE` for constants
- Comment non-obvious logic

### Documentation
- Markdown for all docs
- Include diagrams where helpful (Mermaid preferred)

## Protocol Changes

Changes to the core protocol specification require:
- Discussion in an Issue first
- Backward compatibility analysis
- Update to PROTOCOL_SPEC.md
- Reference implementation updates

## Questions?

Open an Issue with the `question` label.

## Code of Conduct

Be excellent to each other. We're building something to help systems work together — let's model that ourselves.

---

*"Efficiency through subtlety — whispers over shouts, resonance over force."*
