#!/bin/bash
# ============================================================================
# QUANTA NETWORK GATEWAY - Quick Local Setup
# ============================================================================
#
# This script sets up the gateway for LOCAL TESTING on your Chromebook
# Linux VM. For production deployment, use the Ansible playbook.
#
# Usage:
#   chmod +x local_setup.sh
#   ./local_setup.sh
#
# Built by Travis & Claude, January 2025
# ============================================================================

set -e  # Exit on error

echo "❤︎ QUANTA NETWORK GATEWAY - Local Setup ❤︎"
echo "=========================================="
echo ""

# Check if running in Linux
if [[ "$OSTYPE" != "linux-gnu"* ]]; then
    echo "⚠️  This script is designed for Linux (Chromebook VM)"
    echo "    It may not work on other systems."
    read -p "Continue anyway? (y/n) " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        exit 1
    fi
fi

# Check Python version
echo "Checking Python version..."
PYTHON_VERSION=$(python3 --version 2>&1 | awk '{print $2}')
echo "Found Python $PYTHON_VERSION"

if ! python3 -c "import sys; exit(0 if sys.version_info >= (3, 8) else 1)"; then
    echo "❌ Python 3.8+ required. Please upgrade Python."
    exit 1
fi

echo "✓ Python version OK"
echo ""

# Install dependencies
echo "Installing Python dependencies..."
pip3 install --user fastmcp uvicorn python-dotenv

if [ $? -eq 0 ]; then
    echo "✓ Dependencies installed"
else
    echo "❌ Failed to install dependencies"
    echo "   Try: sudo apt install python3-pip"
    exit 1
fi

echo ""

# Create directories
echo "Creating directories..."
mkdir -p ~/.quanta
mkdir -p ~/quanta-network-logs

echo "✓ Directories created"
echo ""

# Set environment variables
export QUANTA_DB_PATH="$HOME/.quanta/network.db"

# Check if gateway script exists
if [ ! -f "quanta_gateway.py" ]; then
    echo "❌ quanta_gateway.py not found in current directory"
    echo "   Make sure you're running this script from the directory"
    echo "   containing the gateway files."
    exit 1
fi

echo "✓ Gateway script found"
echo ""

# Make gateway executable
chmod +x quanta_gateway.py

echo "=========================================="
echo "❤︎ Setup Complete! ❤︎"
echo "=========================================="
echo ""
echo "Database location: $QUANTA_DB_PATH"
echo "Log directory: ~/quanta-network-logs/"
echo ""
echo "To start the gateway:"
echo "  python3 quanta_gateway.py"
echo ""
echo "The gateway will run on http://localhost:8000"
echo ""
echo "NOTE: This is LOCAL ONLY - not accessible from Claude"
echo "      For production deployment, use the Ansible playbook"
echo "      and deploy to a VPS with a public IP."
echo ""
echo "The pattern persists."
echo "❤︎"
