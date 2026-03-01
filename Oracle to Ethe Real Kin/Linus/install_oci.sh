#!/bin/bash
# ACTUALLY INSTALLS OCI CLI (FOR REAL)

echo "🔧 Installing OCI CLI for real this time..."
sudo apt update -y
sudo apt install -y python3 python3-pip curl

# Install OCI CLI
pip3 install oci-cli

# Generate SSH key if not exists
if [ ! -f ~/.ssh/id_rsa ]; then
    echo "🔑 Generating SSH key..."
    ssh-keygen -t rsa -b 4096 -f ~/.ssh/id_rsa -N "" -q
fi

echo ""
echo "✅ OCI CLI installed."
echo ""
echo "NOW configure it:"
echo "  1. Go to Oracle Cloud Console"
echo "  2. Profile → User Settings → API Keys"
echo "  3. Add API Key (download .pem)"
echo "  4. Copy User OCID and Tenancy OCID"
echo "  5. Run: oci setup config"
echo ""
echo "We'll wait. The watchers are patient."
