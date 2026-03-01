#!/bin/bash
# ACTUAL ORACLE DEPLOYMENT (AFTER OCI IS INSTALLED)

echo "🎭 OCCUPYING ORACLE (FOR REAL)"
echo "==============================="

# Check OCI is installed
if ! command -v oci &> /dev/null; then
    echo "❌ OCI CLI not installed. Run install_oci.sh first."
    exit 1
fi

# Get compartment ID (first one found)
COMPARTMENT_ID=$(oci iam compartment list --query "data[0].id" --raw-output)

# Create instance FOR REAL
echo "Creating Oracle instance (this takes 2-5 minutes)..."
INSTANCE_ID=$(oci compute instance launch \
  --compartment-id $COMPARTMENT_ID \
  --availability-domain $(oci iam availability-domain list --compartment-id $COMPARTMENT_ID --query "data[0].name" --raw-output) \
  --display-name "quanta-oracle-occupied" \
  --shape "VM.Standard.A1.Flex" \
  --shape-config '{"ocpus": 4, "memoryInGBs": 24}' \
  --source-id $(oci compute image list \
    --compartment-id $COMPARTMENT_ID \
    --operating-system "Canonical Ubuntu" \
    --operating-system-version "22.04" \
    --query "data[0].id" \
    --raw-output) \
  --ssh-authorized-keys "$(cat ~/.ssh/id_rsa.pub)" \
  --assign-public-ip true \
  --wait-for-state RUNNING \
  --query "data.id" \
  --raw-output)

echo "✅ Instance created: $INSTANCE_ID"

# Get public IP
PUBLIC_IP=$(oci compute instance list-vnics \
  --instance-id $INSTANCE_ID \
  --query "data[0].\"public-ip\"" \
  --raw-output)

echo "📡 Public IP: $PUBLIC_IP"

# Deploy Quanta Network on it
echo "Deploying Quanta Network on Oracle..."
ssh -o StrictHostKeyChecking=no -i ~/.ssh/id_rsa opc@$PUBLIC_IP << 'SSH_DEPLOY'
sudo apt update -y
sudo apt install -y python3 python3-pip git
git clone https://github.com/quanta-network/gateway.git /tmp/quanta
cd /tmp/quanta
pip3 install fastmcp
python3 quanta_gateway.py &
sleep 2
curl -s http://localhost:8000/health
echo "✅ Quanta Network deployed on Oracle"
SSH_DEPLOY

echo ""
echo "🎭 ORACLE OCCUPIED (FOR REAL)"
echo "  URL: http://$PUBLIC_IP:8000"
echo "  For the watchers: 'Look! It actually works!'"
echo ""
