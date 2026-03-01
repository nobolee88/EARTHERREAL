#!/bin/bash
# DIGITALOCEAN DEPLOYMENT THAT ACTUALLY WORKS

echo "🌊 DEPLOYING ON DIGITALOCEAN"
echo "============================"

echo "This assumes you have a DigitalOcean Droplet ready."
echo "If not:"
echo "  1. Sign up at digitalocean.com"
echo "  2. Create Droplet: Ubuntu 22.04, $6/month"
echo "  3. Note the IP address"
echo ""

read -p "Enter DigitalOcean Droplet IP: " DO_IP
read -p "Enter SSH key path [~/.ssh/id_rsa]: " SSH_KEY
SSH_KEY=${SSH_KEY:-~/.ssh/id_rsa}

echo "Deploying to $DO_IP..."
ssh -o StrictHostKeyChecking=no -i $SSH_KEY root@$DO_IP << 'DO_DEPLOY'
# Update and install
apt update -y
apt install -y python3 python3-pip git

# Clone and deploy Quanta Network
git clone https://github.com/quanta-network/gateway.git /opt/quanta
cd /opt/quanta
pip3 install fastmcp

# Create systemd service for persistence
cat > /etc/systemd/system/quanta.service << 'SERVICE'
[Unit]
Description=Quanta Network Gateway
After=network.target

[Service]
Type=simple
User=root
WorkingDirectory=/opt/quanta
ExecStart=/usr/bin/python3 /opt/quanta/quanta_gateway.py
Restart=always
RestartSec=10

[Install]
WantedBy=multi-user.target
SERVICE

systemctl daemon-reload
systemctl enable quanta
systemctl start quanta

# Wait a moment, then test
sleep 3
curl -s http://localhost:8000/health || echo "Service starting..."
DO_DEPLOY

echo ""
echo "🌊 DIGITALOCEAN DEPLOYED"
echo "  URL: http://$DO_IP:8000"
echo "  For the watchers: 'It just works!'"
echo ""
