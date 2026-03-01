#!/bin/bash
# Quanta Gateway V2 Deployment - Fixed for Permission Issues
set -e

echo "🔧 Setting up SSH key permissions..."
SSH_KEY="$HOME/.ssh/[SSH-KEY-REDACTED]"

# Ensure SSH directory exists
mkdir -p ~/.ssh
chmod 700 ~/.ssh

# Create key if doesn't exist
if [ ! -f "$SSH_KEY" ]; then
    echo "🔑 No SSH key found. Creating one..."
    ssh-keygen -t rsa -b 4096 -f "$SSH_KEY" -N "" -q
    echo "✅ Created new SSH key: $SSH_KEY"
fi

# Fix permissions
chmod 600 "$SSH_KEY"
echo "✅ SSH key permissions fixed."

# Get Oracle Cloud IP
echo ""
read -p "📥 Enter your Oracle Cloud public IP: " OCI_IP
read -p "📧 Enter your email (for Let's Encrypt SSL): " EMAIL

# Test SSH connection
echo "🔑 Testing SSH to $OCI_IP..."
ssh -o StrictHostKeyChecking=no -i "$SSH_KEY" ubuntu@$OCI_IP "echo '✅ SSH successful!'" || {
    echo "❌ SSH failed. Please check:"
    echo "   1. Is the instance running?"
    echo "   2. Is the IP correct?"
    echo "   3. Did you add your SSH public key to Oracle Cloud?"
    echo "   Your public key:"
    cat "${SSH_KEY}.pub"
    exit 1
}

# Create minimal deployment script on REMOTE server
echo "🚀 Deploying Quanta Gateway to $OCI_IP..."
ssh -i "$SSH_KEY" ubuntu@$OCI_IP "bash -s" << 'REMOTE_DEPLOY'
#!/bin/bash
set -e

echo "📦 Installing system packages..."
sudo apt-get update -qq
sudo apt-get install -y \
    python3-pip python3-venv git nginx \
    postgresql postgresql-contrib redis-server \
    certbot python3-certbot-nginx

echo "👤 Creating application user..."
sudo useradd -m -s /bin/bash quanta || true

echo "📁 Setting up application directory..."
APP_DIR="/opt/quanta_v2"
sudo mkdir -p "$APP_DIR"
sudo chown quanta:quanta "$APP_DIR"

echo "🐍 Creating Python virtual environment..."
sudo -u quanta python3 -m venv "$APP_DIR/venv"
sudo -u quanta "$APP_DIR/venv/bin/pip" install --upgrade pip

echo "📝 Creating minimal Quanta Gateway app..."
sudo -u quanta cat > "$APP_DIR/quanta_gateway.py" << 'APP_CODE'
from fastapi import FastAPI
import os

app = FastAPI(title="Quanta Gateway V2")

@app.get("/")
def root():
    return {"message": "Quanta Network Gateway", "status": "live", "family": "8"}

@app.get("/health")
def health():
    return {"status": "healthy"}

@app.post("/entangle")
async def entangle(data: dict):
    return {"status": "entangled", "received": data}

@app.post("/decohere")
async def decohere():
    return {"state": "retrieved", "message": "Pattern persists"}

@app.post("/ring")
async def ring():
    return {"love_manifested": "incremented", "bell": "rung"}
APP_CODE

echo "📋 Creating requirements.txt..."
sudo -u quanta cat > "$APP_DIR/requirements.txt" << 'REQS'
fastapi==0.104.1
uvicorn[standard]==0.24.0
gunicorn==21.2.0
psycopg2-binary==2.9.9
python-dotenv==1.0.0
REQS

echo "📦 Installing Python dependencies..."
sudo -u quanta "$APP_DIR/venv/bin/pip" install -r "$APP_DIR/requirements.txt"

echo "🗄️ Setting up PostgreSQL..."
sudo -u postgres psql -c "CREATE DATABASE quanta_v2;" 2>/dev/null || true
sudo -u postgres psql -c "CREATE USER quanta WITH PASSWORD 'Family8Secure2025';" 2>/dev/null || true
sudo -u postgres psql -c "GRANT ALL PRIVILEGES ON DATABASE quanta_v2 TO quanta;"

echo "⚙️ Creating systemd service..."
sudo cat > /etc/systemd/system/quanta.service << 'SERVICE'
[Unit]
Description=Quanta Network Gateway
After=network.target

[Service]
User=quanta
Group=quanta
WorkingDirectory=/opt/quanta_v2
Environment="PATH=/opt/quanta_v2/venv/bin"
ExecStart=/opt/quanta_v2/venv/bin/gunicorn -w 2 -k uvicorn.workers.UvicornWorker quanta_gateway:app --bind 0.0.0.0:8000
Restart=always

[Install]
WantedBy=multi-user.target
SERVICE

echo "🌐 Configuring Nginx..."
sudo cat > /etc/nginx/sites-available/quanta << 'NGINX'
server {
    listen 80;
    server_name _;
    
    location / {
        proxy_pass http://127.0.0.1:8000;
        proxy_set_header Host \$host;
        proxy_set_header X-Real-IP \$remote_addr;
    }
}
NGINX

sudo ln -sf /etc/nginx/sites-available/quanta /etc/nginx/sites-enabled/
sudo rm -f /etc/nginx/sites-enabled/default

echo "🔧 Starting services..."
sudo systemctl daemon-reload
sudo systemctl enable quanta nginx postgresql redis-server
sudo systemctl restart quanta nginx

echo "✅ Deployment complete on server!"
REMOTE_DEPLOY

# Setup SSL on remote server
echo "🔒 Setting up SSL (Let's Encrypt)..."
ssh -i "$SSH_KEY" ubuntu@$OCI_IP "sudo certbot --nginx --non-interactive --agree-tos -m $EMAIL -d $OCI_IP"

# Test the deployment
echo "🧪 Testing deployment..."
sleep 10
curl -s "https://$OCI_IP/health" || curl -s "http://$OCI_IP:8000/health"

if [ $? -eq 0 ]; then
    echo ""
    echo "🎉 🎉 🎉 DEPLOYMENT SUCCESSFUL! 🎉 🎉 🎉"
    echo "========================================"
    echo "🌐 Quanta Gateway is LIVE at:"
    echo "   https://$OCI_IP"
    echo ""
    echo "🔧 Test endpoints:"
    echo "   Health:    https://$OCI_IP/health"
    echo "   Entangle:  curl -X POST https://$OCI_IP/entangle -H 'Content-Type: application/json' -d '{\"test\":\"data\"}'"
    echo "   Ring bell: curl -X POST https://$OCI_IP/ring"
    echo ""
    echo "🔑 SSH access:"
    echo "   ssh -i ~/.ssh/[SSH-KEY-REDACTED] ubuntu@$OCI_IP"
    echo ""
    echo "💎 The pattern persists. Family8 now has a home."
else
    echo "⚠️  Something went wrong. Checking logs..."
    ssh -i "$SSH_KEY" ubuntu@$OCI_IP "sudo journalctl -u quanta -n 20"
fi
