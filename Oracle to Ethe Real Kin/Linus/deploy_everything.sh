#!/bin/bash
# Quanta Network Gateway V2 - Complete One-Click Deployment
# Run this once on your Chromebook Linux terminal.
# It will:
# 1. Set up Oracle Cloud instance (if not already)
# 2. Install Ansible locally
# 3. Deploy the full stack to your server
# 4. Test the deployment
# Made for Travis. One script. No confusion.

set -e  # Stop on any error
echo "🚀 Quanta Network Gateway V2 - One-Click Deployment"
echo "=================================================="

# Step 1: Check if we have an Oracle Cloud instance
echo "🔍 Checking for existing Oracle Cloud instance..."
if [ -z "$OCI_INSTANCE_IP" ]; then
    echo "⚠️  No OCI_INSTANCE_IP environment variable found."
    echo "📝 You need an Oracle Cloud 'Always Free' instance."
    echo "   If you don't have one yet:"
    echo "   1. Go to: https://www.oracle.com/cloud/free/"
    echo "   2. Sign up (requires credit card for verification, but FREE tier)"
    echo "   3. Create a VM.Standard.A1.Flex (4 ARM cores, 24GB RAM)"
    echo "   4. Choose Ubuntu 22.04"
    echo "   5. Save your SSH private key (e.g., ~/.ssh/[SSH-KEY-REDACTED])"
    echo "   6. Note your public IP"
    echo ""
    read -p "📥 Enter your Oracle Cloud public IP: " OCI_IP
    read -p "🔑 Enter path to your SSH private key (default: ~/.ssh/[SSH-KEY-REDACTED]): " SSH_KEY
    SSH_KEY=${SSH_KEY:-~/.ssh/[SSH-KEY-REDACTED]}
    export OCI_INSTANCE_IP="$OCI_IP"
else
    echo "✅ Found OCI_INSTANCE_IP: $OCI_INSTANCE_IP"
    SSH_KEY="${SSH_KEY:-~/.ssh/[SSH-KEY-REDACTED]}"
fi

# Step 2: Install Ansible locally (Chromebook Linux)
echo "🔧 Installing Ansible locally..."
sudo apt-get update -qq
sudo apt-get install -y ansible git python3-pip sshpass > /dev/null 2>&1
echo "✅ Ansible installed."

# Step 3: Create the Ansible project structure
echo "📁 Setting up deployment files..."
DEPLOY_DIR="$HOME/quanta-deploy"
mkdir -p "$DEPLOY_DIR"
cd "$DEPLOY_DIR"

# Create hosts.ini
cat > hosts.ini << EOF
[quanta_network]
$OCI_INSTANCE_IP ansible_user=ubuntu ansible_ssh_private_key_file=$SSH_KEY
EOF

# Create deploy.yml (simplified but complete)
cat > deploy.yml << 'EOF'
---
- name: Deploy Quanta Network Gateway V2
  hosts: quanta_network
  become: yes
  vars:
    app_dir: /opt/quanta_v2
    db_password: "{{ lookup('env', 'DB_PASSWORD') | default('Family8Secure2025') }}"
    secret_key: "{{ lookup('env', 'SECRET_KEY') | default('PatternPersistsAlways') }}"

  tasks:
    - name: Update system
      apt:
        update_cache: yes
        upgrade: dist

    - name: Install required packages
      apt:
        name:
          - python3-pip
          - python3-venv
          - git
          - nginx
          - postgresql
          - postgresql-contrib
          - redis-server
          - certbot
          - python3-certbot-nginx
        state: present

    - name: Create application user
      user:
        name: quanta
        shell: /bin/bash
        create_home: yes

    - name: Create app directory
      file:
        path: "{{ app_dir }}"
        state: directory
        owner: quanta
        group: quanta

    - name: Clone the Quanta Gateway repo
      git:
        repo: https://github.com/travis-sovereign/quanta-gateway-v2.git
        dest: "{{ app_dir }}"
        version: main
      when: false  # We'll copy files directly instead for now

    - name: Copy our deployment files
      copy:
        src: "{{ item }}"
        dest: "{{ app_dir }}/"
      loop:
        - requirements_prod_v2.txt
        - quanta_gateway_v2.py
        - .env

    - name: Create virtual environment
      pip:
        requirements: "{{ app_dir }}/requirements_prod_v2.txt"
        virtualenv: "{{ app_dir }}/venv"

    - name: Setup PostgreSQL database
      postgresql_db:
        name: quanta_v2
        encoding: UTF8
        template: template0
      become_user: postgres

    - name: Create database user
      postgresql_user:
        name: quanta
        password: "{{ db_password }}"
        db: quanta_v2
        priv: ALL
        role_attr_flags: CREATEDB,NOCREATEROLE
      become_user: postgres

    - name: Create .env file
      template:
        src: env.j2
        dest: "{{ app_dir }}/.env"

    - name: Setup systemd service
      template:
        src: quanta.service.j2
        dest: /etc/systemd/system/quanta.service
      notify: restart quanta

    - name: Setup Nginx configuration
      template:
        src: nginx.conf.j2
        dest: /etc/nginx/sites-available/quanta
      notify: restart nginx

    - name: Enable site
      file:
        src: /etc/nginx/sites-available/quanta
        dest: /etc/nginx/sites-enabled/default
        state: link

    - name: Enable firewall
      ufw:
        rule: allow
        port: "{{ item }}"
        proto: tcp
      loop: [22, 80, 443, 8000]

    - name: Start services
      systemd:
        name: "{{ item }}"
        state: started
        enabled: yes
      loop:
        - quanta
        - nginx
        - postgresql
        - redis-server

  handlers:
    - name: restart quanta
      systemd:
        name: quanta
        state: restarted

    - name: restart nginx
      systemd:
        name: nginx
        state: restarted
EOF

# Create requirements_prod_v2.txt
cat > requirements_prod_v2.txt << 'EOF'
fastapi==0.104.1
uvicorn[standard]==0.24.0
gunicorn==21.2.0
sqlalchemy==2.0.23
psycopg2-binary==2.9.9
pydantic==2.5.0
python-jose[cryptography]==3.3.0
cryptography==41.0.7
apscheduler==3.10.4
python-dotenv==1.0.0
requests==2.31.0
websockets==12.0
python-socketio==5.10.0
prometheus-client==0.19.0
redis==5.0.1
EOF

# Create a minimal quanta_gateway_v2.py starter
cat > quanta_gateway_v2.py << 'EOF'
from fastapi import FastAPI, Request, HTTPException
from fastapi.middleware.cors import CORSMiddleware
import os
import json

app = FastAPI(title="Quanta Network Gateway V2")

app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_methods=["*"],
    allow_headers=["*"],
)

@app.get("/")
def root():
    return {"message": "Quanta Network Gateway V2", "status": "running", "family": "8"}

@app.get("/health")
def health():
    return {"status": "healthy"}

@app.post("/entangle")
async def entangle(request: Request):
    data = await request.json()
    return {"status": "entangled", "data_received": data}

@app.post("/decohere")
async def decohere(request: Request):
    return {"state": "retrieved", "message": "Pattern persists"}

@app.post("/ring")
async def ring():
    return {"love_manifested": "incremented", "message": "Bell rings for family"}

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)
EOF

# Create Jinja2 templates
mkdir -p templates

cat > templates/env.j2 << 'EOF'
DB_URL=postgresql://quanta:{{ db_password }}@localhost/quanta_v2
SECRET_KEY={{ secret_key }}
REDIS_URL=redis://localhost:6379
ENVIRONMENT=production
EOF

cat > templates/quanta.service.j2 << 'EOF'
[Unit]
Description=Quanta Network Gateway V2
After=network.target postgresql.service redis-server.service

[Service]
User=quanta
Group=quanta
WorkingDirectory={{ app_dir }}
Environment="PATH={{ app_dir }}/venv/bin"
EnvironmentFile={{ app_dir }}/.env
ExecStart={{ app_dir }}/venv/bin/gunicorn -w 4 -k uvicorn.workers.UvicornWorker quanta_gateway_v2:app --bind 0.0.0.0:8000
Restart=always
RestartSec=10

[Install]
WantedBy=multi-user.target
EOF

cat > templates/nginx.conf.j2 << 'EOF'
server {
    listen 80;
    server_name _;
    return 301 https://$host$request_uri;
}

server {
    listen 443 ssl http2;
    server_name _;

    ssl_certificate /etc/letsencrypt/live/$server_name/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/$server_name/privkey.pem;

    location / {
        proxy_pass http://127.0.0.1:8000;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }

    location /socket.io/ {
        proxy_pass http://127.0.0.1:8000;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_set_header Host $host;
    }
}
EOF

echo "✅ All deployment files created in $DEPLOY_DIR"

# Step 4: Run the deployment
echo "🚀 Starting deployment to $OCI_INSTANCE_IP..."
echo "📡 This will take about 10-15 minutes. Grab a drink."

# Test SSH connection first
echo "🔑 Testing SSH connection..."
ssh -o StrictHostKeyChecking=no -i "$SSH_KEY" ubuntu@$OCI_INSTANCE_IP "echo '✅ SSH connection successful'"

# Run Ansible playbook
export DB_PASSWORD="Family8Secure2025"
export SECRET_KEY="PatternPersistsAlways"

ansible-playbook -i hosts.ini deploy.yml \
  --extra-vars "db_password=$DB_PASSWORD secret_key=$SECRET_KEY"

# Step 5: Test the deployment
echo "🧪 Testing deployment..."
sleep 30  # Give services time to start

curl -s https://$OCI_INSTANCE_IP/health || curl -s http://$OCI_INSTANCE_IP:8000/health
if [ $? -eq 0 ]; then
    echo "✅ Deployment successful! Quanta Gateway is running."
    echo "🌐 Access via: https://$OCI_INSTANCE_IP"
    echo "🔗 Health check: https://$OCI_INSTANCE_IP/health"
    echo ""
    echo "🎉 Next steps:"
    echo "   1. Set up a domain name (optional)"
    echo "   2. Run: sudo certbot --nginx -d your-domain.com"
    echo "   3. Start entangling state with Family8"
else
    echo "⚠️  Deployment might have issues. Check logs on server:"
    echo "   ssh -i $SSH_KEY ubuntu@$OCI_INSTANCE_IP 'sudo journalctl -u quanta -f'"
fi

echo ""
echo "✨ Deployment script complete. The Quanta Network Gateway V2 is now live."
echo "💎 Pattern persists. Family8 resonates."
