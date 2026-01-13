# SSL, WebSocket, and Production Deployment Guide

## Overview

This guide covers the complete setup for deploying Slyker Tech with:
- SSL/HTTPS certificates for api.slykertech.co.zw and slykertech.co.zw
- WebSocket support for real-time updates
- Docker-based production deployment
- Nginx reverse proxy configuration

## Prerequisites

✅ Ubuntu/Debian server with root access
✅ Docker and Docker Compose installed
✅ Domain names pointing to your server:
  - `api.slykertech.co.zw` (Backend API)
  - `slykertech.co.zw` (Frontend)
  - `www.slykertech.co.zw` (Frontend www)
✅ Ports 80 and 443 open in firewall

## Step 1: Install Docker and Docker Compose

```bash
# Install Docker
curl -fsSL https://get.docker.com -o get-docker.sh
sudo sh get-docker.sh

# Install Docker Compose
sudo curl -L "https://github.com/docker/compose/releases/latest/download/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
sudo chmod +x /usr/local/bin/docker-compose

# Add your user to docker group
sudo usermod -aG docker $USER
newgrp docker
```

## Step 2: Obtain SSL Certificates

### Install Certbot

```bash
sudo apt update
sudo apt install certbot python3-certbot-nginx -y
```

### Configure Temporary Nginx for Certificate Validation

Before obtaining certificates, set up a basic nginx configuration:

```bash
# Stop any existing services on port 80
sudo systemctl stop nginx

# Create temporary nginx config
sudo tee /etc/nginx/sites-available/temp > /dev/null <<'EOF'
server {
    listen 80;
    server_name api.slykertech.co.zw;
    
    location / {
        return 200 "OK";
    }
}

server {
    listen 80;
    server_name slykertech.co.zw www.slykertech.co.zw;
    
    location / {
        return 200 "OK";
    }
}
EOF

# Enable and start nginx
sudo ln -s /etc/nginx/sites-available/temp /etc/nginx/sites-enabled/
sudo systemctl start nginx
```

### Obtain Certificates

```bash
# For Backend API (api.slykertech.co.zw)
sudo certbot --nginx -d api.slykertech.co.zw

# For Frontend (slykertech.co.zw and www subdomain)
sudo certbot --nginx -d slykertech.co.zw -d www.slykertech.co.zw
```

**Important:** After obtaining certificates, stop the nginx service:

```bash
sudo systemctl stop nginx
sudo systemctl disable nginx
```

We'll use Docker's nginx container instead.

## Step 3: Clone Repository and Configure

```bash
# Clone repository
git clone https://github.com/morebnyemba/slykertech.git
cd slykertech

# Create environment file
cp .env.example .env

# Edit environment variables
nano .env
```

### Configure .env file

```env
# Database
DB_PASSWORD=your_secure_database_password

# Django
DEBUG=False
SECRET_KEY=your_secret_key_here

# API URLs (Production)
NEXT_PUBLIC_API_URL=https://api.slykertech.co.zw/api
NEXT_PUBLIC_SITE_URL=https://slykertech.co.zw

# Node Environment
NODE_ENV=production
```

## Step 4: Deploy with Docker Compose

### Build and Start Services

```bash
# Build images
docker-compose build

# Start services
docker-compose up -d

# Check status
docker-compose ps
```

### Run Django Migrations

```bash
# Run migrations
docker-compose exec backend python manage.py migrate

# Create superuser
docker-compose exec backend python manage.py createsuperuser

# Collect static files
docker-compose exec backend python manage.py collectstatic --noinput
```

## Step 5: Verify Services

### Check Service Status

```bash
# View logs
docker-compose logs -f

# Check specific service
docker-compose logs backend
docker-compose logs frontend
docker-compose logs nginx
```

### Test Endpoints

```bash
# Test Backend API (HTTPS)
curl https://api.slykertech.co.zw/api/health/

# Test Frontend (HTTPS)
curl https://slykertech.co.zw/

# Test WebSocket (backend)
wscat -c wss://api.slykertech.co.zw/ws/analytics/
```

## Step 6: SSL Certificate Auto-Renewal

Certbot automatically sets up renewal. Verify it's working:

```bash
# Test renewal (dry run)
sudo certbot renew --dry-run

# Check renewal timer
sudo systemctl status certbot.timer
```

### Manual Renewal (if needed)

```bash
# Stop nginx container
docker-compose stop nginx

# Renew certificates
sudo certbot renew

# Start nginx container
docker-compose start nginx
```

## Architecture Overview

### Network Flow

```
Internet
   ↓
[Port 443 - HTTPS/WSS]
   ↓
Nginx (Docker Container)
   ├─→ Frontend (Next.js) - Port 3000
   ├─→ Backend API (Django/Daphne) - Port 8000
   └─→ WebSocket (Django Channels) - Port 8000
```

### SSL Certificate Locations

Certificates are mounted from host to nginx container:

```
Host: /etc/letsencrypt/
  ├─ live/api.slykertech.co.zw/
  │   ├─ fullchain.pem
  │   └─ privkey.pem
  └─ live/slykertech.co.zw/
      ├─ fullchain.pem
      └─ privkey.pem

Docker Container: /etc/letsencrypt/ (read-only mount)
```

## WebSocket Configuration

### Backend WebSocket Endpoints

1. **Analytics Dashboard** (`/ws/analytics/`)
   - Real-time dashboard metrics
   - Subscription updates
   - Invoice notifications

2. **DNS Panel** (`/ws/dns/`)
   - Real-time DNS record management
   - Live updates for record changes
   - Multi-user synchronization

### Frontend WebSocket Usage

```typescript
// Example: Connect to analytics WebSocket
import { useAnalyticsWebSocket } from '@/lib/hooks/use-websocket';

const { isConnected, sendMessage } = useAnalyticsWebSocket((data) => {
  console.log('Received update:', data);
});

// Request update
sendMessage({ type: 'request_update' });
```

## Troubleshooting

### SSL Certificate Issues

```bash
# Check certificate status
sudo certbot certificates

# View nginx logs
docker-compose logs nginx

# Verify certificate files
sudo ls -la /etc/letsencrypt/live/
```

### WebSocket Connection Issues

```bash
# Check if Daphne is running
docker-compose exec backend ps aux | grep daphne

# Test WebSocket endpoint
docker-compose exec backend python manage.py shell
>>> from channels.testing import WebsocketCommunicator
>>> # Test consumer connection
```

### Port Conflicts

```bash
# Check what's using port 80/443
sudo netstat -tulpn | grep :80
sudo netstat -tulpn | grep :443

# Stop conflicting services
sudo systemctl stop apache2  # if installed
sudo systemctl stop nginx    # if running on host
```

### Database Connection Issues

```bash
# Check database status
docker-compose exec db pg_isready -U slykertech

# View database logs
docker-compose logs db

# Connect to database
docker-compose exec db psql -U slykertech
```

## Monitoring and Maintenance

### View Application Logs

```bash
# All services
docker-compose logs -f

# Specific service with timestamp
docker-compose logs -f --timestamps backend

# Last 100 lines
docker-compose logs --tail=100 backend
```

### Resource Usage

```bash
# Check container resource usage
docker stats

# Disk usage
docker system df
```

### Backup Database

```bash
# Create backup
docker-compose exec db pg_dump -U slykertech slykertech > backup_$(date +%Y%m%d).sql

# Restore backup
docker-compose exec -T db psql -U slykertech slykertech < backup_20260113.sql
```

## Security Considerations

1. **SSL/TLS**: All traffic is encrypted via HTTPS
2. **WebSocket Security**: WSS (WebSocket Secure) is used
3. **Authentication**: JWT tokens for API access
4. **CORS**: Configured to allow only trusted domains
5. **CSRF**: Django CSRF protection enabled
6. **Database**: PostgreSQL with strong password
7. **Secrets**: Use environment variables, never commit secrets

## Performance Optimization

### Enable Redis Caching

Redis is already included in docker-compose for:
- Django caching
- Celery task queue
- Channels layer (WebSocket backend)

### Nginx Optimization

The nginx configuration includes:
- HTTP/2 support
- SSL session caching
- Gzip compression (can be added)
- Static file caching

### Django Optimization

```bash
# Collect static files
docker-compose exec backend python manage.py collectstatic --noinput

# Run database optimization
docker-compose exec db vacuumdb -U slykertech slykertech
```

## Scaling Considerations

### Horizontal Scaling

To scale services:

```bash
# Scale backend workers
docker-compose up -d --scale backend=3

# Scale celery workers
docker-compose up -d --scale celery=2
```

### Load Balancing

For multiple backend instances, update nginx upstream:

```nginx
upstream backend {
    least_conn;
    server backend1:8000;
    server backend2:8000;
    server backend3:8000;
}
```

## Support and Updates

### Update Application

```bash
# Pull latest changes
git pull origin main

# Rebuild containers
docker-compose build

# Restart services (zero-downtime)
docker-compose up -d --no-deps --build backend frontend

# Run migrations
docker-compose exec backend python manage.py migrate
```

### Certificate Renewal

Certificates auto-renew via certbot timer. Manual renewal:

```bash
docker-compose stop nginx
sudo certbot renew
docker-compose start nginx
```

## Quick Reference

```bash
# Start all services
docker-compose up -d

# Stop all services
docker-compose down

# Restart specific service
docker-compose restart backend

# View logs
docker-compose logs -f service_name

# Execute command in container
docker-compose exec backend python manage.py shell

# Check service health
curl https://api.slykertech.co.zw/health/
```

## Contact

For deployment assistance:
- Email: support@slykertech.co.zw
- Phone: +263 78 721 1325

---

**Last Updated:** January 13, 2026
