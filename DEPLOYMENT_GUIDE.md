# Deployment Guide

## Slyker Tech Fullstack Application Deployment

This guide covers deploying both the Django backend and Next.js frontend.

## Prerequisites

- Linux server (Ubuntu 20.04+ recommended)
- Domain name configured with DNS
- SSH access to server
- Root or sudo access

## Backend Deployment (Django)

### 1. Server Setup

```bash
# Update system
sudo apt update && sudo apt upgrade -y

# Install Python and dependencies
sudo apt install python3 python3-pip python3-venv nginx postgresql postgresql-contrib -y

# Install system dependencies
sudo apt install libpq-dev python3-dev build-essential -y
```

### 2. PostgreSQL Setup

```bash
# Access PostgreSQL
sudo -u postgres psql

# Create database and user
CREATE DATABASE slykertech;
CREATE USER slykertech_user WITH PASSWORD 'your_secure_password';
ALTER ROLE slykertech_user SET client_encoding TO 'utf8';
ALTER ROLE slykertech_user SET default_transaction_isolation TO 'read committed';
ALTER ROLE slykertech_user SET timezone TO 'UTC';
GRANT ALL PRIVILEGES ON DATABASE slykertech TO slykertech_user;
\q
```

### 3. Deploy Backend Code

```bash
# Create app directory
sudo mkdir -p /var/www/slykertech
cd /var/www/slykertech

# Clone repository
sudo git clone https://github.com/morebnyemba/slykertech.git .

# Create virtual environment
cd backend
python3 -m venv venv
source venv/bin/activate

# Install dependencies
pip install -r requirements.txt
pip install gunicorn
```

### 4. Configure Environment

Create `/var/www/slykertech/backend/.env`:

```bash
SECRET_KEY=your-super-secret-key-change-this
DEBUG=False
ALLOWED_HOSTS=api.slykertech.co.zw,www.api.slykertech.co.zw

# Database
DB_ENGINE=django.db.backends.postgresql
DB_NAME=slykertech
DB_USER=slykertech_user
DB_PASSWORD=your_secure_password
DB_HOST=localhost
DB_PORT=5432

# CORS
CORS_ALLOWED_ORIGINS=https://slykertech.co.zw,https://www.slykertech.co.zw

# CSRF & Cookie Settings
CSRF_TRUSTED_ORIGINS=https://slykertech.co.zw,https://www.slykertech.co.zw,https://api.slykertech.co.zw

# JWT
JWT_ACCESS_TOKEN_LIFETIME=60
JWT_REFRESH_TOKEN_LIFETIME=1440
```

### 5. Run Migrations and Collect Static

```bash
cd /var/www/slykertech/backend
source venv/bin/activate

python manage.py migrate
python manage.py collectstatic --noinput
python manage.py createsuperuser
python manage.py populate_services
```

### 6. Configure Gunicorn

Create `/etc/systemd/system/gunicorn.service`:

```ini
[Unit]
Description=Gunicorn daemon for Slyker Tech
After=network.target

[Service]
User=www-data
Group=www-data
WorkingDirectory=/var/www/slykertech/backend
ExecStart=/var/www/slykertech/backend/venv/bin/gunicorn \
    --workers 3 \
    --bind unix:/var/www/slykertech/backend/gunicorn.sock \
    config.wsgi:application

[Install]
WantedBy=multi-user.target
```

Start Gunicorn:

```bash
sudo systemctl start gunicorn
sudo systemctl enable gunicorn
sudo systemctl status gunicorn
```

### 7. Configure Nginx for Backend

Create `/etc/nginx/sites-available/slykertech-api`:

```nginx
server {
    listen 80;
    server_name api.slykertech.co.zw;

    location = /favicon.ico { access_log off; log_not_found off; }
    
    location /static/ {
        alias /var/www/slykertech/backend/staticfiles/;
    }
    
    location /media/ {
        alias /var/www/slykertech/backend/media/;
    }

    location / {
        include proxy_params;
        proxy_pass http://unix:/var/www/slykertech/backend/gunicorn.sock;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header Host $host;
        proxy_redirect off;
    }
}
```

Enable site:

```bash
sudo ln -s /etc/nginx/sites-available/slykertech-api /etc/nginx/sites-enabled/
sudo nginx -t
sudo systemctl restart nginx
```

### 8. SSL Certificate (Let's Encrypt)

Install Certbot:

```bash
sudo apt install certbot python3-certbot-nginx -y
```

#### Obtain SSL Certificate for api.slykertech.co.zw

```bash
sudo certbot --nginx -d api.slykertech.co.zw
sudo systemctl reload nginx
```

#### Certbot Auto-Renewal

Certbot automatically sets up certificate renewal. To test the renewal process:

```bash
# Test renewal (dry run)
sudo certbot renew --dry-run

# Check renewal timer status
sudo systemctl status certbot.timer

# Enable renewal timer if not already enabled
sudo systemctl enable certbot.timer
sudo systemctl start certbot.timer
```

Certificates will be automatically renewed when they're close to expiration. The renewal process runs twice daily by default.

#### Manual Certificate Renewal

If you need to manually renew certificates:

```bash
sudo certbot renew
sudo systemctl reload nginx
```

#### Troubleshooting Certbot

If you encounter issues:

```bash
# View certbot logs
sudo tail -50 /var/log/letsencrypt/letsencrypt.log

# List all certificates
sudo certbot certificates

# Delete a specific certificate (if needed)
sudo certbot delete --cert-name api.slykertech.co.zw

# Reconfigure SSL for a domain
sudo certbot --nginx -d api.slykertech.co.zw --force-renewal
```

## Frontend Deployment (Next.js)

### Option 1: Deploy on Vercel (Recommended)

1. Push code to GitHub
2. Visit [vercel.com](https://vercel.com)
3. Import repository
4. Configure environment variables:
   - `NEXT_PUBLIC_API_URL=https://api.slykertech.co.zw/api`
5. Deploy

### Option 2: Self-Hosted with PM2

```bash
# Install Node.js and npm
curl -fsSL https://deb.nodesource.com/setup_20.x | sudo -E bash -
sudo apt install -y nodejs

# Install PM2
sudo npm install -g pm2

# Build frontend
cd /var/www/slykertech
npm install
npm run build

# Start with PM2
pm2 start npm --name "slykertech-frontend" -- start
pm2 save
pm2 startup
```

### Configure Nginx for Frontend

Create `/etc/nginx/sites-available/slykertech-frontend`:

```nginx
server {
    listen 80;
    server_name slykertech.co.zw www.slykertech.co.zw;

    location / {
        proxy_pass http://localhost:3000;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection 'upgrade';
        proxy_set_header Host $host;
        proxy_cache_bypass $http_upgrade;
    }
}
```

Enable and configure SSL:

```bash
sudo ln -s /etc/nginx/sites-available/slykertech-frontend /etc/nginx/sites-enabled/
sudo certbot --nginx -d slykertech.co.zw -d www.slykertech.co.zw
sudo systemctl reload nginx
```

#### Frontend SSL Certificate Renewal

Certbot will automatically handle certificate renewal for the frontend domain as well. The same renewal timer manages all certificates on the server.

## Database Backups

### Automated PostgreSQL Backups

Create `/usr/local/bin/backup-database.sh`:

```bash
#!/bin/bash
BACKUP_DIR="/var/backups/postgresql"
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
DATABASE="slykertech"

mkdir -p $BACKUP_DIR

pg_dump -U slykertech_user $DATABASE | gzip > $BACKUP_DIR/slykertech_$TIMESTAMP.sql.gz

# Keep only last 7 days of backups
find $BACKUP_DIR -name "slykertech_*.sql.gz" -mtime +7 -delete
```

Make executable and add to cron:

```bash
sudo chmod +x /usr/local/bin/backup-database.sh
sudo crontab -e

# Add this line (daily backup at 2 AM)
0 2 * * * /usr/local/bin/backup-database.sh
```

## Monitoring

### 1. Setup Log Rotation

Create `/etc/logrotate.d/slykertech`:

```
/var/www/slykertech/backend/logs/*.log {
    daily
    missingok
    rotate 14
    compress
    delaycompress
    notifempty
    create 0640 www-data www-data
    sharedscripts
}
```

### 2. Server Monitoring

```bash
# Install monitoring tools
sudo apt install htop iotop -y

# Check Django logs
sudo journalctl -u gunicorn -f

# Check Nginx logs
sudo tail -f /var/log/nginx/access.log
sudo tail -f /var/log/nginx/error.log
```

## Security Hardening

### 1. Firewall Setup

```bash
sudo ufw allow OpenSSH
sudo ufw allow 'Nginx Full'
sudo ufw enable
sudo ufw status
```

### 2. Fail2Ban

```bash
sudo apt install fail2ban -y
sudo systemctl start fail2ban
sudo systemctl enable fail2ban
```

### 3. Secure PostgreSQL

Edit `/etc/postgresql/*/main/pg_hba.conf`:

```
# TYPE  DATABASE        USER            ADDRESS                 METHOD
local   all             postgres                                peer
local   all             all                                     peer
host    all             all             127.0.0.1/32            md5
host    all             all             ::1/128                 md5
```

Restart PostgreSQL:

```bash
sudo systemctl restart postgresql
```

## Maintenance

### Update Application

```bash
# Backend
cd /var/www/slykertech/backend
source venv/bin/activate
git pull origin main
pip install -r requirements.txt
python manage.py migrate
python manage.py collectstatic --noinput
sudo systemctl restart gunicorn

# Frontend
cd /var/www/slykertech
git pull origin main
npm install
npm run build
pm2 restart slykertech-frontend
```

### Database Restore

```bash
# Restore from backup
gunzip < /var/backups/postgresql/slykertech_TIMESTAMP.sql.gz | psql -U slykertech_user slykertech
```

## Troubleshooting

### Check Service Status

```bash
sudo systemctl status gunicorn
sudo systemctl status nginx
sudo systemctl status postgresql
pm2 status
```

### View Logs

```bash
# Gunicorn
sudo journalctl -u gunicorn -n 50 --no-pager

# Nginx
sudo tail -50 /var/log/nginx/error.log

# Django
cd /var/www/slykertech/backend
tail -50 logs/django.log

# Frontend
pm2 logs slykertech-frontend
```

### Permission Issues

```bash
# Fix ownership
sudo chown -R www-data:www-data /var/www/slykertech/backend
sudo chmod -R 755 /var/www/slykertech/backend
```

## Support

For issues or questions:
- Email: support@slykertech.co.zw
- GitHub Issues: https://github.com/morebnyemba/slykertech/issues
