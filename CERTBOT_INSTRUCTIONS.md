# Certbot SSL Certificate Instructions

This guide provides detailed instructions for setting up and managing SSL certificates using Certbot and Let's Encrypt for the Slykertech platform.

## Prerequisites

- Ubuntu/Debian server with root or sudo access
- Nginx web server installed and configured
- Domain names pointing to your server's IP address:
  - `api.slykertech.co.zw` (Backend API)
  - `slykertech.co.zw` (Frontend)
- Ports 80 and 443 open in your firewall

## Installation

### Install Certbot and Nginx Plugin

```bash
sudo apt update
sudo apt install certbot python3-certbot-nginx -y
```

## Obtaining SSL Certificates

### For Backend API (api.slykertech.co.zw)

```bash
# Single domain
sudo certbot --nginx -d api.slykertech.co.zw

# With www subdomain (if needed)
sudo certbot --nginx -d api.slykertech.co.zw -d www.api.slykertech.co.zw
```

### For Frontend (slykertech.co.zw)

```bash
# With and without www
sudo certbot --nginx -d slykertech.co.zw -d www.slykertech.co.zw
```

### Interactive Setup

During the Certbot setup, you'll be prompted for:

1. **Email address**: Enter a valid email for renewal notifications
2. **Terms of Service**: Accept by entering 'Y'
3. **Newsletter**: Optional - enter 'Y' or 'N'
4. **HTTPS redirect**: Choose option 2 to automatically redirect HTTP to HTTPS (recommended)

## Certificate Management

### List All Certificates

```bash
sudo certbot certificates
```

This shows all installed certificates with their expiration dates and domains.

### Check Certificate Expiration

```bash
sudo certbot certificates | grep -E "Certificate Name|Expiry Date"
```

### Renew Certificates

Certbot automatically sets up a systemd timer for renewal. Certificates are renewed when they have 30 days or less until expiration.

#### Manual Renewal (All Certificates)

```bash
sudo certbot renew
```

#### Renew Specific Certificate

```bash
sudo certbot renew --cert-name api.slykertech.co.zw
```

#### Test Renewal (Dry Run)

Test the renewal process without actually renewing:

```bash
sudo certbot renew --dry-run
```

#### Force Renewal

Force renewal even if certificates aren't close to expiration:

```bash
sudo certbot renew --force-renewal
```

### Automatic Renewal

Check the renewal timer status:

```bash
# Check timer status
sudo systemctl status certbot.timer

# View timer details
sudo systemctl list-timers | grep certbot

# Enable timer (should be enabled by default)
sudo systemctl enable certbot.timer

# Start timer
sudo systemctl start certbot.timer
```

The timer runs twice daily and will automatically renew certificates when needed.

### Post-Renewal Actions

After manual renewal, reload Nginx:

```bash
sudo systemctl reload nginx
```

For automatic renewals, you can add a post-renewal hook:

```bash
sudo nano /etc/letsencrypt/renewal-hooks/post/reload-nginx.sh
```

Add the following content:

```bash
#!/bin/bash
systemctl reload nginx
```

Make it executable:

```bash
sudo chmod +x /etc/letsencrypt/renewal-hooks/post/reload-nginx.sh
```

## Troubleshooting

### View Certbot Logs

```bash
sudo tail -50 /var/log/letsencrypt/letsencrypt.log
```

For more detailed logs:

```bash
sudo cat /var/log/letsencrypt/letsencrypt.log | less
```

### Common Issues

#### Port 80 Not Accessible

If Certbot can't validate your domain, check:

```bash
# Check if Nginx is running
sudo systemctl status nginx

# Check if port 80 is open
sudo netstat -tuln | grep :80

# Test domain accessibility
curl -I http://api.slykertech.co.zw

# Check firewall rules
sudo ufw status
```

#### Domain Not Resolving

Verify DNS configuration:

```bash
# Check DNS resolution
nslookup api.slykertech.co.zw
dig api.slykertech.co.zw

# Check if domain points to your server
host api.slykertech.co.zw
```

#### Rate Limiting

Let's Encrypt has rate limits. If you hit the limit:

- Wait before trying again
- Use `--dry-run` for testing
- Consider using the staging environment for testing:

```bash
sudo certbot --nginx -d api.slykertech.co.zw --staging
```

### Delete a Certificate

If you need to remove a certificate:

```bash
# Delete certificate
sudo certbot delete --cert-name api.slykertech.co.zw

# List certificates to find the exact name
sudo certbot certificates
```

### Revoke a Certificate

If a certificate is compromised:

```bash
sudo certbot revoke --cert-path /etc/letsencrypt/live/api.slykertech.co.zw/cert.pem
```

## Certificate Files Location

Certificates are stored in `/etc/letsencrypt/`:

```
/etc/letsencrypt/
├── live/
│   ├── api.slykertech.co.zw/
│   │   ├── cert.pem         # Certificate
│   │   ├── chain.pem        # Certificate chain
│   │   ├── fullchain.pem    # Certificate + chain
│   │   └── privkey.pem      # Private key
│   └── slykertech.co.zw/
│       ├── cert.pem
│       ├── chain.pem
│       ├── fullchain.pem
│       └── privkey.pem
├── renewal/                 # Renewal configuration
└── archive/                 # Certificate history
```

## Advanced Configuration

### Custom Renewal Configuration

Edit renewal configuration for a specific certificate:

```bash
sudo nano /etc/letsencrypt/renewal/api.slykertech.co.zw.conf
```

### Wildcard Certificates

For wildcard certificates (*.slykertech.co.zw), you'll need DNS validation:

```bash
sudo certbot certonly --manual --preferred-challenges dns -d *.slykertech.co.zw -d slykertech.co.zw
```

Follow the prompts to add TXT records to your DNS.

### Using Standalone Mode

If Nginx is not running, you can use standalone mode:

```bash
# Stop Nginx
sudo systemctl stop nginx

# Get certificate
sudo certbot certonly --standalone -d api.slykertech.co.zw

# Start Nginx
sudo systemctl start nginx
```

## Nginx Configuration for SSL

Example SSL configuration:

```nginx
# Backend API
server {
    listen 443 ssl http2;
    server_name api.slykertech.co.zw;
    
    ssl_certificate /etc/letsencrypt/live/api.slykertech.co.zw/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/api.slykertech.co.zw/privkey.pem;
    
    # SSL configuration
    ssl_protocols TLSv1.2 TLSv1.3;
    ssl_prefer_server_ciphers on;
    ssl_ciphers ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256;
    
    # Your location blocks...
}

# Redirect HTTP to HTTPS
server {
    listen 80;
    server_name api.slykertech.co.zw;
    return 301 https://$host$request_uri;
}
```

## Security Best Practices

1. **Always redirect HTTP to HTTPS** - Certbot can do this automatically
2. **Enable HSTS** - Force browsers to use HTTPS
3. **Keep certificates renewed** - Let the automatic renewal handle this
4. **Monitor certificate expiration** - Set up alerts
5. **Use strong SSL protocols** - TLSv1.2 and TLSv1.3 only
6. **Regular updates** - Keep Certbot and Nginx updated

## Monitoring

### Setup Email Notifications

Certbot sends email notifications about certificate expiration. Ensure you:

1. Provide a valid email during setup
2. Check renewal logs regularly
3. Monitor the renewal timer

### Check Renewal Status

```bash
# Check when certificates will be renewed
sudo certbot certificates

# View renewal configuration
ls -la /etc/letsencrypt/renewal/

# Test renewal process
sudo certbot renew --dry-run
```

## Backup

Always backup your certificates before major changes:

```bash
# Backup entire letsencrypt directory
sudo tar -czf letsencrypt-backup-$(date +%Y%m%d).tar.gz /etc/letsencrypt/

# Store backup securely
sudo mv letsencrypt-backup-*.tar.gz /root/backups/
```

## Support

For issues:
- Certbot Documentation: https://certbot.eff.org/docs/
- Let's Encrypt Community: https://community.letsencrypt.org/
- Slykertech Support: support@slykertech.co.zw

## Quick Reference

```bash
# Install
sudo apt install certbot python3-certbot-nginx -y

# Get certificate
sudo certbot --nginx -d api.slykertech.co.zw

# Renew all
sudo certbot renew

# Test renewal
sudo certbot renew --dry-run

# List certificates
sudo certbot certificates

# Delete certificate
sudo certbot delete --cert-name api.slykertech.co.zw

# View logs
sudo tail -50 /var/log/letsencrypt/letsencrypt.log
```
