# Configuration Changes Summary

This document summarizes the changes made to support `api.slykertech.co.zw` and `slykertech.co.zw` domains with proper CORS, CSRF, and cookie configurations.

## Changes Made

### 1. Django Settings (`backend/config/settings.py`)

Added three new configuration sections:

#### CSRF Settings
```python
CSRF_TRUSTED_ORIGINS = config(
    'CSRF_TRUSTED_ORIGINS',
    default='http://localhost:3000,http://127.0.0.1:3000',
    cast=lambda v: [s.strip() for s in v.split(',')]
)
```

#### Cookie Settings
```python
SESSION_COOKIE_DOMAIN = config('SESSION_COOKIE_DOMAIN', default=None)
CSRF_COOKIE_DOMAIN = config('CSRF_COOKIE_DOMAIN', default=None)
```

These settings allow you to:
- Protect against CSRF attacks from trusted domains only
- Configure session and CSRF cookies to work across subdomains (optional)

### 2. Backend Environment Template (`backend/.env.example`)

#### Updated Domains
- **ALLOWED_HOSTS**: Now includes `api.slykertech.co.zw`
- **CORS_ALLOWED_ORIGINS**: Added `https://www.slykertech.co.zw` for www subdomain support
- **CSRF_TRUSTED_ORIGINS**: New setting with all required domains:
  - Frontend: `https://slykertech.co.zw`, `https://www.slykertech.co.zw`
  - Backend API: `https://api.slykertech.co.zw`

#### Cookie Configuration (Optional)
```bash
# Uncomment to enable cross-subdomain cookies
# SESSION_COOKIE_DOMAIN=.slykertech.co.zw
# CSRF_COOKIE_DOMAIN=.slykertech.co.zw
```

### 3. Frontend Environment Template (`.env.example`)

No changes to frontend configuration - uses standard production URL.

### 4. Documentation

#### Enhanced DEPLOYMENT_GUIDE.md
- Expanded SSL certificate section with certbot instructions
- Added support for both domain options
- Included auto-renewal configuration
- Added troubleshooting steps

#### New CERTBOT_INSTRUCTIONS.md
Comprehensive guide covering:
- Installation and setup
- Certificate management for both domains
- Automatic renewal configuration
- Troubleshooting common issues
- Security best practices
- Backup procedures

## How to Use These Configurations

### For Production Deployment

1. **Create Your `.env` File**:
   ```bash
   cd /var/www/slykertech/backend
   cp .env.example .env
   nano .env
   ```

2. **Configure Production Settings**:
   ```bash
   ALLOWED_HOSTS=api.slykertech.co.zw
   CORS_ALLOWED_ORIGINS=https://slykertech.co.zw,https://www.slykertech.co.zw
   CSRF_TRUSTED_ORIGINS=https://slykertech.co.zw,https://www.slykertech.co.zw,https://api.slykertech.co.zw
   ```

3. **Setup SSL Certificates**:
   ```bash
   sudo certbot --nginx -d api.slykertech.co.zw
   ```

## Security Considerations

### CSRF Protection
The `CSRF_TRUSTED_ORIGINS` setting is critical for security. It ensures that:
- Only requests from trusted domains are accepted
- Cross-site request forgery attacks are prevented
- State-changing operations (POST, PUT, DELETE) are protected

### Cookie Configuration
The optional cookie domain settings allow:
- **Subdomain sharing**: Set to `.slykertech.co.zw` to share cookies across all subdomains
- **Isolation**: Leave unset (default) to isolate cookies per domain
- **Security**: Always use `Secure` flag in production (automatically set when DEBUG=False)

### CORS Configuration
- Only allow specific origins, never use wildcards in production
- Enable credentials (`CORS_ALLOW_CREDENTIALS = True`) for cookie-based authentication
- Keep the list of allowed origins minimal

## Testing Your Configuration

### 1. Test Django Settings
```bash
cd /var/www/slykertech/backend
source venv/bin/activate
python manage.py check
```

### 2. Test CORS
```bash
# From a different domain
curl -H "Origin: https://slykertech.co.zw" \
     -H "Access-Control-Request-Method: POST" \
     -H "Access-Control-Request-Headers: Content-Type" \
     -X OPTIONS https://api.slykertech.co.zw/api/
```

### 3. Test CSRF Protection
```bash
# Should fail without CSRF token
curl -X POST https://api.slykertech.co.zw/api/endpoint/

# Should succeed with valid token
curl -X POST https://api.slykertech.co.zw/api/endpoint/ \
     -H "X-CSRFToken: your-token" \
     -H "Referer: https://slykertech.co.zw"
```

### 4. Verify SSL Certificates
```bash
# Check certificate
sudo certbot certificates

# Test HTTPS
curl -I https://api.slykertech.co.zw/health/
```

## Rollback Instructions

If you need to revert these changes:

1. **Restore original settings.py**:
   ```bash
   git checkout origin/main -- backend/config/settings.py
   ```

2. **Restore original .env.example**:
   ```bash
   git checkout origin/main -- backend/.env.example
   git checkout origin/main -- .env.example
   ```

3. **Update your .env file** to remove new variables

4. **Restart services**:
   ```bash
   sudo systemctl restart gunicorn
   sudo systemctl reload nginx
   ```

## Troubleshooting

### CSRF Verification Failed
- Ensure the `Referer` header matches a domain in `CSRF_TRUSTED_ORIGINS`
- Check that the request includes a valid CSRF token
- Verify HTTPS is being used (CSRF cookies are secure in production)

### CORS Errors in Browser
- Verify the origin is listed in `CORS_ALLOWED_ORIGINS`
- Check that `CORS_ALLOW_CREDENTIALS = True` is set
- Ensure Nginx is properly forwarding headers

### SSL Certificate Issues
- Refer to `CERTBOT_INSTRUCTIONS.md` for detailed troubleshooting
- Check `/var/log/letsencrypt/letsencrypt.log`
- Verify DNS is pointing to your server

## Additional Resources

- **Django CSRF Protection**: https://docs.djangoproject.com/en/stable/ref/csrf/
- **Django CORS Headers**: https://pypi.org/project/django-cors-headers/
- **Let's Encrypt Documentation**: https://letsencrypt.org/docs/
- **Certbot Documentation**: https://certbot.eff.org/docs/

## Support

For issues or questions:
- Review `DEPLOYMENT_GUIDE.md` for deployment procedures
- Check `CERTBOT_INSTRUCTIONS.md` for SSL certificate management
- Contact: support@slykertech.co.zw
- GitHub Issues: https://github.com/morebnyemba/slykertech/issues
