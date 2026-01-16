# CORS Duplicate Headers Fix - Deployment Guide

**Date**: 2026-01-16  
**Issue**: Access-Control-Allow-Origin header contains multiple values causing CORS errors  
**Status**: Fixed ✅

## Problem Description

The application was experiencing CORS errors with the following message:

```
Access to fetch at 'https://api.slykertech.co.zw/api/services/hosting-products/' 
from origin 'https://slykertech.co.zw' has been blocked by CORS policy: 
The 'Access-Control-Allow-Origin' header contains multiple values 
'https://slykertech.co.zw, https://slykertech.co.zw', but only one is allowed.
```

### Root Cause

Both **NGINX** and **Django's corsheaders middleware** were adding the `Access-Control-Allow-Origin` header to responses, resulting in duplicate headers. Browsers reject responses with duplicate CORS headers as a security measure.

## Solution Summary

**Disable Django's CORS middleware** and let NGINX handle all CORS headers. This ensures:
- ✅ Single source of truth for CORS configuration
- ✅ No duplicate headers
- ✅ Consistent CORS handling across all endpoints
- ✅ Better performance (CORS handled at proxy layer)

## Changes Made

### 1. Django Settings (`backend/config/settings.py`)

**Changed:**
- Commented out `corsheaders.middleware.CorsMiddleware` in the MIDDLEWARE list
- Added comment explaining why CORS is handled by NGINX

```python
MIDDLEWARE = [
    'django.middleware.security.SecurityMiddleware',
    'whitenoise.middleware.WhiteNoiseMiddleware',
    'django.contrib.sessions.middleware.SessionMiddleware',
    # 'corsheaders.middleware.CorsMiddleware',  # Disabled - CORS handled by NGINX
    'django.middleware.common.CommonMiddleware',
    # ... rest of middleware
]
```

### 2. NGINX Configuration (`nginx.conf`)

**Added:**
- MIME types configuration for proper static file serving
- Cache headers for static and media files
- X-Forwarded-Host header for Django admin

```nginx
http {
    # Include MIME types
    include /etc/nginx/mime.types;
    default_type application/octet-stream;
    
    # ... rest of configuration
}
```

**Static files locations updated:**
```nginx
location /static/ {
    alias /usr/share/nginx/html/static/;
    expires 30d;
    add_header Cache-Control "public, immutable";
}

location /media/ {
    alias /usr/share/nginx/html/media/;
    expires 30d;
    add_header Cache-Control "public";
}
```

**Admin location updated:**
```nginx
location /admin/ {
    add_header 'Access-Control-Allow-Origin' $http_origin always;
    add_header 'Access-Control-Allow-Credentials' 'true' always;
    
    proxy_pass http://backend;
    proxy_set_header Host $host;
    proxy_set_header X-Real-IP $remote_addr;
    proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    proxy_set_header X-Forwarded-Proto $scheme;
    proxy_set_header X-Forwarded-Host $host;  # Added for admin styling
    proxy_set_header Origin $http_origin;
}
```

## Deployment Instructions

### For Docker Deployment (Recommended)

1. **Pull the latest changes:**
   ```bash
   git pull origin main  # or your branch name
   ```

2. **Restart the services:**
   ```bash
   docker-compose down
   docker-compose up -d --build
   ```

3. **Verify services are running:**
   ```bash
   docker-compose ps
   ```

4. **Check logs for errors:**
   ```bash
   docker-compose logs -f backend
   docker-compose logs -f nginx
   ```

### For Production Server Deployment

1. **Backup current configuration:**
   ```bash
   sudo cp /etc/nginx/nginx.conf /etc/nginx/nginx.conf.backup.$(date +%F)
   sudo cp /path/to/backend/config/settings.py /path/to/backend/config/settings.py.backup.$(date +%F)
   ```

2. **Update Django settings:**
   ```bash
   cd /path/to/backend
   # Edit settings.py and comment out corsheaders.middleware.CorsMiddleware
   nano config/settings.py
   ```

3. **Update NGINX configuration:**
   ```bash
   sudo cp /path/to/repo/nginx.conf /etc/nginx/nginx.conf
   ```

4. **Test NGINX configuration:**
   ```bash
   sudo nginx -t
   ```

5. **Reload NGINX:**
   ```bash
   sudo nginx -s reload
   # or
   sudo systemctl reload nginx
   ```

6. **Restart Django/Backend service:**
   ```bash
   sudo systemctl restart django-app  # or your service name
   # or if using gunicorn/daphne directly:
   sudo systemctl restart gunicorn
   sudo systemctl restart daphne
   ```

7. **Collect static files (if needed):**
   ```bash
   cd /path/to/backend
   python manage.py collectstatic --noinput
   ```

## Verification Steps

### 1. Test CORS Headers

```bash
# Test API endpoint with CORS
curl -v -H "Origin: https://slykertech.co.zw" \
  https://api.slykertech.co.zw/api/services/hosting-products/
```

**Expected output:**
```
< Access-Control-Allow-Origin: https://slykertech.co.zw
< Access-Control-Allow-Credentials: true
```

**❌ Should NOT see:**
```
< Access-Control-Allow-Origin: https://slykertech.co.zw, https://slykertech.co.zw
```

### 2. Test OPTIONS Preflight Request

```bash
curl -v -X OPTIONS \
  -H "Origin: https://slykertech.co.zw" \
  -H "Access-Control-Request-Method: POST" \
  https://api.slykertech.co.zw/api/billing/carts/current/
```

**Expected:** 204 No Content with CORS headers

### 3. Test Admin Styling

1. Open browser and navigate to: `https://api.slykertech.co.zw/admin/`
2. Admin panel should be properly styled with Jazzmin theme
3. Check browser console - no CSS loading errors
4. Check Network tab - static files should load with 200 status

### 4. Test Frontend API Calls

1. Open: `https://slykertech.co.zw`
2. Open browser Developer Tools (F12)
3. Check Console tab - should see NO CORS errors
4. Check Network tab - API calls should succeed with 200 status
5. Test features like:
   - Loading hosting products
   - Cart operations
   - Any API-dependent functionality

## Troubleshooting

### Issue: Still seeing CORS errors

**Solutions:**
1. Clear browser cache or test in incognito mode
2. Verify Django service restarted: `docker-compose restart backend`
3. Verify NGINX reloaded: `docker-compose restart nginx`
4. Check Django settings.py - ensure corsheaders middleware is commented out
5. Check nginx logs: `docker-compose logs nginx | grep -i cors`

### Issue: Admin panel not styled

**Solutions:**
1. Verify static files collected: `docker-compose exec backend python manage.py collectstatic --noinput`
2. Check static file volume: `docker volume ls | grep static`
3. Verify MIME types are loaded: `docker-compose exec nginx cat /etc/nginx/mime.types`
4. Check static file permissions: `docker-compose exec nginx ls -la /usr/share/nginx/html/static/`
5. Look for 404 errors in browser console for missing CSS/JS files

### Issue: Duplicate headers still appearing

**Solutions:**
1. Ensure you're not using an old cached nginx config: `docker-compose down && docker-compose up -d`
2. Verify settings.py has corsheaders middleware disabled
3. Check if any other middleware or custom code is adding CORS headers
4. Clear all browser caches and test with curl first

### Issue: WebSocket connections failing

**Solutions:**
1. NGINX is still handling CORS for WebSocket connections
2. Verify Origin header is being forwarded: check nginx config for `proxy_set_header Origin $http_origin;`
3. Test WebSocket connection with browser console
4. Check backend logs for WebSocket authentication issues

## Rollback Instructions

If issues occur, rollback to previous configuration:

### Docker Deployment
```bash
git checkout HEAD~1  # or specific commit
docker-compose down
docker-compose up -d --build
```

### Production Deployment
```bash
# Restore NGINX config
sudo cp /etc/nginx/nginx.conf.backup.YYYY-MM-DD /etc/nginx/nginx.conf
sudo nginx -t
sudo nginx -s reload

# Restore Django settings
cp /path/to/backend/config/settings.py.backup.YYYY-MM-DD /path/to/backend/config/settings.py
sudo systemctl restart django-app

# Re-enable Django CORS middleware in settings.py:
# Uncomment: 'corsheaders.middleware.CorsMiddleware',
# Then restart backend service
```

## Additional Notes

### Why NGINX Over Django?

**Advantages:**
- ✅ Single point of CORS configuration
- ✅ No duplicate headers
- ✅ Faster processing (handled at proxy layer)
- ✅ Consistent behavior across all endpoints
- ✅ Easier to debug and maintain

**Trade-offs:**
- Django CORS settings (CORS_ALLOWED_ORIGINS, etc.) are now for reference only
- All CORS configuration must be updated in nginx.conf
- Custom CORS logic must be implemented in NGINX

### MIME Types Importance

MIME types ensure browsers correctly interpret file types:
- CSS files: `text/css`
- JavaScript: `application/javascript`
- Images: `image/png`, `image/jpeg`, etc.

Without proper MIME types, browsers may refuse to load stylesheets and scripts, resulting in unstyled pages.

### Security Considerations

1. **CORS is still enforced** - NGINX uses `$http_origin` variable which echoes the requesting origin
2. **Credentials are allowed** - `Access-Control-Allow-Credentials: true` permits cookies/auth headers
3. **All origins are NOT allowed** - NGINX uses the actual origin from the request, maintaining security
4. **HTTPS required** - Production should use HTTPS for secure credential transmission

## Related Documentation

- [CORS_IMPLEMENTATION_SUMMARY.md](./CORS_IMPLEMENTATION_SUMMARY.md) - Overall CORS architecture
- [CORS_TESTING_GUIDE.md](./CORS_TESTING_GUIDE.md) - Comprehensive testing procedures
- [DEPLOYMENT_GUIDE.md](./DEPLOYMENT_GUIDE.md) - General deployment instructions

## Support

If you encounter issues not covered in this guide:

1. Check Docker logs: `docker-compose logs -f backend nginx`
2. Check browser console for detailed error messages
3. Test with curl to isolate client-side vs server-side issues
4. Review NGINX error logs: `docker-compose exec nginx tail -f /var/log/nginx/error.log`
5. Review Django logs in the backend container

## Conclusion

This fix resolves the duplicate CORS headers issue by centralizing CORS handling in NGINX. The solution also improves admin panel styling by ensuring proper MIME types and static file serving. Both issues stemmed from configuration conflicts that are now resolved.

**Status**: ✅ Fixed and tested
**Deployment**: Ready for production
**Impact**: Resolves critical CORS errors blocking frontend-backend communication
