# CORS Fix - Quick Deployment Guide

## What Was Fixed

The CORS errors were caused by Nginx not forwarding the `Origin` header to Django. This prevented Django's CORS middleware from functioning properly.

**Solution**: Configure Nginx to forward the Origin header, and let Django handle all CORS logic.

## Files Changed

1. ✅ `nginx.conf` - Added Origin header forwarding to all backend proxy locations
2. ✅ `.env.example` - Added CORS and CSRF environment variables
3. ✅ `CORS_TESTING_GUIDE.md` - Comprehensive testing guide
4. ✅ `CORS_IMPLEMENTATION_SUMMARY.md` - Architecture documentation

## Deployment Steps

### 1. Update Nginx Configuration

```bash
# On your server, copy the new nginx.conf
sudo cp /path/to/repo/nginx.conf /etc/nginx/nginx.conf

# Or if using Docker, rebuild the nginx container
docker-compose up -d --build nginx

# Test the configuration
sudo nginx -t

# If test passes, reload nginx
sudo nginx -s reload
```

### 2. Update Backend Environment Variables

Add these variables to your backend `.env` file:

```env
# Set to False in production
DEBUG=False

# CORS - Allow frontend origins
CORS_ALLOWED_ORIGINS="https://slykertech.co.zw,https://www.slykertech.co.zw"

# CSRF - Trust these origins for form submissions
CSRF_TRUSTED_ORIGINS="https://slykertech.co.zw,https://www.slykertech.co.zw,https://api.slykertech.co.zw"

# Existing setting (verify it's correct)
ALLOWED_HOSTS=localhost,127.0.0.1,api.slykertech.co.zw,slykertech.co.zw,www.slykertech.co.zw
```

### 3. Restart Backend Service

```bash
# If using systemd
sudo systemctl restart django-app

# Or if using Docker
docker-compose restart backend

# Or if using Daphne directly
pkill daphne
daphne -b 0.0.0.0 -p 8000 config.asgi:application
```

### 4. Verify the Fix

#### Quick Test with curl:

```bash
# Test that CORS headers are present
curl -v \
  -H "Origin: https://slykertech.co.zw" \
  https://api.slykertech.co.zw/api/services/

# Look for these headers in the response:
# Access-Control-Allow-Origin: https://slykertech.co.zw
# Access-Control-Allow-Credentials: true
```

#### Test in Browser:

1. Open `https://slykertech.co.zw`
2. Open Browser DevTools (F12) → Console
3. Check for CORS errors
   - ✅ **Good**: No CORS errors
   - ❌ **Bad**: "blocked by CORS policy" errors
4. Go to Network tab
5. Make an API request (navigate through the site)
6. Click on an API request
7. Check Response Headers for CORS headers

## What Changed in nginx.conf

### Before (❌ Problematic):
```nginx
location /api/ {
    proxy_pass http://backend;
    proxy_set_header Host $host;
    # Missing: Origin header forwarding
}
```

### After (✅ Fixed):
```nginx
location /api/ {
    proxy_pass http://backend;
    proxy_set_header Host $host;
    proxy_set_header X-Real-IP $remote_addr;
    proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    proxy_set_header X-Forwarded-Proto $scheme;
    proxy_set_header X-Forwarded-Host $host;
    # ✅ Added: Forward Origin header so Django can handle CORS
    proxy_set_header Origin $http_origin;
}
```

## How It Works Now

```
┌─────────┐                 ┌───────┐                 ┌────────┐
│ Browser │ ─── Request ──→ │ Nginx │ ─── Forward ──→ │ Django │
│         │                 │       │    + Origin     │        │
│         │ ←── Response ── │       │ ← + CORS hdrs ─ │  CORS  │
└─────────┘                 └───────┘                 │  MW    │
                                                      └────────┘
```

1. Browser sends request with `Origin: https://slykertech.co.zw` header
2. Nginx forwards the request to Django, including the Origin header
3. Django's CORS middleware checks if origin is in `CORS_ALLOWED_ORIGINS`
4. Django adds appropriate CORS headers to the response
5. Nginx forwards the response (with CORS headers) back to browser
6. Browser sees CORS headers and allows the request

## Troubleshooting

### Issue: Still seeing CORS errors after deployment

**Check:**
```bash
# 1. Verify nginx was reloaded
sudo nginx -s reload

# 2. Verify backend was restarted
sudo systemctl status django-app

# 3. Check Django is receiving Origin header
# Look for Origin header in Django logs

# 4. Test with curl
curl -v -H "Origin: https://slykertech.co.zw" \
  https://api.slykertech.co.zw/api/services/
```

### Issue: OPTIONS requests failing

This might indicate:
- Django CORS middleware not configured properly (check `backend/config/settings.py`)
- CORS middleware not before CommonMiddleware in MIDDLEWARE list
- Environment variables not loaded (restart backend)

**Verify:**
```bash
# Check middleware order
grep -A 15 "MIDDLEWARE = \[" backend/config/settings.py
```

### Issue: Some origins work, others don't

**Check:**
```bash
# View current CORS_ALLOWED_ORIGINS setting
grep CORS_ALLOWED_ORIGINS backend/.env

# Should include both with and without www:
# CORS_ALLOWED_ORIGINS=https://slykertech.co.zw,https://www.slykertech.co.zw
```

## Testing Checklist

After deployment, verify:

- [ ] Nginx configuration reloaded without errors
- [ ] Backend service restarted
- [ ] Environment variables updated
- [ ] CORS headers present in API responses (use curl)
- [ ] Frontend can make API requests without errors
- [ ] No CORS errors in browser console
- [ ] WebSocket connections work (if applicable)
- [ ] Can login/logout successfully
- [ ] Session/cookies work correctly

## Security Notes

✅ **Production Configuration:**
- `DEBUG=False` - Debug mode off
- `CORS_ALLOW_ALL_ORIGINS=False` - Specific origins only
- HTTPS enforced
- Explicit origin whitelist

❌ **Never in Production:**
- `DEBUG=True`
- `CORS_ALLOW_ALL_ORIGINS=True`
- HTTP without HTTPS
- Wildcard origins (`*`)

## Need More Information?

See detailed documentation:
- [CORS_TESTING_GUIDE.md](./CORS_TESTING_GUIDE.md) - Testing procedures
- [CORS_IMPLEMENTATION_SUMMARY.md](./CORS_IMPLEMENTATION_SUMMARY.md) - Architecture details
- [CORS_FIX_DOCUMENTATION.md](./CORS_FIX_DOCUMENTATION.md) - Original documentation

## Quick Commands Reference

```bash
# Test CORS with curl
curl -v -H "Origin: https://slykertech.co.zw" \
  https://api.slykertech.co.zw/api/services/

# Reload nginx
sudo nginx -s reload

# Restart backend (systemd)
sudo systemctl restart django-app

# Restart backend (Docker)
docker-compose restart backend

# View nginx error logs
sudo tail -f /var/log/nginx/error.log

# View nginx access logs
sudo tail -f /var/log/nginx/access.log

# View Django logs
sudo journalctl -u django-app -f
```

## Summary

✅ **What was fixed:**
- Nginx now forwards the Origin header to Django
- Django's CORS middleware can now properly validate origins
- All CORS logic centralized in Django (single source of truth)

✅ **What you need to do:**
1. Deploy updated `nginx.conf`
2. Set environment variables in backend `.env`
3. Restart services
4. Test with curl and browser

✅ **Expected result:**
- No CORS errors in browser console
- API requests work from frontend
- WebSocket connections succeed
- Sessions/cookies work properly
