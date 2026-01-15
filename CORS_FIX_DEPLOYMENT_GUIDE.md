# CORS Fix Deployment Guide

## Overview

This guide provides step-by-step instructions for deploying the CORS fixes to production. These changes fix CORS errors for API requests and WebSocket connections between the frontend (`https://slykertech.co.zw`) and backend API (`https://api.slykertech.co.zw`).

## Changes Summary

### Backend Changes
1. **Custom WebSocket Origin Validator** - Validates WebSocket connections against CORS_ALLOWED_ORIGINS
2. **CORS Expose Headers** - Exposes response headers to frontend

### Infrastructure Changes
1. **Nginx Configuration** - Passes Origin header for WebSocket connections

## Pre-Deployment Checklist

- [ ] Backup current production configuration files
- [ ] Verify environment variables are set correctly
- [ ] Review all changes in the PR
- [ ] Ensure all services are healthy before deployment

## Deployment Steps

### Step 1: Update Environment Variables

Ensure the following environment variables are set in the backend `.env` file:

```bash
# Backend environment variables
DEBUG=False
ALLOWED_HOSTS=localhost,127.0.0.1,api.slykertech.co.zw,slykertech.co.zw,www.slykertech.co.zw
CORS_ALLOWED_ORIGINS=https://slykertech.co.zw,https://www.slykertech.co.zw
CSRF_TRUSTED_ORIGINS=https://slykertech.co.zw,https://www.slykertech.co.zw,https://api.slykertech.co.zw
```

### Step 2: Deploy Backend Changes

```bash
# Navigate to the project directory
cd /path/to/slykertech

# Pull the latest changes
git pull origin main  # or your deployment branch

# If using Docker:
docker-compose down
docker-compose build backend
docker-compose up -d backend

# If not using Docker:
# Restart Django/Daphne service
sudo systemctl restart slykertech-backend
# or
sudo supervisorctl restart slykertech-backend
```

### Step 3: Update Nginx Configuration

```bash
# Test nginx configuration
sudo nginx -t

# If test passes, reload nginx
sudo nginx -s reload

# If using Docker:
docker-compose restart nginx
```

### Step 4: Verify Services

```bash
# Check backend service status
docker-compose ps backend  # for Docker
# or
sudo systemctl status slykertech-backend

# Check nginx status
sudo systemctl status nginx  # for non-Docker
# or
docker-compose ps nginx  # for Docker

# Check logs for errors
docker-compose logs -f backend  # for Docker
# or
sudo journalctl -u slykertech-backend -f
```

## Post-Deployment Testing

### Test 1: API CORS Headers

Test that API endpoints return proper CORS headers:

```bash
curl -H "Origin: https://slykertech.co.zw" \
     -H "Access-Control-Request-Method: GET" \
     -H "Access-Control-Request-Headers: Authorization, Content-Type" \
     -X OPTIONS \
     https://api.slykertech.co.zw/api/billing/carts/current/ \
     -v
```

Expected response headers:
```
Access-Control-Allow-Origin: https://slykertech.co.zw
Access-Control-Allow-Credentials: true
Access-Control-Allow-Methods: DELETE, GET, OPTIONS, PATCH, POST, PUT
Access-Control-Expose-Headers: content-type, x-csrftoken
```

### Test 2: WebSocket Connection

1. Open the frontend in a browser: `https://slykertech.co.zw`
2. Open browser Developer Tools (F12)
3. Navigate to the Console tab
4. Look for WebSocket connection logs
5. Verify no CORS errors appear
6. Check the Network tab → WS filter to see WebSocket connections

Expected result:
- WebSocket connection to `wss://api.slykertech.co.zw/ws/chat/support/` should succeed
- Status should show as "101 Switching Protocols"
- No CORS errors in console

### Test 3: Frontend API Requests

1. Open the frontend: `https://slykertech.co.zw`
2. Open browser Developer Tools (F12)
3. Navigate to the Network tab
4. Perform actions that trigger API requests (e.g., add to cart, view products)
5. Check that requests succeed without CORS errors

Expected result:
- All API requests should return status 200/201/204 (not 401/403/500)
- No CORS policy errors in console
- Response headers include `Access-Control-Allow-Origin`

## Rollback Procedure

If issues occur after deployment:

### Quick Rollback

```bash
# Revert to previous commit
git revert HEAD~3..HEAD  # Reverts the last 3 commits (adjust as needed)

# Rebuild and restart services
docker-compose down
docker-compose build backend
docker-compose up -d

# Or for non-Docker:
sudo systemctl restart slykertech-backend nginx
```

### Manual Rollback

1. **Backend ASGI Configuration** (`backend/config/asgi.py`):
   - Change `CorsOriginValidator` back to `AllowedHostsOriginValidator`
   - Remove the custom `CorsOriginValidator` class

2. **Backend Settings** (`backend/config/settings.py`):
   - Remove `CORS_EXPOSE_HEADERS` setting

3. **Nginx Configuration** (`nginx.conf`):
   - Remove `proxy_set_header Origin $http_origin;` from WebSocket location block

4. Restart services as described in Step 2 and 3 above

## Monitoring

After deployment, monitor the following:

### Application Logs

```bash
# Backend logs
docker-compose logs -f backend | grep -i "cors\|websocket\|origin"

# Nginx access logs
tail -f /var/log/nginx/access.log | grep -i "options\|ws/"

# Nginx error logs
tail -f /var/log/nginx/error.log
```

### Metrics to Watch

- WebSocket connection success rate
- API request error rates (especially 403/500 errors)
- CORS-related errors in application logs
- Browser console errors reported by users

### Common Issues and Solutions

**Issue 1: CORS errors still appearing**
- Solution: Verify `CORS_ALLOWED_ORIGINS` includes the correct domains (with https://)
- Check nginx is passing requests properly to backend

**Issue 2: WebSocket connections failing**
- Solution: Verify Origin header is being passed by nginx
- Check backend logs for origin validation failures
- Ensure `ALLOWED_HOSTS` includes both frontend and backend domains

**Issue 3: Credentials not being sent**
- Solution: Verify frontend is using `credentials: 'include'` in fetch requests
- Check `CORS_ALLOW_CREDENTIALS = True` in backend settings

## Security Checklist

After deployment, verify:

- [ ] `DEBUG=False` in production environment
- [ ] `CORS_ALLOW_ALL_ORIGINS=False` in production (not set or explicitly False)
- [ ] Only trusted domains listed in `CORS_ALLOWED_ORIGINS`
- [ ] HTTPS is enforced for all connections
- [ ] Session cookies are `Secure` and `HttpOnly` in production
- [ ] No sensitive data exposed in CORS headers

## Support

If issues persist:

1. Check the detailed documentation: [CORS_FIX_DOCUMENTATION.md](./CORS_FIX_DOCUMENTATION.md)
2. Review Django logs for detailed error messages
3. Test CORS headers using browser Developer Tools
4. Verify all environment variables are set correctly

## Success Criteria

Deployment is successful when:

- [ ] No CORS errors in browser console
- [ ] API requests from frontend complete successfully
- [ ] WebSocket connections establish properly
- [ ] All existing functionality works as expected
- [ ] No security vulnerabilities introduced
- [ ] Performance is not degraded

## Contacts

For deployment issues or questions:
- Backend Team: [contact information]
- DevOps Team: [contact information]
- Documentation: See CORS_FIX_DOCUMENTATION.md for technical details
