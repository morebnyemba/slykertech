# CORS Fix - Quick Reference & Troubleshooting

## Quick Deployment Steps

1. **Pull latest changes:**
   ```bash
   git pull origin copilot/fix-cors-policy-issue
   ```

2. **Restart NGINX:**
   ```bash
   # If using Docker Compose
   docker-compose restart nginx
   
   # If using system NGINX
   sudo nginx -t && sudo systemctl reload nginx
   ```

3. **Test CORS headers:**
   ```bash
   ./test-cors.sh
   ```

## What Was Changed

- **NGINX Configuration (`nginx.conf`)**: Added CORS headers at reverse proxy level
  - Handles OPTIONS preflight requests (returns 204)
  - Adds CORS headers to ALL responses (including errors) using `always` flag
  - Applied to: `/api/`, `/admin/`, `/ws/`, `/ws/chat/`, `/health/`

## Expected Behavior After Fix

✅ **Before (Problem):**
- CORS error: "No 'Access-Control-Allow-Origin' header is present"
- WebSocket connection failures
- 400/404 errors blocked by browser

✅ **After (Fixed):**
- All API responses include CORS headers (including errors)
- OPTIONS preflight requests succeed with 204 status
- WebSocket connections establish successfully
- Error responses (400, 401, 404) are visible to frontend

## Quick Tests

### 1. Test CORS on API endpoint
```bash
curl -v -X OPTIONS \
  -H "Origin: https://slykertech.co.zw" \
  https://api.slykertech.co.zw/api/billing/carts/current/ 2>&1 | grep -i "access-control"
```
**Expected:** Should see `Access-Control-Allow-Origin: https://slykertech.co.zw`

### 2. Test CORS on error response
```bash
curl -v -X GET \
  -H "Origin: https://slykertech.co.zw" \
  https://api.slykertech.co.zw/api/nonexistent/ 2>&1 | grep -i "access-control"
```
**Expected:** Should see CORS headers even on 404 error

### 3. Test from browser console
```javascript
// Test API endpoint
fetch('https://api.slykertech.co.zw/api/billing/carts/current/', {
  credentials: 'include',
  headers: { 'Content-Type': 'application/json' }
})
.then(r => console.log('Success:', r.status))
.catch(e => console.error('Error:', e));

// Test WebSocket
const ws = new WebSocket('wss://api.slykertech.co.zw/ws/chat/sales/');
ws.onopen = () => console.log('✓ WebSocket connected');
ws.onerror = (e) => console.error('✗ WebSocket error:', e);
```

## Common Issues & Solutions

### Issue 1: Still seeing CORS errors
**Solution:**
1. Clear browser cache (Ctrl+Shift+Delete)
2. Hard refresh (Ctrl+Shift+R or Cmd+Shift+R)
3. Check if NGINX was restarted: `docker-compose ps nginx`
4. Verify NGINX config is mounted: `docker-compose exec nginx cat /etc/nginx/nginx.conf | grep "Access-Control"`

### Issue 2: WebSocket connections fail
**Possible causes:**
1. SSL certificate issues - verify cert is valid for WSS
2. Redis not running - check: `docker-compose ps redis`
3. Backend not running - check: `docker-compose ps backend`
4. Wrong WebSocket path - verify URL matches routing patterns

**Debug:**
```bash
# Check backend logs
docker-compose logs backend --tail=100

# Check Redis connection
docker-compose exec redis redis-cli ping

# Test WebSocket from command line
wscat -c wss://api.slykertech.co.zw/ws/analytics/ -H "Origin: https://slykertech.co.zw"
```

### Issue 3: 400 Bad Request on cart endpoint
**Possible causes:**
1. Session cookie not being set (should be fixed by CORS headers)
2. Missing CSRF token for POST requests
3. Invalid request data

**Debug:**
```bash
# Check if session cookie is being set
curl -v -X GET \
  -H "Origin: https://slykertech.co.zw" \
  https://api.slykertech.co.zw/api/billing/carts/current/ 2>&1 | grep -i "set-cookie"

# Check backend logs for detailed error
docker-compose logs backend --tail=50 | grep -A 5 "billing/carts"
```

### Issue 4: Daphne not starting
**Error:** `Traceback at /usr/local/bin/daphne`

**Solution:**
```bash
# Check if all dependencies are installed
docker-compose exec backend pip list | grep -E "daphne|channels|django"

# Rebuild backend container
docker-compose build backend
docker-compose up -d backend

# Check for Python errors in ASGI config
docker-compose exec backend python -c "from config.asgi import application; print('OK')"
```

## Verification Checklist

After deployment, verify:
- [ ] NGINX container is running: `docker-compose ps nginx`
- [ ] Backend container is running: `docker-compose ps backend`
- [ ] OPTIONS requests return 204 with CORS headers
- [ ] GET requests return CORS headers (test with curl)
- [ ] Error responses (404) include CORS headers
- [ ] WebSocket connections establish successfully
- [ ] Frontend application loads without CORS errors
- [ ] Cart functionality works from frontend
- [ ] No errors in browser console

## Logs to Check

```bash
# NGINX access logs (see CORS requests)
docker-compose logs nginx --tail=100

# Backend application logs
docker-compose logs backend --tail=100

# Browser console (F12)
# Check for:
# - CORS errors (should be gone)
# - WebSocket connection status
# - API request/response
```

## Rollback Plan

If issues persist, rollback:
```bash
git revert HEAD
docker-compose restart nginx
```

Or revert to previous nginx.conf from backup.

## Support

If issues persist after following this guide:
1. Run `./test-cors.sh` and save output
2. Check all logs (nginx, backend, browser console)
3. Provide specific error messages and steps to reproduce
4. Check if issue occurs with different browsers
5. Verify environment variables in `.env` file match expected values

## Technical Details

**NGINX CORS Configuration:**
- Uses `$http_origin` variable to reflect actual origin (required for credentials)
- `always` flag ensures headers are added to error responses
- OPTIONS requests handled at NGINX level (faster, no backend hit)
- Compatible with Django's CORS middleware

**Session Configuration:**
- `SESSION_COOKIE_SAMESITE = 'None'` (production)
- `SESSION_COOKIE_SECURE = True` (production, requires HTTPS)
- `CORS_ALLOW_CREDENTIALS = True` (allows cookies in CORS requests)

**WebSocket Configuration:**
- Custom `CorsOriginValidator` in `asgi.py`
- Validates origin against `CORS_ALLOWED_ORIGINS`
- NGINX adds CORS headers to WebSocket upgrade requests
