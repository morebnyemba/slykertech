# CORS and WebSocket Fix - Deployment and Testing Guide

## Overview
This document describes the fixes applied to resolve CORS and WebSocket connection issues in the Slykertech platform.

## Issues Fixed

### 1. CORS Headers on Error Responses
**Problem**: API error responses (4xx, 5xx) were not including CORS headers, causing frontend to be unable to read error details.

**Solution**:
- Added `EnhancedCorsMiddleware` to ensure CORS headers are present on all responses
- Updated `custom_exception_handler` to explicitly add CORS headers to error responses
- Middleware placement: After `corsheaders.middleware.CorsMiddleware` as a safety net

**Files Modified**:
- `backend/config/cors_middleware.py` (new)
- `backend/config/exception_handler.py`
- `backend/config/settings.py`

### 2. WebSocket Origin Validation
**Problem**: WebSocket connections from `https://slykertech.co.zw` to `wss://api.slykertech.co.zw/ws/chat/sales/` were being rejected due to strict origin validation.

**Solution**:
- Enhanced `CorsOriginValidator` in ASGI configuration to:
  - Check origins with and without port numbers (handles browser inconsistencies)
  - Fall back to ALLOWED_HOSTS for same-domain connections
  - Add debug logging for troubleshooting
- Improved origin matching logic to handle standard ports (80, 443)

**Files Modified**:
- `backend/config/asgi.py`

### 3. Cart Endpoint Session Management
**Problem**: The `/api/billing/carts/current/` endpoint could fail if session creation failed or returned None.

**Solution**:
- Added explicit `session.save()` after session creation
- Added comprehensive error handling and logging
- Added meaningful error messages for session creation failures
- Better handling of edge cases (no session_id, missing client, etc.)

**Files Modified**:
- `backend/billing/views.py`

## Deployment Steps

### 1. Update Environment Variables
Ensure the following environment variables are set correctly in production:

```bash
# CORS Configuration
CORS_ALLOWED_ORIGINS=https://slykertech.co.zw,https://www.slykertech.co.zw

# CSRF Configuration
CSRF_TRUSTED_ORIGINS=https://slykertech.co.zw,https://www.slykertech.co.zw,https://api.slykertech.co.zw

# Allowed Hosts
ALLOWED_HOSTS=localhost,127.0.0.1,api.slykertech.co.zw,slykertech.co.zw,www.slykertech.co.zw

# Debug (must be False in production)
DEBUG=False
```

### 2. Rebuild and Restart Services

```bash
# Stop current services
docker-compose down

# Rebuild backend service (includes new code)
docker-compose build backend

# Start all services
docker-compose up -d

# Check logs for any errors
docker-compose logs -f backend
```

### 3. Verify Deployment

Check that services are running:
```bash
docker-compose ps
```

All services should show as "Up" or "healthy".

## Testing Guide

### 1. Test CORS Headers on Success Response

```bash
curl -H "Origin: https://slykertech.co.zw" \
     -H "Access-Control-Request-Method: GET" \
     -H "Access-Control-Request-Headers: Content-Type, Authorization" \
     -X OPTIONS \
     https://api.slykertech.co.zw/api/billing/carts/current/

# Expected: 204 No Content with CORS headers
# - Access-Control-Allow-Origin: https://slykertech.co.zw
# - Access-Control-Allow-Credentials: true
# - Access-Control-Allow-Methods: DELETE, GET, OPTIONS, PATCH, POST, PUT
```

### 2. Test CORS Headers on Error Response

```bash
curl -H "Origin: https://slykertech.co.zw" \
     -X GET \
     https://api.slykertech.co.zw/api/billing/carts/99999/

# Expected: 404 Not Found with CORS headers
# - Access-Control-Allow-Origin: https://slykertech.co.zw
# - Access-Control-Allow-Credentials: true
```

### 3. Test Cart Endpoint

```bash
# Test without authentication (should create session cart)
curl -H "Origin: https://slykertech.co.zw" \
     -X GET \
     https://api.slykertech.co.zw/api/billing/carts/current/

# Expected: 200 OK with cart data
```

### 4. Test WebSocket Connection

Open browser console on `https://slykertech.co.zw` and run:

```javascript
// Test sales chat WebSocket
const ws = new WebSocket('wss://api.slykertech.co.zw/ws/chat/sales/');

ws.onopen = () => {
    console.log('✓ WebSocket connected successfully');
    ws.send(JSON.stringify({
        type: 'chat_message',
        message: 'Test message',
        sender: 'Test User'
    }));
};

ws.onerror = (error) => {
    console.error('✗ WebSocket error:', error);
};

ws.onmessage = (event) => {
    console.log('WebSocket message received:', event.data);
};
```

Expected output:
- "✓ WebSocket connected successfully"
- Should receive connection_established message

### 5. Check Backend Logs

```bash
# Check for WebSocket origin validation logs
docker-compose logs backend | grep -i "websocket\|origin"

# Check for cart endpoint logs
docker-compose logs backend | grep -i "cart"

# Check for CORS-related logs
docker-compose logs backend | grep -i "cors"
```

## Troubleshooting

### Issue: CORS headers still missing on error responses

**Check**:
1. Verify `EnhancedCorsMiddleware` is in MIDDLEWARE list after `CorsMiddleware`
2. Check that origin is in CORS_ALLOWED_ORIGINS
3. Check backend logs for CORS middleware messages

**Solution**: Restart backend service after verifying configuration

### Issue: WebSocket connection rejected

**Check**:
1. Check backend logs for "WebSocket origin rejected" messages
2. Verify origin format matches exactly (https:// vs http://, port number)
3. Check that origin is in CORS_ALLOWED_ORIGINS or host is in ALLOWED_HOSTS

**Solution**: 
- Add origin to CORS_ALLOWED_ORIGINS
- Verify nginx is forwarding Origin header correctly

### Issue: Cart endpoint returns 400 or 500

**Check**:
1. Check backend logs for cart-related errors
2. Verify database is accessible
3. Check session configuration

**Solution**:
- If session creation fails, verify cookie settings (SESSION_COOKIE_SECURE, SESSION_COOKIE_SAMESITE)
- Ensure cookies are enabled in browser
- Check that CSRF_COOKIE_DOMAIN and SESSION_COOKIE_DOMAIN are set correctly (or None for same-domain)

## Configuration Verification Checklist

- [ ] CORS_ALLOWED_ORIGINS includes all frontend domains
- [ ] ALLOWED_HOSTS includes all API and frontend domains
- [ ] DEBUG=False in production
- [ ] SESSION_COOKIE_SECURE=True in production (automatically set when DEBUG=False)
- [ ] CSRF_COOKIE_SECURE=True in production (automatically set when DEBUG=False)
- [ ] Nginx is running and proxying correctly
- [ ] Redis is running and accessible (for WebSocket channel layer)
- [ ] PostgreSQL is running and accessible
- [ ] SSL certificates are valid and in place

## Monitoring

### Key Metrics to Monitor

1. **CORS Errors**: Should drop to zero after deployment
2. **WebSocket Connection Success Rate**: Should be >95%
3. **Cart Endpoint Response Time**: Should be <500ms
4. **Session Creation Success Rate**: Should be 100%

### Log Patterns to Watch

**Success Patterns**:
```
INFO: WebSocket origin validated by parent class
INFO: User cart created for client
INFO: Enhanced CORS middleware added headers for origin
```

**Error Patterns to Investigate**:
```
WARNING: WebSocket origin rejected
ERROR: Failed to create session for anonymous user
ERROR: Unable to retrieve cart
```

## Rollback Plan

If issues occur after deployment:

1. Revert to previous Docker image:
```bash
docker-compose down
docker-compose pull backend:previous-tag
docker-compose up -d
```

2. Check git history and revert commits:
```bash
git log --oneline
git revert <commit-hash>
git push
```

3. Rebuild and redeploy:
```bash
docker-compose build backend
docker-compose up -d
```

## Additional Notes

- The nginx configuration already has CORS headers with 'always' directive, so it provides a first layer of CORS support
- Django's corsheaders middleware provides application-level CORS support
- EnhancedCorsMiddleware acts as a safety net for edge cases
- All three layers work together to ensure CORS headers are always present

## Support

If you encounter issues not covered in this guide:
1. Check backend logs: `docker-compose logs backend`
2. Check nginx logs: `docker-compose logs nginx`
3. Enable DEBUG logging temporarily to get more details
4. Review the CORS and WebSocket implementation in the codebase
