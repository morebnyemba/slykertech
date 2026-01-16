# CORS and WebSocket Fix - Deployment Guide

## Overview

This guide covers the deployment of fixes for CORS errors and WebSocket connection failures between the frontend (`https://slykertech.co.zw`) and backend (`https://api.slykertech.co.zw`).

## Issues Fixed

### 1. CORS Errors
**Problem**: API requests from frontend to backend were blocked with "No 'Access-Control-Allow-Origin' header" error.

**Root Causes**:
- Environment variables with quoted values were not parsed correctly
- Missing CORS environment variables in docker-compose.yml

**Solutions**:
- Added robust `parse_comma_separated()` function to handle both quoted and unquoted environment variables
- Updated `.env.example` to use unquoted format for clarity
- Added CORS_ALLOWED_ORIGINS and CSRF_TRUSTED_ORIGINS to docker-compose.yml

### 2. WebSocket Connection Failures
**Problem**: WebSocket connection to `wss://api.slykertech.co.zw/ws/chat/sales/` failed.

**Root Causes**:
- Erlang livechat service didn't validate CORS origins
- WebSocket routing didn't support `/ws/chat/sales/` path pattern

**Solutions**:
- Added origin validation in Erlang WebSocket handler
- Updated routing to support `/ws/chat/:room` and `/ws/[...]` patterns
- Added CORS preflight handler for OPTIONS requests

## Deployment Steps

### 1. Update Environment Variables

Create or update your `.env` file in the project root with the following (unquoted):

```bash
# Django Debug (set to False in production)
DEBUG=False

# Allowed hosts (comma-separated, no spaces around commas for best compatibility)
ALLOWED_HOSTS=localhost,127.0.0.1,api.slykertech.co.zw,slykertech.co.zw,www.slykertech.co.zw

# CORS Configuration (comma-separated, no quotes)
CORS_ALLOWED_ORIGINS=https://slykertech.co.zw,https://www.slykertech.co.zw

# CSRF Configuration (comma-separated, no quotes)
CSRF_TRUSTED_ORIGINS=https://slykertech.co.zw,https://www.slykertech.co.zw,https://api.slykertech.co.zw

# API URLs for frontend
NEXT_PUBLIC_API_URL=https://api.slykertech.co.zw/api
NEXT_PUBLIC_WS_URL=wss://api.slykertech.co.zw/ws
NEXT_PUBLIC_SITE_URL=https://slykertech.co.zw

# Database password
DB_PASSWORD=your_secure_password_here

# Node environment
NODE_ENV=production
```

### 2. Rebuild and Restart Services

```bash
# Stop all services
docker-compose down

# Rebuild images (to pick up Erlang changes)
docker-compose build --no-cache livechat backend

# Start all services
docker-compose up -d

# Check service status
docker-compose ps

# Check logs for any errors
docker-compose logs -f backend
docker-compose logs -f livechat
docker-compose logs -f nginx
```

### 3. Verify Nginx Configuration

Ensure nginx is using the updated configuration:

```bash
# Check nginx config is valid
docker-compose exec nginx nginx -t

# Reload nginx (if config was valid)
docker-compose exec nginx nginx -s reload
```

### 4. Test CORS Configuration

Test that CORS headers are being sent correctly:

```bash
# Test API endpoint
curl -v \
  -H "Origin: https://slykertech.co.zw" \
  https://api.slykertech.co.zw/api/services/

# Expected response headers:
# Access-Control-Allow-Origin: https://slykertech.co.zw
# Access-Control-Allow-Credentials: true

# Test preflight request
curl -v \
  -X OPTIONS \
  -H "Origin: https://slykertech.co.zw" \
  -H "Access-Control-Request-Method: GET" \
  https://api.slykertech.co.zw/api/billing/carts/current/

# Expected: 200 OK with CORS headers
```

### 5. Test WebSocket Connection

Test WebSocket connectivity:

```bash
# Install wscat if not already installed
npm install -g wscat

# Test WebSocket connection
wscat -c "wss://api.slykertech.co.zw/ws/chat/sales/" \
  -H "Origin: https://slykertech.co.zw"

# Expected: Connection established
# If you get "403 Forbidden", check the origin is in the allowed list
```

### 6. Browser Testing

Open browser developer console and navigate to `https://slykertech.co.zw`:

1. **Check Console for CORS Errors**: Should see no CORS-related errors
2. **Check Network Tab**: API requests should have CORS headers
3. **Check WebSocket**: WebSocket connection should establish successfully

## Troubleshooting

### CORS Errors Still Appear

1. **Check environment variables are loaded**:
   ```bash
   docker-compose exec backend python manage.py shell -c "
   from django.conf import settings
   print('DEBUG:', settings.DEBUG)
   print('CORS_ALLOWED_ORIGINS:', settings.CORS_ALLOWED_ORIGINS)
   print('CSRF_TRUSTED_ORIGINS:', settings.CSRF_TRUSTED_ORIGINS)
   "
   ```

2. **Check for quoted values**: Ensure `.env` file doesn't have quotes around comma-separated values

3. **Restart backend service**:
   ```bash
   docker-compose restart backend
   ```

### WebSocket Connection Fails

1. **Check livechat service is running**:
   ```bash
   docker-compose ps livechat
   docker-compose logs livechat
   ```

2. **Test health endpoint**:
   ```bash
   curl http://localhost:4001/health
   # or through nginx
   curl https://api.slykertech.co.zw/health
   ```

3. **Rebuild livechat service**:
   ```bash
   docker-compose build --no-cache livechat
   docker-compose up -d livechat
   ```

### 400 Bad Request Errors

If you see 400 errors before CORS errors:

1. **Check request payload**: Ensure the request body is valid JSON
2. **Check authentication**: Some endpoints require authentication
3. **Check Django logs**: `docker-compose logs backend | grep ERROR`
4. **Check for CSRF issues**: For POST/PUT/DELETE requests, ensure CSRF token is included

## Files Changed

### Backend Configuration
- `backend/config/settings.py`: Added `parse_comma_separated()` helper function for robust environment variable parsing
- `.env.example`: Updated to show unquoted format for better compatibility

### Erlang LiveChat Service
- `livechat_erlang/src/cors_config.erl`: **NEW** - Shared CORS configuration module to avoid duplication
- `livechat_erlang/src/chat_websocket_handler.erl`: Added origin validation using shared config
- `livechat_erlang/src/livechat_app.erl`: Updated routing to `/ws/chat/[...]` for better specificity
- `livechat_erlang/src/cors_handler.erl`: **NEW** - CORS preflight handler for OPTIONS requests

### Docker Configuration
- `docker-compose.yml`: Added CORS_ALLOWED_ORIGINS and CSRF_TRUSTED_ORIGINS environment variables to backend service

## Security Notes

### Production Configuration
- Always set `DEBUG=False` in production
- Use specific origins in `CORS_ALLOWED_ORIGINS` (no wildcards)
- Ensure SSL certificates are valid and up-to-date
- Use strong database passwords

### Development Configuration
- Can set `DEBUG=True` for local development
- Can use `CORS_ALLOW_ALL_ORIGINS=True` for local development only
- Localhost origins are included by default

## Monitoring

After deployment, monitor:

1. **Backend logs**: `docker-compose logs -f backend`
2. **Nginx logs**: `docker-compose logs -f nginx`
3. **Browser console**: Check for any JavaScript errors
4. **Application functionality**: Test login, cart operations, and WebSocket features

## Rollback Plan

If issues occur after deployment:

```bash
# Revert to previous version
git revert HEAD
docker-compose down
docker-compose build --no-cache
docker-compose up -d

# Or checkout previous commit
git log --oneline  # Find previous commit hash
git checkout <previous-commit-hash>
docker-compose down
docker-compose build --no-cache
docker-compose up -d
```

## Support

For issues or questions:
1. Check logs: `docker-compose logs <service-name>`
2. Review this guide's troubleshooting section
3. Check CORS_TESTING_GUIDE.md for detailed testing procedures
4. Review Django and Erlang logs for specific error messages

## Next Steps

After successful deployment:
1. Monitor application logs for 24 hours
2. Verify all features work correctly (login, cart, chat, etc.)
3. Run load tests to ensure WebSocket connections scale properly
4. Update monitoring dashboards to track CORS errors
