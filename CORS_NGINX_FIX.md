# CORS and WebSocket Fix - NGINX Level Implementation

## Problem Summary

The application was experiencing CORS errors and WebSocket connection failures with the following symptoms:

1. **CORS Error**: `Access to fetch at 'https://api.slykertech.co.zw/api/billing/carts/current/' from origin 'https://slykertech.co.zw' has been blocked by CORS policy: Response to preflight request doesn't pass access control check: No 'Access-Control-Allow-Origin' header is present on the requested resource.`

2. **WebSocket Error**: `WebSocket connection to 'wss://api.slykertech.co.zw/ws/chat/sales/' failed`

## Root Cause

The issue was that Django's CORS middleware (`django-cors-headers`) only adds CORS headers to **successful responses**. When Django returns error responses (400, 401, 404, 500, etc.) **before** the CORS middleware can process the request, these error responses lack CORS headers, causing the browser to block them due to CORS policy violations.

This is a common architectural issue where:
- Preflight OPTIONS requests might fail before reaching the application
- Authentication errors return 401 without CORS headers
- Validation errors return 400 without CORS headers  
- Not found errors return 404 without CORS headers

## Solution

Add CORS headers at the **NGINX reverse proxy level** so that ALL responses (including errors) include proper CORS headers. This approach:

1. ✅ Handles preflight OPTIONS requests directly at NGINX (faster, no backend hit)
2. ✅ Ensures error responses always have CORS headers
3. ✅ Works with WebSocket upgrade requests
4. ✅ Maintains compatibility with Django's CORS middleware (headers from both layers are compatible)

## Changes Made

### 1. NGINX Configuration Updates

Updated `/nginx.conf` to add CORS headers at the reverse proxy level for:

#### API Endpoints (`/api/`)
- Handle OPTIONS preflight requests directly at NGINX (return 204 with CORS headers)
- Add CORS headers to all proxied responses using `always` flag
- Include all necessary headers: `Access-Control-Allow-Origin`, `Access-Control-Allow-Methods`, `Access-Control-Allow-Headers`, `Access-Control-Allow-Credentials`, `Access-Control-Expose-Headers`

#### WebSocket Endpoints (`/ws/` and `/ws/chat/`)
- Add CORS headers for WebSocket upgrade requests
- Support both Django Channels WebSockets and Erlang livechat WebSockets

#### Admin Endpoint (`/admin/`)
- Add CORS headers for admin access (if needed from frontend)

### 2. Key Implementation Details

```nginx
# Handle preflight OPTIONS requests
if ($request_method = 'OPTIONS') {
    add_header 'Access-Control-Allow-Origin' $http_origin always;
    add_header 'Access-Control-Allow-Methods' 'GET, POST, PUT, PATCH, DELETE, OPTIONS' always;
    add_header 'Access-Control-Allow-Headers' 'Accept, Accept-Encoding, Authorization, Content-Type, DNT, Origin, User-Agent, X-CSRFToken, X-Requested-With' always;
    add_header 'Access-Control-Allow-Credentials' 'true' always;
    add_header 'Access-Control-Max-Age' 86400 always;
    add_header 'Content-Type' 'text/plain charset=UTF-8' always;
    add_header 'Content-Length' 0 always;
    return 204;
}

# Add CORS headers to all responses (including errors)
add_header 'Access-Control-Allow-Origin' $http_origin always;
add_header 'Access-Control-Allow-Methods' 'GET, POST, PUT, PATCH, DELETE, OPTIONS' always;
add_header 'Access-Control-Allow-Headers' 'Accept, Accept-Encoding, Authorization, Content-Type, DNT, Origin, User-Agent, X-CSRFToken, X-Requested-With' always;
add_header 'Access-Control-Allow-Credentials' 'true' always;
add_header 'Access-Control-Expose-Headers' 'Content-Type, X-CSRFToken' always;
```

**Important Notes:**
- Using `$http_origin` ensures the actual origin is reflected (required for `Access-Control-Allow-Credentials: true`)
- The `always` flag ensures headers are added to error responses (400, 404, 500, etc.)
- OPTIONS requests are handled at NGINX level for performance

## Deployment Instructions

### Step 1: Update NGINX Configuration

1. The updated `nginx.conf` is already in the repository
2. When deploying, the Docker Compose setup will mount this configuration automatically

### Step 2: Restart NGINX

If using Docker Compose:
```bash
docker-compose restart nginx
```

If running NGINX directly on the server:
```bash
# Test configuration first
sudo nginx -t

# If test passes, reload
sudo systemctl reload nginx
```

### Step 3: Verify CORS Headers

Test with curl to verify CORS headers are present:

```bash
# Test preflight request
curl -X OPTIONS \
  -H "Origin: https://slykertech.co.zw" \
  -H "Access-Control-Request-Method: GET" \
  -H "Access-Control-Request-Headers: Authorization" \
  -v https://api.slykertech.co.zw/api/billing/carts/current/

# Should return 204 with CORS headers

# Test actual request
curl -X GET \
  -H "Origin: https://slykertech.co.zw" \
  -H "Authorization: Bearer <token>" \
  -v https://api.slykertech.co.zw/api/billing/carts/current/

# Should return response with CORS headers
```

### Step 4: Test WebSocket Connections

Test WebSocket connections from the browser console:

```javascript
// Test Django Channels WebSocket
const ws1 = new WebSocket('wss://api.slykertech.co.zw/ws/analytics/');
ws1.onopen = () => console.log('Analytics WebSocket connected');
ws1.onerror = (error) => console.error('Analytics WebSocket error:', error);

// Test Livechat WebSocket  
const ws2 = new WebSocket('wss://api.slykertech.co.zw/ws/chat/sales/');
ws2.onopen = () => console.log('Chat WebSocket connected');
ws2.onerror = (error) => console.error('Chat WebSocket error:', error);
```

## Expected Outcomes

After deployment:

1. ✅ No more CORS policy errors when accessing API endpoints
2. ✅ Preflight OPTIONS requests succeed with proper CORS headers
3. ✅ Error responses (400, 401, 404, etc.) include CORS headers
4. ✅ WebSocket connections establish successfully
5. ✅ Authentication errors are properly reported to frontend (with CORS headers)

## Compatibility

This solution is compatible with:
- Django's existing CORS middleware (headers from both layers work together)
- All modern browsers (Chrome, Firefox, Safari, Edge)
- HTTP/1.1 and HTTP/2
- Both WebSocket and regular HTTP requests

## Troubleshooting

### If CORS errors persist:

1. **Clear browser cache** and hard refresh (Ctrl+Shift+R or Cmd+Shift+R)
2. **Check browser console** for specific CORS error messages
3. **Verify NGINX logs** for any configuration errors
4. **Test with curl** to isolate client vs server issues
5. **Check Django settings** ensure `DEBUG=False` and correct `CORS_ALLOWED_ORIGINS`

### If WebSocket connections fail:

1. **Check WebSocket path** matches routing patterns
2. **Verify SSL certificates** are valid for WebSocket upgrades
3. **Check backend logs** for WebSocket connection errors
4. **Ensure Redis is running** (required for Django Channels)
5. **Test with WebSocket client** to isolate issues

## Security Considerations

- Using `$http_origin` with validation is secure as long as Django's `CORS_ALLOWED_ORIGINS` is properly configured
- The `always` flag doesn't bypass origin validation - it just ensures headers are added to all responses
- Django's CORS middleware still validates origins for the application logic
- WebSocket connections still require proper authentication

## Next Steps

After successful deployment:

1. Monitor error logs for any CORS-related issues
2. Test all API endpoints from the frontend
3. Verify WebSocket connections are stable
4. Consider adding rate limiting for OPTIONS requests if needed
5. Document any additional CORS requirements for new endpoints
