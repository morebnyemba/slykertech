# CORS Configuration Fix Documentation

## Problem Statement

The application was experiencing CORS (Cross-Origin Resource Sharing) policy errors when making requests from the frontend (`https://slykertech.co.zw`) to the backend API (`https://api.slykertech.co.zw`).

### Error Messages:
- `Access to fetch at 'https://api.slykertech.co.zw/api/billing/carts/current/' from origin 'https://slykertech.co.zw' has been blocked by CORS policy: Response to preflight request doesn't pass access control check: No 'Access-Control-Allow-Origin' header is present on the requested resource.`
- `Failed to load resource: net::ERR_FAILED`
- `WebSocket connection to 'wss://api.slykertech.co.zw/ws/chat/support/' failed`

## Root Causes Identified

1. **WebSocket Origin Validation**: The `AllowedHostsOriginValidator` in ASGI only validated against `ALLOWED_HOSTS`, which didn't properly handle cross-origin WebSocket connections from the frontend domain.

2. **Missing CORS Expose Headers**: The frontend couldn't access certain response headers because they weren't explicitly exposed via `CORS_EXPOSE_HEADERS`.

3. **Origin Header Not Passed for WebSockets**: The nginx proxy wasn't passing the Origin header for WebSocket connections, preventing proper origin validation.

## Changes Made

### Backend Changes

#### 1. Custom WebSocket Origin Validator (`backend/config/asgi.py`)

Created a custom `CorsOriginValidator` that validates WebSocket connections against both `ALLOWED_HOSTS` and `CORS_ALLOWED_ORIGINS`:

```python
class CorsOriginValidator(OriginValidator):
    """
    Custom origin validator that respects CORS_ALLOWED_ORIGINS setting
    for WebSocket connections in addition to ALLOWED_HOSTS
    """
    
    def valid_origin(self, parsed_origin):
        """
        Validate origin against both ALLOWED_HOSTS and CORS_ALLOWED_ORIGINS
        """
        # First check with parent class (uses ALLOWED_HOSTS)
        if super().valid_origin(parsed_origin):
            return True
        
        # In DEBUG mode with CORS_ALLOW_ALL_ORIGINS, allow all
        if settings.DEBUG and getattr(settings, 'CORS_ALLOW_ALL_ORIGINS', False):
            return True
        
        # Check against CORS_ALLOWED_ORIGINS if configured
        if hasattr(settings, 'CORS_ALLOWED_ORIGINS'):
            # Reconstruct the full origin from parsed_origin
            origin = f"{parsed_origin[0]}://{parsed_origin[1]}"
            if parsed_origin[2] is not None:
                origin = f"{origin}:{parsed_origin[2]}"
            
            # Check if origin is in CORS_ALLOWED_ORIGINS
            if origin in settings.CORS_ALLOWED_ORIGINS:
                return True
        
        return False
```

#### 2. Added CORS Expose Headers (`backend/config/settings.py`)

Added `CORS_EXPOSE_HEADERS` to allow the frontend to access specific response headers:

```python
CORS_EXPOSE_HEADERS = [
    'content-type',
    'x-csrftoken',
]
```

### Nginx Configuration Changes (`nginx.conf`)

Updated the WebSocket proxy configuration to pass the Origin header:

```nginx
location /ws/ {
    proxy_pass http://backend;
    proxy_http_version 1.1;
    proxy_set_header Upgrade $http_upgrade;
    proxy_set_header Connection "upgrade";
    proxy_set_header Host $host;
    proxy_set_header X-Real-IP $remote_addr;
    proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    proxy_set_header X-Forwarded-Proto $scheme;
    proxy_set_header Origin $http_origin;  # Added this line
    proxy_read_timeout 86400;
}
```

## How CORS Works

CORS (Cross-Origin Resource Sharing) is a security feature implemented by browsers to prevent malicious websites from making unauthorized requests to your API.

### Preflight Requests

For certain HTTP requests (like POST, PUT, DELETE with custom headers), browsers send a "preflight" OPTIONS request first to check if the actual request is allowed. The server must respond with appropriate CORS headers:

- `Access-Control-Allow-Origin`: Specifies which origins are allowed
- `Access-Control-Allow-Methods`: Specifies which HTTP methods are allowed
- `Access-Control-Allow-Headers`: Specifies which headers are allowed
- `Access-Control-Allow-Credentials`: Indicates if cookies can be included
- `Access-Control-Expose-Headers`: Specifies which response headers can be accessed by the frontend

### WebSocket CORS

WebSockets have a different CORS mechanism:
- During the WebSocket handshake, the browser sends an `Origin` header
- The server must validate this origin before accepting the connection
- Unlike HTTP CORS, there are no preflight OPTIONS requests for WebSockets

### The Fix

The `django-cors-headers` package handles HTTP CORS automatically, but WebSocket validation required custom implementation:

1. **CORS middleware** handles HTTP requests (already properly configured)
2. **Custom origin validator** handles WebSocket connections from allowed origins
3. **Nginx passes Origin header** so the validator can check it properly
4. **Expose headers** allows frontend to read necessary response headers

## Testing the Fix

To verify the CORS configuration is working:

### For HTTP API Requests:

1. **Check Browser Console**: Ensure no CORS errors appear for API requests
2. **Check Network Tab**: Verify that preflight OPTIONS requests return 200 status
3. **Check Response Headers**: Verify the following headers are present:
   - `Access-Control-Allow-Origin: https://slykertech.co.zw`
   - `Access-Control-Allow-Credentials: true`
   - `Access-Control-Allow-Methods: DELETE, GET, OPTIONS, PATCH, POST, PUT`
   - `Access-Control-Expose-Headers: content-type, x-csrftoken`

### For WebSocket Connections:

1. **Check Browser Console**: Ensure WebSocket connections succeed without errors
2. **Check Connection Status**: Verify the connection shows as "open" in browser dev tools
3. **Test Message Sending**: Send and receive messages through the WebSocket

## Environment Variables

Ensure the following environment variables are set in production:

```env
# Backend (.env)
DEBUG=False
CORS_ALLOWED_ORIGINS=https://slykertech.co.zw,https://www.slykertech.co.zw
CSRF_TRUSTED_ORIGINS=https://slykertech.co.zw,https://www.slykertech.co.zw,https://api.slykertech.co.zw
ALLOWED_HOSTS=localhost,127.0.0.1,api.slykertech.co.zw,slykertech.co.zw,www.slykertech.co.zw

# Frontend (.env)
NEXT_PUBLIC_API_URL=https://api.slykertech.co.zw/api
NEXT_PUBLIC_SITE_URL=https://slykertech.co.zw
```

## Security Considerations

1. **SameSite=None**: Required for cross-origin requests but must be used with HTTPS and `Secure` flag
2. **Credentials**: Only enable for trusted origins
3. **Specific Origins**: Never use `CORS_ALLOW_ALL_ORIGINS=True` in production
4. **HTTPS**: Always use HTTPS in production for secure cookie transmission
5. **Origin Validation**: Custom validator checks both ALLOWED_HOSTS and CORS_ALLOWED_ORIGINS

## Additional Notes

- The cart endpoint (`/api/billing/carts/current/`) requires session support, which is why proper cookie configuration is essential
- The login endpoint (`/api/token/`) needs CORS headers to accept credentials from the frontend
- All API endpoints now properly support cross-origin requests from the frontend domain
- WebSocket connections for live chat (`/ws/chat/support/`) now properly validate origins
- The custom origin validator allows for flexible configuration while maintaining security

## Troubleshooting

If CORS issues persist:

1. **Check Django logs**: Look for origin validation failures
2. **Verify environment variables**: Ensure all domains are listed correctly
3. **Check nginx logs**: Verify requests are being proxied properly
4. **Browser dev tools**: Check Network tab for CORS-related error messages
5. **Test with curl**: Verify CORS headers are present in responses:
   ```bash
   curl -H "Origin: https://slykertech.co.zw" -H "Access-Control-Request-Method: GET" \
        -X OPTIONS https://api.slykertech.co.zw/api/billing/carts/current/ -v
   ```

## Deployment Checklist

- [ ] Update environment variables on production server
- [ ] Restart Django/Daphne service
- [ ] Reload nginx configuration: `nginx -t && nginx -s reload`
- [ ] Test API endpoints from frontend
- [ ] Test WebSocket connections from frontend
- [ ] Monitor logs for any CORS-related errors
