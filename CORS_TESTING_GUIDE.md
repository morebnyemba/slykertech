# CORS Testing and Verification Guide

This guide provides step-by-step instructions for testing and verifying the CORS (Cross-Origin Resource Sharing) implementation for the Slykertech platform.

## Overview of CORS Implementation

The CORS implementation consists of three layers:

1. **Django CORS Headers (django-cors-headers)** - Handles CORS at the application level
2. **Nginx CORS Headers** - Handles CORS at the reverse proxy level
3. **Custom WebSocket Origin Validator** - Validates WebSocket origins in Django Channels

## Testing CORS with curl

### Test 1: Preflight OPTIONS Request

Preflight requests are sent by browsers before making actual requests with custom headers or certain HTTP methods.

```bash
# Test API endpoint preflight from production frontend
curl -v \
  -H "Origin: https://slykertech.co.zw" \
  -H "Access-Control-Request-Method: POST" \
  -H "Access-Control-Request-Headers: Content-Type,Authorization" \
  -X OPTIONS \
  https://api.slykertech.co.zw/api/billing/carts/current/

# Expected response:
# HTTP/1.1 204 No Content
# Access-Control-Allow-Origin: https://slykertech.co.zw
# Access-Control-Allow-Methods: GET, POST, PUT, PATCH, DELETE, OPTIONS
# Access-Control-Allow-Headers: Accept,Accept-Encoding,Authorization,Content-Type,DNT,Origin,User-Agent,X-CSRFToken,X-Requested-With
# Access-Control-Allow-Credentials: true
# Access-Control-Max-Age: 1728000
```

### Test 2: Actual GET Request

```bash
# Test actual API request from production frontend
curl -v \
  -H "Origin: https://slykertech.co.zw" \
  https://api.slykertech.co.zw/api/services/

# Expected response headers should include:
# Access-Control-Allow-Origin: https://slykertech.co.zw
# Access-Control-Allow-Credentials: true
# Access-Control-Expose-Headers: Content-Type,X-CSRFToken
```

### Test 3: Test with www subdomain

```bash
# Test with www subdomain
curl -v \
  -H "Origin: https://www.slykertech.co.zw" \
  -H "Access-Control-Request-Method: GET" \
  -X OPTIONS \
  https://api.slykertech.co.zw/api/services/

# Expected: Same CORS headers with Origin: https://www.slykertech.co.zw
```

### Test 4: Test with Unauthorized Origin

```bash
# Test with an unauthorized origin (should not return CORS headers)
curl -v \
  -H "Origin: https://malicious-site.com" \
  https://api.slykertech.co.zw/api/services/

# Expected: No Access-Control-Allow-Origin header in response
# or the request might be blocked
```

### Test 5: Test Development Localhost

```bash
# Test localhost (should allow any origin in development)
curl -v \
  -H "Origin: http://localhost:3000" \
  http://localhost/api/services/

# Expected: Access-Control-Allow-Origin: http://localhost:3000
```

## Testing in Browser Developer Tools

### Test 1: Network Tab Analysis

1. Open the frontend application: `https://slykertech.co.zw`
2. Open Browser Developer Tools (F12)
3. Go to the Network tab
4. Filter by "XHR" or "Fetch"
5. Trigger an API request (e.g., load a page that fetches data)
6. Click on the request to view details
7. Check the Response Headers for:
   - `Access-Control-Allow-Origin: https://slykertech.co.zw`
   - `Access-Control-Allow-Credentials: true`
   - `Access-Control-Expose-Headers: Content-Type,X-CSRFToken`

### Test 2: Check for CORS Errors in Console

1. Open Browser Console (F12 → Console tab)
2. Look for any CORS-related error messages:
   - ❌ **Bad**: `Access to fetch at '...' has been blocked by CORS policy`
   - ✅ **Good**: No CORS errors

### Test 3: Inspect Preflight Requests

1. In Network tab, filter by "All" or "Other"
2. Look for OPTIONS requests (they appear before actual requests)
3. Check that OPTIONS requests return status 204
4. Verify CORS headers are present in OPTIONS response

### Test 4: Test WebSocket Connections

1. Open a page that establishes WebSocket connections
2. In Network tab, filter by "WS" (WebSocket)
3. Check WebSocket connection status - should be "101 Switching Protocols"
4. Verify no errors in Console related to WebSocket origin

## Testing with Browser Extensions

### Using "Advanced REST Client" or "Postman"

1. Install the browser extension
2. Create a new request:
   - Method: GET
   - URL: `https://api.slykertech.co.zw/api/services/`
   - Headers:
     - `Origin: https://slykertech.co.zw`
3. Send the request
4. Check response headers for CORS headers

## Automated Testing Script

Save this as `test-cors.sh`:

```bash
#!/bin/bash

echo "==================================="
echo "CORS Testing Script for Slykertech"
echo "==================================="
echo ""

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Test URLs
API_URL="https://api.slykertech.co.zw"
FRONTEND_ORIGIN="https://slykertech.co.zw"
WWW_ORIGIN="https://www.slykertech.co.zw"

# Test 1: Preflight request
echo "Test 1: Preflight OPTIONS request"
RESPONSE=$(curl -s -o /dev/null -w "%{http_code}" \
  -H "Origin: $FRONTEND_ORIGIN" \
  -H "Access-Control-Request-Method: POST" \
  -X OPTIONS \
  "$API_URL/api/services/")

if [ "$RESPONSE" -eq 204 ]; then
  echo -e "${GREEN}✓ Preflight request successful (204)${NC}"
else
  echo -e "${RED}✗ Preflight request failed (got $RESPONSE, expected 204)${NC}"
fi
echo ""

# Test 2: Check CORS headers in preflight
echo "Test 2: Verify CORS headers in preflight response"
HEADERS=$(curl -s -I \
  -H "Origin: $FRONTEND_ORIGIN" \
  -H "Access-Control-Request-Method: POST" \
  -X OPTIONS \
  "$API_URL/api/services/")

if echo "$HEADERS" | grep -q "Access-Control-Allow-Origin"; then
  echo -e "${GREEN}✓ Access-Control-Allow-Origin header present${NC}"
else
  echo -e "${RED}✗ Access-Control-Allow-Origin header missing${NC}"
fi

if echo "$HEADERS" | grep -q "Access-Control-Allow-Credentials"; then
  echo -e "${GREEN}✓ Access-Control-Allow-Credentials header present${NC}"
else
  echo -e "${RED}✗ Access-Control-Allow-Credentials header missing${NC}"
fi
echo ""

# Test 3: Actual GET request
echo "Test 3: Actual GET request with Origin header"
RESPONSE=$(curl -s -o /dev/null -w "%{http_code}" \
  -H "Origin: $FRONTEND_ORIGIN" \
  "$API_URL/api/services/")

if [ "$RESPONSE" -eq 200 ] || [ "$RESPONSE" -eq 401 ]; then
  echo -e "${GREEN}✓ GET request successful (got $RESPONSE)${NC}"
else
  echo -e "${RED}✗ GET request failed (got $RESPONSE)${NC}"
fi
echo ""

# Test 4: Check CORS headers in GET response
echo "Test 4: Verify CORS headers in GET response"
HEADERS=$(curl -s -I \
  -H "Origin: $FRONTEND_ORIGIN" \
  "$API_URL/api/services/")

if echo "$HEADERS" | grep -q "Access-Control-Allow-Origin"; then
  echo -e "${GREEN}✓ Access-Control-Allow-Origin header present in GET response${NC}"
else
  echo -e "${RED}✗ Access-Control-Allow-Origin header missing in GET response${NC}"
fi
echo ""

# Test 5: Test with www subdomain
echo "Test 5: Test with www subdomain"
RESPONSE=$(curl -s -o /dev/null -w "%{http_code}" \
  -H "Origin: $WWW_ORIGIN" \
  -H "Access-Control-Request-Method: GET" \
  -X OPTIONS \
  "$API_URL/api/services/")

if [ "$RESPONSE" -eq 204 ]; then
  echo -e "${GREEN}✓ www subdomain CORS working (204)${NC}"
else
  echo -e "${RED}✗ www subdomain CORS failed (got $RESPONSE)${NC}"
fi
echo ""

# Test 6: Health check endpoint
echo "Test 6: Health check endpoint"
RESPONSE=$(curl -s -o /dev/null -w "%{http_code}" \
  -H "Origin: $FRONTEND_ORIGIN" \
  "$API_URL/health/")

if [ "$RESPONSE" -eq 200 ]; then
  echo -e "${GREEN}✓ Health check successful (200)${NC}"
else
  echo -e "${RED}✗ Health check failed (got $RESPONSE)${NC}"
fi
echo ""

echo "==================================="
echo "CORS Testing Complete"
echo "==================================="
```

Make it executable and run:
```bash
chmod +x test-cors.sh
./test-cors.sh
```

## Common Issues and Solutions

### Issue 1: "No 'Access-Control-Allow-Origin' header is present"

**Cause**: Nginx is not adding CORS headers or the origin is not in the allowed list.

**Solution**:
1. Check that nginx is running and has the updated configuration
2. Verify the origin matches exactly (including protocol and port)
3. Reload nginx: `nginx -s reload`

### Issue 2: "The value of the 'Access-Control-Allow-Origin' header... must not be the wildcard '*' when credentials are included"

**Cause**: CORS_ALLOW_CREDENTIALS is true but CORS_ALLOW_ALL_ORIGINS is also true, or nginx is adding wildcard.

**Solution**:
1. Set `CORS_ALLOW_ALL_ORIGINS=False` in production
2. Use `CORS_ALLOWED_ORIGINS` with specific domains
3. Ensure nginx uses `$cors_origin` variable, not '*'

### Issue 3: OPTIONS requests return 403 or 401

**Cause**: CSRF or authentication middleware is intercepting OPTIONS requests.

**Solution**:
1. Ensure nginx handles OPTIONS requests before proxying to Django
2. Verify nginx returns 204 for OPTIONS (check nginx config)
3. Check that Django CORS middleware is before CSRF middleware

### Issue 4: WebSocket connections fail with CORS errors

**Cause**: Origin validation in ASGI is failing.

**Solution**:
1. Check that `CorsOriginValidator` is in use in `asgi.py`
2. Verify `CORS_ALLOWED_ORIGINS` includes WebSocket origins
3. Ensure nginx passes `Origin` header: `proxy_set_header Origin $http_origin;`

### Issue 5: CORS works in development but not production

**Cause**: Environment variables not set correctly in production.

**Solution**:
1. Verify `.env` file has correct production values:
   ```env
   DEBUG=False
   CORS_ALLOWED_ORIGINS=https://slykertech.co.zw,https://www.slykertech.co.zw
   CSRF_TRUSTED_ORIGINS=https://slykertech.co.zw,https://www.slykertech.co.zw,https://api.slykertech.co.zw
   ```
2. Restart backend service to reload environment variables
3. Check Django logs for CORS-related messages

## Verifying Environment Variables

Run this in the Django shell to check CORS settings:

```bash
cd backend
python manage.py shell
```

```python
from django.conf import settings

print("DEBUG:", settings.DEBUG)
print("CORS_ALLOW_ALL_ORIGINS:", getattr(settings, 'CORS_ALLOW_ALL_ORIGINS', False))
print("CORS_ALLOWED_ORIGINS:", getattr(settings, 'CORS_ALLOWED_ORIGINS', []))
print("CORS_ALLOW_CREDENTIALS:", settings.CORS_ALLOW_CREDENTIALS)
print("ALLOWED_HOSTS:", settings.ALLOWED_HOSTS)
print("CSRF_TRUSTED_ORIGINS:", settings.CSRF_TRUSTED_ORIGINS)
```

Expected output for production:
```
DEBUG: False
CORS_ALLOW_ALL_ORIGINS: False
CORS_ALLOWED_ORIGINS: ['https://slykertech.co.zw', 'https://www.slykertech.co.zw']
CORS_ALLOW_CREDENTIALS: True
ALLOWED_HOSTS: ['localhost', '127.0.0.1', 'api.slykertech.co.zw', 'slykertech.co.zw', 'www.slykertech.co.zw']
CSRF_TRUSTED_ORIGINS: ['https://slykertech.co.zw', 'https://www.slykertech.co.zw', 'https://api.slykertech.co.zw']
```

## Deployment Checklist

Before deploying CORS changes:

- [ ] Updated nginx.conf with CORS headers
- [ ] Set environment variables in production .env file
- [ ] Verified Django CORS middleware is installed and configured
- [ ] Tested CORS with curl commands
- [ ] Reloaded nginx: `nginx -s reload`
- [ ] Restarted Django/Daphne service
- [ ] Tested in browser with developer tools
- [ ] Verified no CORS errors in browser console
- [ ] Tested WebSocket connections
- [ ] Checked Django logs for errors

## Security Considerations

1. **Never use `CORS_ALLOW_ALL_ORIGINS=True` in production**
2. **Always specify exact origins** in CORS_ALLOWED_ORIGINS
3. **Use HTTPS in production** - CORS with credentials requires secure connections
4. **Limit exposed headers** - Only expose headers that frontend needs
5. **Set appropriate CORS_MAX_AGE** - Current setting is 20 days (1728000 seconds)
6. **Monitor logs** - Watch for suspicious cross-origin requests

## Additional Resources

- [MDN CORS Documentation](https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS)
- [Django CORS Headers Package](https://github.com/adamchainz/django-cors-headers)
- [Nginx CORS Configuration](https://enable-cors.org/server_nginx.html)
- [Testing CORS](https://www.test-cors.org/)
