# CORS Configuration Fix Documentation

## Problem Statement

The application was experiencing CORS (Cross-Origin Resource Sharing) policy errors when making requests from the frontend (`https://slykertech.co.zw`) to the backend API (`https://api.slykertech.co.zw`).

### Error Messages:
- `Access to fetch at 'https://api.slykertech.co.zw/api/billing/carts/current/' from origin 'https://slykertech.co.zw' has been blocked by CORS policy: Response to preflight request doesn't pass access control check: No 'Access-Control-Allow-Origin' header is present on the requested resource.`
- `Failed to load resource: net::ERR_FAILED`

## Root Causes Identified

1. **Duplicate CORS Configuration**: The `CORS_ALLOWED_ORIGINS` setting was being configured twice in `settings.py`, with the second configuration overriding the first.

2. **Missing API Domain**: The API domain itself (`https://api.slykertech.co.zw`) was not included in the `CORS_ALLOWED_ORIGINS` list.

3. **Missing Credentials in Frontend**: The frontend API service was not sending `credentials: 'include'` with fetch requests, which is required for cross-origin requests with cookies/sessions.

4. **Cookie SameSite Settings**: The `SESSION_COOKIE_SAMESITE` and `CSRF_COOKIE_SAMESITE` settings were not configured for cross-origin requests.

## Changes Made

### Backend Changes (`backend/config/settings.py`)

1. **Consolidated CORS Configuration** (Lines 220-232):
   ```python
   # CORS Settings
   # Allow all origins in development, specific origins in production
   if DEBUG:
       CORS_ALLOW_ALL_ORIGINS = True
   else:
       CORS_ALLOW_ALL_ORIGINS = False
       CORS_ALLOWED_ORIGINS = config(
           'CORS_ALLOWED_ORIGINS',
           default='http://localhost:3000,http://127.0.0.1:3000,https://slykertech.co.zw,https://www.slykertech.co.zw,https://api.slykertech.co.zw',
           cast=lambda v: [s.strip() for s in v.split(',')]
       )
   ```

2. **Removed Duplicate Configuration** (Lines 424-433):
   - Removed the duplicate `CORS_ALLOW_ALL_ORIGINS` and `CORS_ALLOWED_ORIGINS` configuration
   - Replaced with a comment noting that CORS settings are configured above

3. **Updated Cookie Settings** (Lines 261-267):
   ```python
   SESSION_COOKIE_DOMAIN = config('SESSION_COOKIE_DOMAIN', default=None)
   CSRF_COOKIE_DOMAIN = config('CSRF_COOKIE_DOMAIN', default=None)
   SESSION_COOKIE_SAMESITE = 'None' if not DEBUG else 'Lax'  # Required for cross-origin
   CSRF_COOKIE_SAMESITE = 'None' if not DEBUG else 'Lax'  # Required for cross-origin
   SESSION_COOKIE_HTTPONLY = True  # Security: prevent JavaScript access
   CSRF_COOKIE_HTTPONLY = False  # Must be False for JavaScript to read it
   ```

### Frontend Changes (`src/lib/api-service.ts`)

1. **Added Credentials to Fetch Requests** (Line 44):
   ```typescript
   const response = await fetch(`${this.baseUrl}${endpoint}`, {
     ...options,
     credentials: 'include', // Include cookies for CORS
     headers: {
       ...this.getHeaders(),
       ...options.headers,
     },
   });
   ```

### Configuration Changes (`backend/.env.example`)

1. **Updated CORS_ALLOWED_ORIGINS**:
   ```env
   # Add all domains that will access the API, including the API domain itself
   CORS_ALLOWED_ORIGINS=http://localhost:3000,http://127.0.0.1:3000,https://slykertech.co.zw,https://www.slykertech.co.zw,https://api.slykertech.co.zw
   ```

## How CORS Works

CORS (Cross-Origin Resource Sharing) is a security feature implemented by browsers to prevent malicious websites from making unauthorized requests to your API.

### Preflight Requests

For certain HTTP requests (like POST, PUT, DELETE with custom headers), browsers send a "preflight" OPTIONS request first to check if the actual request is allowed. The server must respond with appropriate CORS headers:

- `Access-Control-Allow-Origin`: Specifies which origins are allowed
- `Access-Control-Allow-Methods`: Specifies which HTTP methods are allowed
- `Access-Control-Allow-Headers`: Specifies which headers are allowed
- `Access-Control-Allow-Credentials`: Indicates if cookies can be included

### The Fix

The `django-cors-headers` package handles all of this automatically, but it needs to be properly configured:

1. **CORS middleware** must be positioned correctly in the middleware stack (after SessionMiddleware, before CommonMiddleware)
2. **Allowed origins** must include all domains that will make requests to the API
3. **Credentials** must be allowed if cookies/sessions are used
4. **SameSite cookie settings** must be set to 'None' for cross-origin requests in production

## Testing the Fix

To verify the CORS configuration is working:

1. **Check Browser Console**: Ensure no CORS errors appear
2. **Check Network Tab**: Verify that preflight OPTIONS requests return 200 status
3. **Check Response Headers**: Verify the following headers are present:
   - `Access-Control-Allow-Origin: https://slykertech.co.zw`
   - `Access-Control-Allow-Credentials: true`
   - `Access-Control-Allow-Methods: DELETE, GET, OPTIONS, PATCH, POST, PUT`

## Environment Variables

Ensure the following environment variables are set in production:

```env
# Backend (.env)
DEBUG=False
CORS_ALLOWED_ORIGINS=https://slykertech.co.zw,https://www.slykertech.co.zw,https://api.slykertech.co.zw
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

## Additional Notes

- The cart endpoint (`/api/billing/carts/current/`) requires session support, which is why proper cookie configuration is essential
- The login endpoint (`/api/token/`) needs CORS headers to accept credentials from the frontend
- All API endpoints now properly support cross-origin requests from the frontend domain
