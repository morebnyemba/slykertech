# CORS and WebSocket Fix - Implementation Summary

## Overview

This document summarizes the implementation of fixes for CORS errors and WebSocket connection failures reported in the issue.

## Issues Addressed

### Issue #1: CORS Error
**Error Message:**
```
Failed to load resource: the server responded with a status of 400 ()
Access to fetch at 'https://api.slykertech.co.zw/api/billing/carts/current/' 
from origin 'https://slykertech.co.zw' has been blocked by CORS policy: 
Response to preflight request doesn't pass access control check: 
No 'Access-Control-Allow-Origin' header is present on the requested resource.
```

**Root Cause:** Environment variables containing CORS origins were quoted (e.g., `"https://slykertech.co.zw"`), but the parsing logic didn't strip quotes properly, resulting in malformed origin values that didn't match browser requests.

**Fix:** 
- Created `parse_comma_separated()` helper function that handles both quoted and unquoted values
- Updated all environment variable parsing to use the helper
- Updated `.env.example` to document the correct unquoted format
- Added CORS environment variables to `docker-compose.yml`

### Issue #2: WebSocket Connection Failure
**Error Message:**
```
WebSocket connection to 'wss://api.slykertech.co.zw/ws/chat/sales/' failed
```

**Root Causes:**
1. Erlang livechat service didn't validate CORS origins for WebSocket connections
2. WebSocket routing pattern didn't match the `/ws/chat/sales/` path

**Fix:**
- Created `cors_config` module with shared origin validation logic
- Updated `chat_websocket_handler` to validate origins and reject unauthorized connections
- Updated routing from `/ws/[...]` to `/ws/chat/[...]` for better specificity
- Added proper WebSocket initialization callback

## Technical Details

### Django Backend Changes

#### 1. Environment Variable Parsing (`backend/config/settings.py`)

**Before:**
```python
CORS_ALLOWED_ORIGINS = config(
    'CORS_ALLOWED_ORIGINS',
    default='...',
    cast=lambda v: [s.strip() for s in v.split(',')]
)
```

**After:**
```python
def parse_comma_separated(value):
    """Parse comma-separated values, handling both quoted and unquoted."""
    if not value:
        return []
    value = value.strip().strip('"').strip("'")
    return [s.strip().strip('"').strip("'") for s in value.split(',') if s.strip()]

CORS_ALLOWED_ORIGINS = config(
    'CORS_ALLOWED_ORIGINS',
    default='...',
    cast=parse_comma_separated
)
```

**Why:** The lambda approach didn't handle quoted values from `.env` files, leading to origins like `"https://slykertech.co.zw` (with leading quote) instead of `https://slykertech.co.zw`.

### Erlang LiveChat Changes

#### 1. Shared CORS Configuration (`livechat_erlang/src/cors_config.erl`)

Created a new module to centralize CORS configuration:

```erlang
-module(cors_config).
-export([allowed_origins/0, is_origin_allowed/1]).

allowed_origins() ->
    [
        <<"https://slykertech.co.zw">>,
        <<"https://www.slykertech.co.zw">>,
        <<"http://localhost:3000">>,
        <<"http://127.0.0.1:3000">>
    ].

is_origin_allowed(<<>>) -> true;  % Allow direct connections
is_origin_allowed(Origin) -> lists:member(Origin, allowed_origins()).
```

**Why:** Avoids code duplication and ensures consistency across handlers.

#### 2. WebSocket Handler (`livechat_erlang/src/chat_websocket_handler.erl`)

Added origin validation during WebSocket upgrade:

```erlang
init(Req0, State) ->
    Origin = cowboy_req:header(<<"origin">>, Req0, <<"">>),
    case cors_config:is_origin_allowed(Origin) of
        true -> {cowboy_websocket, Req0, State};
        false -> 
            Req = cowboy_req:reply(403, #{}, <<"Forbidden">>, Req0),
            {ok, Req, State}
    end.
```

**Why:** Browsers send Origin header during WebSocket upgrade; we need to validate it before accepting the connection.

#### 3. Routing (`livechat_erlang/src/livechat_app.erl`)

**Before:**
```erlang
{"/ws/[...]", chat_websocket_handler, []}
```

**After:**
```erlang
{"/ws/chat/:room", chat_websocket_handler, []},
{"/ws/chat/[...]", chat_websocket_handler, []}
```

**Why:** 
- More specific pattern prevents conflicts with future WebSocket endpoints
- Supports both named rooms (`/ws/chat/sales`) and dynamic paths (`/ws/chat/sales/123`)

### Docker Configuration Changes

Added CORS environment variables to `docker-compose.yml`:

```yaml
backend:
  environment:
    - CORS_ALLOWED_ORIGINS=${CORS_ALLOWED_ORIGINS:-...}
    - CSRF_TRUSTED_ORIGINS=${CSRF_TRUSTED_ORIGINS:-...}
```

**Why:** Ensures these critical environment variables are passed to the Django container.

## Security Considerations

### 1. Origin Whitelisting
- **Production**: Only specific origins are allowed (slykertech.co.zw, www.slykertech.co.zw)
- **Development**: Localhost origins included for local testing
- **No Wildcards**: Never use `*` or regex patterns for production

### 2. Credential Handling
- `CORS_ALLOW_CREDENTIALS = True` allows cookies and authentication headers
- Only works with specific origins (not with wildcard)
- Required for JWT authentication to work cross-origin

### 3. WebSocket Security
- Origin validation happens before upgrade completes
- Unauthorized origins receive 403 Forbidden immediately
- No sensitive data exposed to unauthorized origins

## Testing

### Manual Testing Commands

```bash
# Test CORS on API endpoint
curl -v \
  -H "Origin: https://slykertech.co.zw" \
  https://api.slykertech.co.zw/api/services/

# Expected: Access-Control-Allow-Origin: https://slykertech.co.zw

# Test WebSocket connection
wscat -c "wss://api.slykertech.co.zw/ws/chat/sales/" \
  -H "Origin: https://slykertech.co.zw"

# Expected: Connection established
```

### Browser Testing
1. Open DevTools Console on https://slykertech.co.zw
2. Check for CORS errors - should be none
3. Check Network tab - CORS headers should be present
4. Test WebSocket connection - should connect successfully

## Deployment Checklist

- [ ] Update `.env` file with unquoted values
- [ ] Rebuild Erlang container: `docker-compose build --no-cache livechat`
- [ ] Restart all services: `docker-compose up -d`
- [ ] Test CORS with curl
- [ ] Test WebSocket with wscat or browser
- [ ] Monitor logs for errors
- [ ] Verify in browser console

## Files Modified

1. **Backend (Python/Django)**
   - `backend/config/settings.py` - Added parsing helper
   - `.env.example` - Updated format

2. **LiveChat (Erlang)**
   - `livechat_erlang/src/cors_config.erl` - NEW: Shared config
   - `livechat_erlang/src/chat_websocket_handler.erl` - Added validation
   - `livechat_erlang/src/cors_handler.erl` - NEW: Preflight handler
   - `livechat_erlang/src/livechat_app.erl` - Updated routing

3. **Docker**
   - `docker-compose.yml` - Added env vars

4. **Documentation**
   - `CORS_WEBSOCKET_FIX_DEPLOYMENT.md` - NEW: Deployment guide
   - `CORS_WEBSOCKET_FIX_SUMMARY.md` - NEW: This file

## Code Review & Security Scan Results

✅ **Code Review**: Passed with 2 suggestions, both addressed
- Extracted CORS config to shared module
- Made routing more specific

✅ **Security Scan**: Passed with 0 alerts
- No vulnerabilities detected
- All changes follow security best practices

## Known Limitations

1. **Erlang Configuration**: Origins are hardcoded in `cors_config.erl`. Future improvement: read from environment variable.

2. **Error Messages**: 403 responses don't include detailed error messages to avoid information disclosure.

3. **Development Origins**: Localhost origins are included in the code. Consider using environment-based configuration for production deployments.

## Troubleshooting

### CORS Still Failing

1. Check environment variables are loaded:
   ```bash
   docker-compose exec backend python manage.py shell -c \
     "from django.conf import settings; print(settings.CORS_ALLOWED_ORIGINS)"
   ```

2. Verify no quotes in output

3. Restart backend: `docker-compose restart backend`

### WebSocket Still Failing

1. Check livechat is running: `docker-compose ps livechat`

2. Check health: `curl http://localhost:4001/health`

3. Rebuild: `docker-compose build --no-cache livechat && docker-compose up -d livechat`

## Future Improvements

1. **Environment-based Erlang Config**: Read allowed origins from environment variable instead of hardcoding

2. **Metrics**: Add monitoring for CORS rejections and WebSocket connection failures

3. **Logging**: Add structured logging for origin validation failures

4. **Rate Limiting**: Consider adding rate limiting for WebSocket connections

## References

- Django CORS Headers: https://github.com/adamchainz/django-cors-headers
- Cowboy WebSocket Guide: https://ninenines.eu/docs/en/cowboy/2.10/guide/ws_handlers/
- MDN CORS Guide: https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS

## Support

For issues or questions:
1. Check logs: `docker-compose logs <service>`
2. Review CORS_WEBSOCKET_FIX_DEPLOYMENT.md
3. Review Django and Erlang logs for specific errors
4. Check browser DevTools Console and Network tab
