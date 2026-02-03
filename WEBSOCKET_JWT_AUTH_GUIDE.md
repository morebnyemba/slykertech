# WebSocket JWT Authentication Implementation

## Overview

This document describes the JWT authentication implementation for WebSocket connections in the Slyker Tech application. This fix resolves 401 authentication errors that were occurring on WebSocket connections.

## Problem Statement

The original WebSocket implementation used Django's `AuthMiddlewareStack`, which only supports session-based authentication. However, the frontend application uses JWT (JSON Web Token) authentication for API requests. This mismatch caused 401 errors when attempting to establish WebSocket connections.

## Solution

We implemented a custom JWT authentication middleware for Django Channels that:
1. Extracts JWT tokens from WebSocket query parameters
2. Validates tokens using the existing `rest_framework_simplejwt` library
3. Attaches the authenticated user to the WebSocket scope
4. Maintains compatibility with existing WebSocket consumers

## Implementation Details

### New Files Created

#### `backend/config/jwt_auth_middleware.py`

This file contains the JWT authentication middleware for WebSocket connections.

**Key Components:**

1. **`get_user_from_token(token_string)`** - Async function that:
   - Validates the JWT access token
   - Extracts the user ID from the token
   - Retrieves the user from the database
   - Returns the user object or `AnonymousUser` if invalid

2. **`JWTAuthMiddleware`** - Custom middleware class that:
   - Extracts the token from the WebSocket query string
   - Authenticates the user using the token
   - Attaches the authenticated user to `scope['user']`

3. **`JWTAuthMiddlewareStack(inner)`** - Helper function:
   - Wraps the URLRouter with JWT authentication
   - Similar to Django Channels' `AuthMiddlewareStack`

### Modified Files

#### `backend/config/asgi.py`

**Before:**
```python
from channels.auth import AuthMiddlewareStack

application = ProtocolTypeRouter({
    "http": django_asgi_app,
    "websocket": CorsOriginValidator(
        AuthMiddlewareStack(
            URLRouter(websocket_urlpatterns)
        )
    ),
})
```

**After:**
```python
from config.jwt_auth_middleware import JWTAuthMiddlewareStack

application = ProtocolTypeRouter({
    "http": django_asgi_app,
    "websocket": CorsOriginValidator(
        JWTAuthMiddlewareStack(
            URLRouter(websocket_urlpatterns)
        )
    ),
})
```

#### `src/app/about/page.tsx`

- Restored the "Awards & Recognition" section with HostAdvice badge
- Updated the 2026 milestone text to mention the HostAdvice award
- Badge is now displayed only on the About page (not in Footer)

## Usage

### Frontend WebSocket Connection

When establishing a WebSocket connection from the frontend, include the JWT access token in the query string:

```javascript
// Get the access token from your auth store
const token = useAuthStore.getState().token;

// Create WebSocket connection with token in query string
const ws = new WebSocket(`wss://api.slykertech.co.zw/ws/chat/room_name/?token=${token}`);
```

**Example for different WebSocket endpoints:**

```javascript
// Analytics WebSocket
const analyticsWs = new WebSocket(`wss://api.slykertech.co.zw/ws/analytics/?token=${token}`);

// DNS Panel WebSocket
const dnsWs = new WebSocket(`wss://api.slykertech.co.zw/ws/dns/?token=${token}`);

// Live Chat WebSocket
const chatWs = new WebSocket(`wss://api.slykertech.co.zw/ws/chat/${roomName}/?token=${token}`);
```

### Backend Consumer Access

WebSocket consumers can now access the authenticated user as before:

```python
class MyConsumer(AsyncWebsocketConsumer):
    async def connect(self):
        # Access authenticated user
        self.user = self.scope['user']
        
        # Check if user is authenticated
        if not self.user.is_authenticated:
            await self.close(code=4001)
            return
        
        # User is authenticated, proceed with connection
        await self.accept()
```

## Security Considerations

1. **Token Validation**: The middleware validates tokens using the same JWT library (`rest_framework_simplejwt`) used for REST API authentication, ensuring consistency.

2. **User Verification**: The middleware checks:
   - Token validity (signature, expiration)
   - User existence in database
   - User active status

3. **Error Handling**: Invalid or expired tokens result in `AnonymousUser`, not exceptions, preventing service disruption.

4. **CORS Protection**: The existing `CorsOriginValidator` remains in place, providing origin validation for WebSocket connections.

## Testing

### Manual Testing

1. **Test Authenticated Connection:**
   ```javascript
   const token = 'valid_jwt_token';
   const ws = new WebSocket(`ws://localhost:8000/ws/analytics/?token=${token}`);
   
   ws.onopen = () => console.log('Connected with authentication');
   ws.onerror = (error) => console.error('Connection error:', error);
   ```

2. **Test Unauthenticated Connection:**
   ```javascript
   const ws = new WebSocket(`ws://localhost:8000/ws/analytics/`);
   // Should connect but user will be AnonymousUser
   ```

3. **Test Invalid Token:**
   ```javascript
   const ws = new WebSocket(`ws://localhost:8000/ws/analytics/?token=invalid_token`);
   // Should connect but user will be AnonymousUser
   ```

### Automated Testing

Add tests to verify JWT authentication for WebSockets:

```python
from channels.testing import WebsocketCommunicator
from django.test import TestCase
from rest_framework_simplejwt.tokens import AccessToken

class WebSocketJWTAuthTestCase(TestCase):
    async def test_authenticated_connection(self):
        user = await self.create_user()
        token = str(AccessToken.for_user(user))
        
        communicator = WebsocketCommunicator(
            application,
            f"/ws/analytics/?token={token}"
        )
        
        connected, _ = await communicator.connect()
        self.assertTrue(connected)
        
        await communicator.disconnect()
    
    async def test_unauthenticated_connection(self):
        communicator = WebsocketCommunicator(
            application,
            "/ws/analytics/"
        )
        
        connected, _ = await communicator.connect()
        self.assertTrue(connected)  # Connects but user is anonymous
        
        await communicator.disconnect()
```

## Affected WebSocket Endpoints

This authentication middleware affects all WebSocket endpoints:

1. **`/ws/analytics/`** - Dashboard analytics updates (requires authentication)
2. **`/ws/dns/`** - DNS panel real-time updates (requires authentication)
3. **`/ws/chat/<room_name>/`** - Live chat (authentication required for management room)

## Migration Guide

### For Frontend Developers

Update your WebSocket connection code to include the JWT token:

**Before:**
```javascript
const ws = new WebSocket(`wss://api.slykertech.co.zw/ws/analytics/`);
```

**After:**
```javascript
import { useAuthStore } from '@/lib/stores/auth-store';

const token = useAuthStore.getState().token;
const ws = new WebSocket(`wss://api.slykertech.co.zw/ws/analytics/?token=${token}`);
```

### For Backend Developers

No changes required to existing consumer code. The `scope['user']` will now contain the JWT-authenticated user instead of the session-authenticated user.

## Troubleshooting

### Common Issues

1. **401 Errors Still Occurring**
   - Verify the token is being passed in the query string
   - Check that the token hasn't expired
   - Ensure the token is a valid JWT access token (not refresh token)

2. **User is AnonymousUser**
   - Check that the token is included in the WebSocket URL
   - Verify the token format: `?token=<jwt_token>`
   - Ensure the user exists and is active in the database

3. **Connection Rejected**
   - Check CORS settings in `settings.py`
   - Verify the origin is allowed in `CORS_ALLOWED_ORIGINS`
   - Check that `ALLOWED_HOSTS` includes the WebSocket server domain

## Future Improvements

1. **Token Refresh**: Implement automatic token refresh for long-running WebSocket connections
2. **Connection Pooling**: Add support for connection pooling with token validation
3. **Logging**: Add detailed logging for authentication failures
4. **Metrics**: Track authentication success/failure rates

## References

- [Django Channels Documentation](https://channels.readthedocs.io/)
- [Django REST Framework SimpleJWT](https://django-rest-framework-simplejwt.readthedocs.io/)
- [WebSocket Authentication Best Practices](https://auth0.com/blog/websockets-authentication/)

## Conclusion

This implementation provides secure, JWT-based authentication for WebSocket connections while maintaining compatibility with the existing REST API authentication system. The middleware is transparent to both frontend and backend code, requiring minimal changes to integrate.
