"""
JWT Authentication Middleware for Django Channels WebSockets

This middleware authenticates WebSocket connections using JWT tokens.
It extracts the token from the query string and validates it against
the Django REST Framework SimpleJWT backend.
"""

from urllib.parse import parse_qs
from django.contrib.auth.models import AnonymousUser
from channels.db import database_sync_to_async
from channels.middleware import BaseMiddleware
from rest_framework_simplejwt.tokens import AccessToken
from rest_framework_simplejwt.exceptions import TokenError, InvalidToken


@database_sync_to_async
def get_user_from_token(token_string):
    """
    Validate JWT token and return the associated user.
    
    Args:
        token_string: The JWT access token string
        
    Returns:
        User object if token is valid, AnonymousUser otherwise
    """
    try:
        # Validate the token
        access_token = AccessToken(token_string)
        
        # Get user ID from token
        user_id = access_token.get('user_id')
        
        if not user_id:
            return AnonymousUser()
        
        # Import User model
        from django.contrib.auth import get_user_model
        User = get_user_model()
        
        # Get user from database
        try:
            user = User.objects.get(id=user_id)
            
            # Check if user is active
            if not user.is_active:
                return AnonymousUser()
                
            return user
            
        except User.DoesNotExist:
            return AnonymousUser()
            
    except (TokenError, InvalidToken) as e:
        # Invalid token
        return AnonymousUser()
    except Exception as e:
        # Any other error
        return AnonymousUser()


class JWTAuthMiddleware(BaseMiddleware):
    """
    Custom middleware that authenticates WebSocket connections using JWT tokens.
    
    The token should be passed in the query string as 'token' parameter:
    ws://example.com/ws/chat/?token=<jwt_access_token>
    
    If no token is provided or the token is invalid, the user will be set to AnonymousUser.
    """
    
    async def __call__(self, scope, receive, send):
        """
        Process the WebSocket connection and authenticate the user.
        
        Args:
            scope: The connection scope containing query string and headers
            receive: ASGI receive channel
            send: ASGI send channel
        """
        # Get query string from scope
        query_string = scope.get('query_string', b'').decode('utf-8')
        query_params = parse_qs(query_string)
        
        # Extract token from query parameters
        token = query_params.get('token', [None])[0]
        
        # Set user to AnonymousUser by default
        scope['user'] = AnonymousUser()
        
        if token:
            # Authenticate user from token
            scope['user'] = await get_user_from_token(token)
        
        # Call the next middleware or consumer
        return await super().__call__(scope, receive, send)


def JWTAuthMiddlewareStack(inner):
    """
    Helper function to wrap the URLRouter with JWT authentication middleware.
    
    Usage:
        application = ProtocolTypeRouter({
            "websocket": JWTAuthMiddlewareStack(
                URLRouter(websocket_urlpatterns)
            ),
        })
    """
    return JWTAuthMiddleware(inner)
