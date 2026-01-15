"""
ASGI config for config project.

It exposes the ASGI callable as a module-level variable named ``application``.

For more information on this file, see
https://docs.djangoproject.com/en/6.0/howto/deployment/asgi/
"""

import os

from django.core.asgi import get_asgi_application
from channels.routing import ProtocolTypeRouter, URLRouter
from channels.auth import AuthMiddlewareStack
from channels.security.websocket import OriginValidator

os.environ.setdefault('DJANGO_SETTINGS_MODULE', 'config.settings')

django_asgi_app = get_asgi_application()

# Import routing and settings after Django setup
from config.routing import websocket_urlpatterns
from django.conf import settings


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
        
        # Then check against CORS_ALLOWED_ORIGINS if not in DEBUG mode
        if not settings.DEBUG and hasattr(settings, 'CORS_ALLOWED_ORIGINS'):
            # Reconstruct the full origin from parsed_origin
            origin = f"{parsed_origin[0]}://{parsed_origin[1]}"
            if parsed_origin[2] is not None:
                origin = f"{origin}:{parsed_origin[2]}"
            
            # Check if origin is in CORS_ALLOWED_ORIGINS
            if origin in settings.CORS_ALLOWED_ORIGINS:
                return True
        
        # In DEBUG mode with CORS_ALLOW_ALL_ORIGINS, allow all
        if settings.DEBUG and getattr(settings, 'CORS_ALLOW_ALL_ORIGINS', False):
            return True
        
        return False


application = ProtocolTypeRouter({
    "http": django_asgi_app,
    "websocket": CorsOriginValidator(
        AuthMiddlewareStack(
            URLRouter(websocket_urlpatterns)
        )
    ),
})
