"""
ASGI config for config project.

It exposes the ASGI callable as a module-level variable named ``application``.

For more information on this file, see
https://docs.djangoproject.com/en/6.0/howto/deployment/asgi/
"""

import os
import logging

from django.core.asgi import get_asgi_application
from channels.routing import ProtocolTypeRouter, URLRouter
from channels.auth import AuthMiddlewareStack
from channels.security.websocket import OriginValidator

# Configure logging for WebSocket debugging
logger = logging.getLogger(__name__)

os.environ.setdefault('DJANGO_SETTINGS_MODULE', 'config.settings')

django_asgi_app = get_asgi_application()

# Import routing and settings after Django setup
from config.routing import websocket_urlpatterns
from django.conf import settings


class CorsOriginValidator(OriginValidator):
    """
    Custom origin validator that respects CORS_ALLOWED_ORIGINS setting
    for WebSocket connections in addition to ALLOWED_HOSTS.
    More permissive to allow legitimate cross-origin WebSocket connections.
    """
    
    def valid_origin(self, parsed_origin):
        """
        Validate origin against both ALLOWED_HOSTS and CORS_ALLOWED_ORIGINS.
        parsed_origin is a tuple: (scheme, host, port)
        """
        # Log the origin validation attempt
        logger.info(f"WebSocket origin validation: {parsed_origin}")
        
        # First check with parent class (uses ALLOWED_HOSTS)
        if super().valid_origin(parsed_origin):
            logger.info(f"Origin validated by parent class (ALLOWED_HOSTS): {parsed_origin}")
            return True
        
        # In DEBUG mode with CORS_ALLOW_ALL_ORIGINS, allow all
        if settings.DEBUG and getattr(settings, 'CORS_ALLOW_ALL_ORIGINS', False):
            logger.info(f"Origin allowed in DEBUG mode with CORS_ALLOW_ALL_ORIGINS: {parsed_origin}")
            return True
        
        # Check against CORS_ALLOWED_ORIGINS if configured
        if hasattr(settings, 'CORS_ALLOWED_ORIGINS'):
            # Reconstruct the full origin from parsed_origin
            origin = f"{parsed_origin[0]}://{parsed_origin[1]}"
            if parsed_origin[2] is not None:
                origin = f"{origin}:{parsed_origin[2]}"
            
            # Check if origin is in CORS_ALLOWED_ORIGINS
            if origin in settings.CORS_ALLOWED_ORIGINS:
                logger.info(f"Origin found in CORS_ALLOWED_ORIGINS: {origin}")
                return True
            
            # Also check without port for standard ports (80, 443)
            # since browsers may or may not include them
            if parsed_origin[2] in (80, 443, None):
                origin_without_port = f"{parsed_origin[0]}://{parsed_origin[1]}"
                if origin_without_port in settings.CORS_ALLOWED_ORIGINS:
                    logger.info(f"Origin (without port) found in CORS_ALLOWED_ORIGINS: {origin_without_port}")
                    return True
        
        # Additional check: if the host matches any ALLOWED_HOSTS, accept it
        # This helps with WebSocket connections from the same domain
        if hasattr(settings, 'ALLOWED_HOSTS'):
            host = parsed_origin[1]
            if host in settings.ALLOWED_HOSTS or '*' in settings.ALLOWED_HOSTS:
                logger.info(f"Host found in ALLOWED_HOSTS: {host}")
                return True
        
        logger.warning(f"WebSocket origin rejected: {parsed_origin}")
        return False


application = ProtocolTypeRouter({
    "http": django_asgi_app,
    "websocket": CorsOriginValidator(
        AuthMiddlewareStack(
            URLRouter(websocket_urlpatterns)
        )
    ),
})
