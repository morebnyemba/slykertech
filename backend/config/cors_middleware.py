"""
Enhanced CORS middleware to ensure headers are always present,
even on error responses and when corsheaders middleware might miss them.
"""
from django.conf import settings
from django.utils.deprecation import MiddlewareMixin
import logging

logger = logging.getLogger(__name__)


class EnhancedCorsMiddleware(MiddlewareMixin):
    """
    Middleware to ensure CORS headers are always present on responses,
    including error responses that might not go through corsheaders middleware.
    
    This is a safety net to work alongside django-cors-headers.
    """
    
    def process_response(self, request, response):
        """
        Add CORS headers to response if they're not already present.
        This ensures error responses and edge cases have CORS headers.
        """
        origin = request.META.get('HTTP_ORIGIN')
        
        if not origin:
            return response
        
        # Only add headers if Access-Control-Allow-Origin is not already set
        # (to avoid conflicts with django-cors-headers)
        if 'Access-Control-Allow-Origin' not in response:
            # Check if origin is allowed
            if self.is_origin_allowed(origin):
                response['Access-Control-Allow-Origin'] = origin
                response['Access-Control-Allow-Credentials'] = 'true'
                
                # Add other CORS headers
                response['Access-Control-Allow-Methods'] = ', '.join(
                    getattr(settings, 'CORS_ALLOW_METHODS', [
                        'DELETE', 'GET', 'OPTIONS', 'PATCH', 'POST', 'PUT'
                    ])
                )
                
                response['Access-Control-Allow-Headers'] = ', '.join(
                    getattr(settings, 'CORS_ALLOW_HEADERS', [
                        'accept',
                        'accept-encoding',
                        'authorization',
                        'content-type',
                        'dnt',
                        'origin',
                        'user-agent',
                        'x-csrftoken',
                        'x-requested-with',
                    ])
                )
                
                response['Access-Control-Expose-Headers'] = ', '.join(
                    getattr(settings, 'CORS_EXPOSE_HEADERS', [
                        'content-type',
                        'x-csrftoken',
                    ])
                )
                
                logger.debug(f"Enhanced CORS middleware added headers for origin: {origin}")
        
        return response
    
    def is_origin_allowed(self, origin):
        """
        Check if the origin is allowed based on CORS settings.
        """
        # In DEBUG mode with CORS_ALLOW_ALL_ORIGINS, allow all
        if settings.DEBUG and getattr(settings, 'CORS_ALLOW_ALL_ORIGINS', False):
            return True
        
        # Check against CORS_ALLOWED_ORIGINS
        if hasattr(settings, 'CORS_ALLOWED_ORIGINS'):
            return origin in settings.CORS_ALLOWED_ORIGINS
        
        return False
