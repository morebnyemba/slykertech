"""
Enhanced CORS middleware to ensure headers are always present,
even on error responses and when corsheaders middleware might miss them.
"""
from django.utils.deprecation import MiddlewareMixin
from .cors_utils import add_cors_headers_to_response, is_origin_allowed
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
            # Check if origin is allowed and add headers
            if is_origin_allowed(origin):
                add_cors_headers_to_response(response, origin)
                logger.debug(f"Enhanced CORS middleware added headers for origin: {origin}")
        
        return response
