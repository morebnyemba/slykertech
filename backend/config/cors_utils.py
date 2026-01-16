"""
Shared utilities for CORS handling across the application.
"""
from django.conf import settings


def add_cors_headers_to_response(response, origin):
    """
    Add CORS headers to a response object for the given origin.
    
    Args:
        response: Django Response object
        origin: The origin string from the request
    
    Returns:
        The response object with CORS headers added (if origin is allowed)
    """
    if not origin:
        return response
    
    # Check if origin is allowed
    if not is_origin_allowed(origin):
        return response
    
    # Add CORS headers
    response['Access-Control-Allow-Origin'] = origin
    response['Access-Control-Allow-Credentials'] = 'true'
    
    # Add other CORS headers if not present
    if 'Access-Control-Allow-Methods' not in response:
        response['Access-Control-Allow-Methods'] = ', '.join(
            getattr(settings, 'CORS_ALLOW_METHODS', [
                'DELETE', 'GET', 'OPTIONS', 'PATCH', 'POST', 'PUT'
            ])
        )
    
    if 'Access-Control-Allow-Headers' not in response:
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
    
    if 'Access-Control-Expose-Headers' not in response:
        response['Access-Control-Expose-Headers'] = ', '.join(
            getattr(settings, 'CORS_EXPOSE_HEADERS', [
                'content-type',
                'x-csrftoken',
            ])
        )
    
    return response


def is_origin_allowed(origin):
    """
    Check if the origin is allowed based on CORS settings.
    
    Args:
        origin: The origin string to check
    
    Returns:
        True if origin is allowed, False otherwise
    """
    # In DEBUG mode with CORS_ALLOW_ALL_ORIGINS, allow all
    if settings.DEBUG and getattr(settings, 'CORS_ALLOW_ALL_ORIGINS', False):
        return True
    
    # Check against CORS_ALLOWED_ORIGINS
    if hasattr(settings, 'CORS_ALLOWED_ORIGINS'):
        return origin in settings.CORS_ALLOWED_ORIGINS
    
    return False
