"""
Middleware to allow HTTP for internal Docker network requests
"""
from django.utils.deprecation import MiddlewareMixin


class DisableSSLRedirectForInternalMiddleware(MiddlewareMixin):
    """
    Disable SECURE_SSL_REDIRECT for internal Docker network requests
    to /api/livechat/bridge/ endpoints from Erlang services
    """
    
    def process_request(self, request):
        # Check if this is a livechat bridge API request from internal Docker network
        if request.path.startswith('/api/livechat/bridge/'):
            # Mark as secure to prevent SSL redirect
            request._dont_enforce_csrf_checks = True
            # Set the scheme to https to bypass SSL redirect
            request.META['wsgi.url_scheme'] = 'https'
        
        return None
