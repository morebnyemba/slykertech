"""
Middleware to handle SSL redirect and internal Docker requests
"""
from django.utils.deprecation import MiddlewareMixin
from django.conf import settings


class CustomSSLRedirectMiddleware(MiddlewareMixin):
    """
    Custom SSL redirect that exempts internal Docker API paths
    from SECURE_SSL_REDIRECT
    """
    
    EXEMPT_PATHS = [
        '/api/livechat/bridge/',  # Internal Erlang service calls
    ]
    
    def process_request(self, request):
        # Check if this is an exempt path
        for exempt_path in self.EXEMPT_PATHS:
            if request.path.startswith(exempt_path):
                # Skip SSL redirect for internal APIs
                return None
        
        # Apply standard SSL redirect if configured
        if settings.SECURE_SSL_REDIRECT and not self._is_secure(request):
            return self._redirect_to_https(request)
        
        return None
    
    def _is_secure(self, request):
        """Check if request is secure"""
        if request.is_secure():
            return True
        
        # Check X-Forwarded-Proto header (set by nginx)
        if request.META.get('HTTP_X_FORWARDED_PROTO') == 'https':
            return True
        
        return False
    
    def _redirect_to_https(self, request):
        """Redirect to HTTPS"""
        from django.http import HttpResponsePermanentRedirect
        
        url = request.get_full_path()
        url = url.replace('http://', 'https://', 1)
        return HttpResponsePermanentRedirect(url)
