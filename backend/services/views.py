from rest_framework import viewsets, permissions, status
from rest_framework.decorators import action, api_view, permission_classes
from rest_framework.response import Response
from .models import (Service, ServiceSubscription, DNSRecord,
                    ProjectTracker, ProjectMilestone, ProjectTask, ProjectComment,
                    HostingProduct, DomainProduct, ServiceAddon, DomainRegistration)
from .serializers import (
    ServiceSerializer, ServiceSubscriptionSerializer, 
    ServiceSubscriptionCreateSerializer, DNSRecordSerializer,
    ProjectTrackerSerializer, ProjectTrackerCreateSerializer,
    ProjectMilestoneSerializer, ProjectTaskSerializer, ProjectCommentSerializer,
    HostingProductSerializer, DomainProductSerializer, ServiceAddonSerializer,
    DomainRegistrationSerializer
)
from backend.services.whois_service import whois_service
import logging

logger = logging.getLogger(__name__)


class ServiceViewSet(viewsets.ModelViewSet):
    """ViewSet for Service model"""
    
    queryset = Service.objects.filter(is_active=True)
    serializer_class = ServiceSerializer
    permission_classes = [permissions.IsAuthenticatedOrReadOnly]
    
    def get_queryset(self):
        """Allow all users to view active services"""
        if self.request.user.is_authenticated and (self.request.user.is_superuser or self.request.user.user_type == 'admin'):
            return Service.objects.all()
        return Service.objects.filter(is_active=True)


class ServiceSubscriptionViewSet(viewsets.ModelViewSet):
    """ViewSet for ServiceSubscription model"""
    
    queryset = ServiceSubscription.objects.all()
    serializer_class = ServiceSubscriptionSerializer
    permission_classes = [permissions.IsAuthenticated]
    
    def get_queryset(self):
        """Filter subscriptions based on user"""
        user = self.request.user
        if user.is_superuser or user.user_type == 'admin':
            return ServiceSubscription.objects.all()
        # Clients can only see their own subscriptions
        return ServiceSubscription.objects.filter(client__user=user)
    
    def get_serializer_class(self):
        if self.action == 'create':
            return ServiceSubscriptionCreateSerializer
        return ServiceSubscriptionSerializer
    
    @action(detail=True, methods=['post'])
    def suspend(self, request, pk=None):
        """Suspend a subscription"""
        subscription = self.get_object()
        subscription.status = 'suspended'
        subscription.save()
        return Response(
            {"message": "Subscription suspended successfully."},
            status=status.HTTP_200_OK
        )
    
    @action(detail=True, methods=['post'])
    def activate(self, request, pk=None):
        """Activate a subscription"""
        subscription = self.get_object()
        subscription.status = 'active'
        subscription.save()
        return Response(
            {"message": "Subscription activated successfully."},
            status=status.HTTP_200_OK
        )


class DNSRecordViewSet(viewsets.ModelViewSet):
    """ViewSet for DNSRecord model"""
    
    queryset = DNSRecord.objects.all()
    serializer_class = DNSRecordSerializer
    permission_classes = [permissions.IsAuthenticated]
    
    def get_queryset(self):
        """Filter DNS records based on user"""
        user = self.request.user
        if user.is_superuser or user.user_type == 'admin':
            return DNSRecord.objects.all()
        # Clients can only see DNS records for their subscriptions
        return DNSRecord.objects.filter(subscription__client__user=user)


class ProjectTrackerViewSet(viewsets.ModelViewSet):
    """ViewSet for ProjectTracker model"""
    
    queryset = ProjectTracker.objects.all()
    serializer_class = ProjectTrackerSerializer
    permission_classes = [permissions.IsAuthenticated]
    
    def get_queryset(self):
        """Filter projects based on user"""
        user = self.request.user
        if user.is_superuser or user.user_type == 'admin':
            return ProjectTracker.objects.all()
        # Clients can only see projects for their subscriptions
        return ProjectTracker.objects.filter(subscription__client__user=user)
    
    def get_serializer_class(self):
        if self.action == 'create':
            return ProjectTrackerCreateSerializer
        return ProjectTrackerSerializer
    
    @action(detail=True, methods=['post'])
    def update_progress(self, request, pk=None):
        """Update project progress"""
        project = self.get_object()
        percentage = request.data.get('percentage')
        
        if percentage is not None:
            try:
                percentage = int(percentage)
                project.update_progress(percentage)
                return Response(
                    {"message": f"Progress updated to {percentage}%"},
                    status=status.HTTP_200_OK
                )
            except ValueError:
                return Response(
                    {"error": "Invalid percentage value"},
                    status=status.HTTP_400_BAD_REQUEST
                )
        
        return Response(
            {"error": "percentage field is required"},
            status=status.HTTP_400_BAD_REQUEST
        )


class ProjectMilestoneViewSet(viewsets.ModelViewSet):
    """ViewSet for ProjectMilestone model"""
    
    queryset = ProjectMilestone.objects.all()
    serializer_class = ProjectMilestoneSerializer
    permission_classes = [permissions.IsAuthenticated]
    
    def get_queryset(self):
        """Filter milestones based on user"""
        user = self.request.user
        if user.is_superuser or user.user_type == 'admin':
            return ProjectMilestone.objects.all()
        return ProjectMilestone.objects.filter(project__subscription__client__user=user)


class ProjectTaskViewSet(viewsets.ModelViewSet):
    """ViewSet for ProjectTask model"""
    
    queryset = ProjectTask.objects.all()
    serializer_class = ProjectTaskSerializer
    permission_classes = [permissions.IsAuthenticated]
    
    def get_queryset(self):
        """Filter tasks based on user"""
        user = self.request.user
        if user.is_superuser or user.user_type == 'admin':
            return ProjectTask.objects.all()
        return ProjectTask.objects.filter(project__subscription__client__user=user)


class ProjectCommentViewSet(viewsets.ModelViewSet):
    """ViewSet for ProjectComment model"""
    
    queryset = ProjectComment.objects.all()
    serializer_class = ProjectCommentSerializer
    permission_classes = [permissions.IsAuthenticated]
    
    def get_queryset(self):
        """Filter comments based on user"""
        user = self.request.user
        if user.is_superuser or user.user_type == 'admin':
            return ProjectComment.objects.all()
        # Clients can't see internal comments
        return ProjectComment.objects.filter(
            project__subscription__client__user=user,
            is_internal=False
        )
    
    def perform_create(self, serializer):
        """Set the user when creating a comment"""
        serializer.save(user=self.request.user)


class HostingProductViewSet(viewsets.ReadOnlyModelViewSet):
    """ViewSet for HostingProduct model - Read only for clients"""
    
    queryset = HostingProduct.objects.filter(is_active=True)
    serializer_class = HostingProductSerializer
    permission_classes = [permissions.IsAuthenticatedOrReadOnly]
    
    def get_queryset(self):
        """Allow all users to view active hosting products"""
        if self.request.user.is_authenticated and (self.request.user.is_superuser or self.request.user.user_type == 'admin'):
            return HostingProduct.objects.all()
        return HostingProduct.objects.filter(is_active=True)


class DomainProductViewSet(viewsets.ReadOnlyModelViewSet):
    """ViewSet for DomainProduct model - Read only for clients"""
    
    queryset = DomainProduct.objects.filter(is_active=True)
    serializer_class = DomainProductSerializer
    permission_classes = [permissions.IsAuthenticatedOrReadOnly]
    
    def get_queryset(self):
        """Allow all users to view active domain products"""
        if self.request.user.is_authenticated and (self.request.user.is_superuser or self.request.user.user_type == 'admin'):
            return DomainProduct.objects.all()
        return DomainProduct.objects.filter(is_active=True)


class ServiceAddonViewSet(viewsets.ReadOnlyModelViewSet):
    """ViewSet for ServiceAddon model - Read only for clients"""
    
    queryset = ServiceAddon.objects.filter(is_active=True)
    serializer_class = ServiceAddonSerializer
    permission_classes = [permissions.IsAuthenticatedOrReadOnly]
    
    def get_queryset(self):
        """Allow all users to view active service addons"""
        if self.request.user.is_authenticated and (self.request.user.is_superuser or self.request.user.user_type == 'admin'):
            return ServiceAddon.objects.all()
        return ServiceAddon.objects.filter(is_active=True)


class DomainRegistrationViewSet(viewsets.ModelViewSet):
    """ViewSet for DomainRegistration model"""
    
    queryset = DomainRegistration.objects.all()
    serializer_class = DomainRegistrationSerializer
    permission_classes = [permissions.IsAuthenticated]
    
    def get_queryset(self):
        """Filter domain registrations based on user"""
        user = self.request.user
        if user.is_superuser or user.user_type == 'admin':
            return DomainRegistration.objects.all()
        # Clients can only see their own domains
        return DomainRegistration.objects.filter(client__user=user)


@api_view(['POST', 'GET'])
@permission_classes([permissions.AllowAny])
def whois_check(request):
    """
    WHOIS domain availability check endpoint
    POST: Check domain availability for one or more domains
    GET: Get API information
    """
    if request.method == 'GET':
        return Response({
            'name': 'WHOIS Domain Check API',
            'version': '1.0.0',
            'description': 'Check domain availability using WHOIS servers',
            'features': {
                'parallel_processing': True,
                'result_caching': True,
                'cache_ttl': '1 hour',
                'max_workers': 5
            },
            'endpoints': {
                'POST': {
                    'path': '/api/whois/check/',
                    'description': 'Check domain availability',
                    'requestBody': {
                        'domains': ['example.com', 'test.org'],
                        'parallel': True,
                        'max_workers': 5
                    },
                    'response': {
                        'results': [
                            {
                                'domain': 'string',
                                'available': 'boolean',
                                'tld': 'string',
                                'whoisServer': 'string',
                                'message': 'string (optional)',
                                'error': 'string (optional)',
                                'cached': 'boolean'
                            }
                        ]
                    }
                }
            }
        })
    
    # POST method
    try:
        domains = request.data.get('domains', [])
        parallel = request.data.get('parallel', True)
        max_workers = request.data.get('max_workers', 5)
        
        if not domains or not isinstance(domains, list):
            return Response(
                {'error': 'Invalid request: domains must be a non-empty array'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        if len(domains) > 10:
            return Response(
                {'error': 'Maximum 10 domains per request'},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Query domains using WHOIS service with parallel processing
        logger.info(f"WHOIS check requested for domains: {domains} (parallel={parallel})")
        results = whois_service.query_multiple_domains(
            domains, 
            parallel=parallel, 
            max_workers=max_workers
        )
        
        return Response({'results': results})
        
    except Exception as e:
        logger.error(f"WHOIS check error: {e}", exc_info=True)
        return Response(
            {'error': 'Internal server error'},
            status=status.HTTP_500_INTERNAL_SERVER_ERROR
        )


@api_view(['GET', 'DELETE'])
@permission_classes([permissions.IsAdminUser])
def whois_cache(request):
    """
    WHOIS cache management endpoint (admin only)
    GET: Get cache statistics
    DELETE: Clear cache
    """
    if request.method == 'GET':
        try:
            stats = whois_service.get_cache_stats()
            return Response({
                'cache_stats': stats,
                'message': 'Cache statistics retrieved successfully'
            })
        except Exception as e:
            logger.error(f"Error getting cache stats: {e}", exc_info=True)
            return Response(
                {'error': 'Failed to retrieve cache stats'},
                status=status.HTTP_500_INTERNAL_SERVER_ERROR
            )
    
    elif request.method == 'DELETE':
        try:
            whois_service.clear_cache()
            return Response({
                'message': 'Cache cleared successfully'
            })
        except Exception as e:
            logger.error(f"Error clearing cache: {e}", exc_info=True)
            return Response(
                {'error': 'Failed to clear cache'},
                status=status.HTTP_500_INTERNAL_SERVER_ERROR
            )



