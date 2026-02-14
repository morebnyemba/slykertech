from rest_framework import viewsets, permissions, status
from rest_framework.decorators import action, api_view, permission_classes
from rest_framework.response import Response
from django.utils import timezone
from .models import (Service, ServiceSubscription, DNSRecord,
                    ProjectTracker, ProjectMilestone, ProjectTask, ProjectComment,
                    HostingProduct, DomainProduct, ServiceAddon, DomainRegistration,
                    DomainTransferRequest, ProvisioningFailure)
from .serializers import (
    ServiceSerializer, ServiceSubscriptionSerializer, 
    ServiceSubscriptionCreateSerializer, DNSRecordSerializer,
    ProjectTrackerSerializer, ProjectTrackerCreateSerializer,
    ProjectMilestoneSerializer, ProjectTaskSerializer, ProjectCommentSerializer,
    HostingProductSerializer, DomainProductSerializer, ServiceAddonSerializer,
    DomainRegistrationSerializer, DomainTransferRequestSerializer,
    DomainTransferRequestCreateSerializer, ProvisioningFailureSerializer,
    ProvisioningFailureUpdateSerializer, ManualProvisioningSerializer
)
from .whois_service import whois_service
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
        """Filter DNS records based on user, with optional domain and subscription filters"""
        user = self.request.user
        if user.is_superuser or user.user_type == 'admin':
            qs = DNSRecord.objects.all()
        else:
            # Clients can only see DNS records for their subscriptions
            qs = DNSRecord.objects.filter(subscription__client__user=user)

        domain = self.request.query_params.get('domain')
        if domain:
            qs = qs.filter(domain=domain)

        subscription_id = self.request.query_params.get('subscription')
        if subscription_id:
            qs = qs.filter(subscription_id=subscription_id)

        record_type = self.request.query_params.get('record_type')
        if record_type:
            qs = qs.filter(record_type=record_type.upper())

        return qs

    def perform_create(self, serializer):
        """Ensure the subscription belongs to the requesting user"""
        user = self.request.user
        subscription = serializer.validated_data.get('subscription')
        if subscription and not (user.is_superuser or user.user_type == 'admin'):
            if subscription.client.user != user:
                from rest_framework.exceptions import PermissionDenied
                raise PermissionDenied("You do not have permission to add DNS records to this subscription.")
        serializer.save()


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


class DomainTransferRequestViewSet(viewsets.ModelViewSet):
    """ViewSet for DomainTransferRequest model - handles domain transfer requests"""
    
    queryset = DomainTransferRequest.objects.all()
    serializer_class = DomainTransferRequestSerializer
    permission_classes = [permissions.AllowAny]  # Allow public access for transfer requests
    
    def get_queryset(self):
        """Filter transfer requests based on user"""
        user = self.request.user
        if user.is_authenticated:
            if user.is_superuser or user.user_type == 'admin':
                return DomainTransferRequest.objects.all()
            # Authenticated clients can only see their own transfer requests
            return DomainTransferRequest.objects.filter(client__user=user)
        # Non-authenticated users can't list transfer requests
        return DomainTransferRequest.objects.none()
    
    def get_serializer_class(self):
        if self.action == 'create':
            return DomainTransferRequestCreateSerializer
        return DomainTransferRequestSerializer
    
    def perform_create(self, serializer):
        """Set the client when creating a transfer request (if authenticated)"""
        user = self.request.user
        if user.is_authenticated:
            try:
                from clients.models import Client
                client = Client.objects.get(user=user)
                serializer.save(client=client)
            except Client.DoesNotExist:
                # User is authenticated but doesn't have a client profile
                serializer.save()
        else:
            serializer.save()
    
    @action(detail=True, methods=['post'], permission_classes=[permissions.IsAuthenticated])
    def update_epp_code(self, request, pk=None):
        """Update EPP code for a transfer request"""
        transfer_request = self.get_object()
        epp_code = request.data.get('epp_code')
        
        if not epp_code:
            return Response(
                {"error": "EPP code is required"},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        transfer_request.epp_code = epp_code
        transfer_request.status = 'in_progress'
        transfer_request.save()
        
        return Response(
            {"message": "EPP code updated successfully"},
            status=status.HTTP_200_OK
        )


class IsStaffUser(permissions.BasePermission):
    """Permission class to check if user is staff"""
    
    def has_permission(self, request, view):
        return request.user and request.user.is_authenticated and (
            request.user.is_staff or request.user.is_superuser or 
            getattr(request.user, 'user_type', '') == 'admin'
        )


class ProvisioningFailureViewSet(viewsets.ModelViewSet):
    """ViewSet for ProvisioningFailure model - Admin only"""
    
    queryset = ProvisioningFailure.objects.all()
    serializer_class = ProvisioningFailureSerializer
    permission_classes = [IsStaffUser]
    
    def get_queryset(self):
        """Get provisioning failures with optional filtering"""
        queryset = ProvisioningFailure.objects.all().order_by('-created_at')
        
        # Filter by status if provided
        status_filter = self.request.query_params.get('status', None)
        if status_filter:
            queryset = queryset.filter(status=status_filter)
        
        return queryset
    
    def get_serializer_class(self):
        if self.action in ['update', 'partial_update']:
            return ProvisioningFailureUpdateSerializer
        return ProvisioningFailureSerializer
    
    @action(detail=False, methods=['get'])
    def pending_count(self, request):
        """Get count of pending provisioning failures"""
        count = ProvisioningFailure.objects.filter(status='pending').count()
        return Response({'pending_count': count})
    
    @action(detail=True, methods=['post'])
    def mark_resolved(self, request, pk=None):
        """Mark a provisioning failure as resolved"""
        failure = self.get_object()
        notes = request.data.get('notes', '')
        
        failure.mark_resolved(request.user, notes)
        
        return Response({
            'message': 'Provisioning failure marked as resolved',
            'resolved_by': request.user.email,
            'resolved_at': failure.resolved_at.isoformat()
        })
    
    @action(detail=True, methods=['post'])
    def complete_manual_provisioning(self, request, pk=None):
        """
        Complete manual provisioning for a failed subscription.
        Admin provides the manually created credentials.
        """
        failure = self.get_object()
        serializer = ManualProvisioningSerializer(data=request.data)
        
        if not serializer.is_valid():
            return Response(serializer.errors, status=status.HTTP_400_BAD_REQUEST)
        
        subscription = failure.subscription
        data = serializer.validated_data
        
        # Update subscription with manual provisioning data
        subscription.metadata.update({
            'manually_provisioned': True,
            'provisioned_by': request.user.email,
            'provisioned_at': timezone.now().isoformat(),
            'provisioned_username': data.get('provisioned_username', ''),
            'provisioned_domain': data.get('provisioned_domain', ''),
            **data.get('additional_data', {})
        })
        subscription.provisioning_completed = True
        subscription.status = 'active'
        subscription.provisioning_error = None
        subscription.save()
        
        # Mark failure as resolved
        failure.mark_resolved(request.user, data.get('notes', 'Manual provisioning completed'))
        
        return Response({
            'message': 'Manual provisioning completed successfully',
            'subscription_id': subscription.id,
            'subscription_status': subscription.status
        })
    
    @action(detail=True, methods=['post'])
    def dismiss(self, request, pk=None):
        """Dismiss a provisioning failure (e.g., if not applicable)"""
        failure = self.get_object()
        notes = request.data.get('notes', '')
        
        failure.status = 'dismissed'
        failure.admin_notes = notes
        failure.resolved_by = request.user
        failure.resolved_at = timezone.now()
        failure.save()
        
        return Response({
            'message': 'Provisioning failure dismissed',
            'dismissed_by': request.user.email
        })
    
    @action(detail=True, methods=['post'])
    def retry_provisioning(self, request, pk=None):
        """Retry automatic provisioning for a failed subscription"""
        from .provisioning import provisioning_service
        
        failure = self.get_object()
        subscription = failure.subscription
        
        # Mark failure as in progress
        failure.status = 'in_progress'
        failure.save()
        
        # Attempt provisioning again
        success, message = provisioning_service.provision_subscription(subscription)
        
        if success:
            # Mark subscription as provisioned
            subscription.provisioning_completed = True
            subscription.status = 'active'
            subscription.provisioning_error = None
            subscription.save()
            
            # Mark failure as resolved
            failure.mark_resolved(request.user, f'Retry successful: {message}')
            
            return Response({
                'success': True,
                'message': message
            })
        else:
            # Update failure with new error
            failure.status = 'pending'
            failure.error_message = message
            failure.error_details['retry_attempted'] = timezone.now().isoformat()
            failure.error_details['retry_error'] = message
            failure.save()
            
            return Response({
                'success': False,
                'message': message
            }, status=status.HTTP_400_BAD_REQUEST)


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



