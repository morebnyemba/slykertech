from rest_framework import viewsets, permissions, status
from rest_framework.decorators import action
from rest_framework.response import Response
from .models import (Service, ServiceSubscription, DNSRecord,
                    ProjectTracker, ProjectMilestone, ProjectTask, ProjectComment)
from .serializers import (
    ServiceSerializer, ServiceSubscriptionSerializer, 
    ServiceSubscriptionCreateSerializer, DNSRecordSerializer,
    ProjectTrackerSerializer, ProjectTrackerCreateSerializer,
    ProjectMilestoneSerializer, ProjectTaskSerializer, ProjectCommentSerializer
)


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

