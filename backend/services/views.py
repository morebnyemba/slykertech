from rest_framework import viewsets, permissions, status
from rest_framework.decorators import action
from rest_framework.response import Response
from .models import Service, ServiceSubscription, DNSRecord
from .serializers import (
    ServiceSerializer, ServiceSubscriptionSerializer, 
    ServiceSubscriptionCreateSerializer, DNSRecordSerializer
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

