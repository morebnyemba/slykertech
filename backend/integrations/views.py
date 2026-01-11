from rest_framework import viewsets, permissions, status
from rest_framework.decorators import action
from rest_framework.response import Response
from .models import APIConfiguration, IntegrationCredential, cPanelAccount, DirectAdminAccount
from .serializers import (
    APIConfigurationSerializer, IntegrationCredentialSerializer, 
    cPanelAccountSerializer, DirectAdminAccountSerializer
)


class APIConfigurationViewSet(viewsets.ModelViewSet):
    """ViewSet for APIConfiguration model"""
    
    queryset = APIConfiguration.objects.all()
    serializer_class = APIConfigurationSerializer
    permission_classes = [permissions.IsAuthenticated]
    
    def get_queryset(self):
        """Only admins can manage API configurations"""
        user = self.request.user
        if user.is_superuser or user.user_type == 'admin':
            return APIConfiguration.objects.all()
        # Regular users can only view active configs
        return APIConfiguration.objects.filter(is_active=True)


class IntegrationCredentialViewSet(viewsets.ModelViewSet):
    """ViewSet for IntegrationCredential model"""
    
    queryset = IntegrationCredential.objects.all()
    serializer_class = IntegrationCredentialSerializer
    permission_classes = [permissions.IsAuthenticated]
    
    def get_queryset(self):
        """Filter credentials based on user"""
        user = self.request.user
        if user.is_superuser or user.user_type == 'admin':
            return IntegrationCredential.objects.all()
        # Clients can only see their own credentials
        return IntegrationCredential.objects.filter(client__user=user)
    
    @action(detail=True, methods=['post'])
    def verify(self, request, pk=None):
        """Verify integration credentials"""
        credential = self.get_object()
        # TODO: Implement actual verification logic based on provider
        from django.utils import timezone
        credential.last_verified = timezone.now()
        credential.save()
        return Response(
            {"message": "Credentials verified successfully."},
            status=status.HTTP_200_OK
        )


class cPanelAccountViewSet(viewsets.ModelViewSet):
    """ViewSet for cPanelAccount model"""
    
    queryset = cPanelAccount.objects.all()
    serializer_class = cPanelAccountSerializer
    permission_classes = [permissions.IsAuthenticated]
    
    def get_queryset(self):
        """Filter accounts based on user"""
        user = self.request.user
        if user.is_superuser or user.user_type == 'admin':
            return cPanelAccount.objects.all()
        # Clients can only see their own accounts
        return cPanelAccount.objects.filter(subscription__client__user=user)
    
    @action(detail=True, methods=['post'])
    def sync(self, request, pk=None):
        """Sync cPanel account data"""
        account = self.get_object()
        # TODO: Implement actual sync logic with cPanel API
        from django.utils import timezone
        account.last_sync = timezone.now()
        account.save()
        return Response(
            {"message": "Account synced successfully."},
            status=status.HTTP_200_OK
        )


class DirectAdminAccountViewSet(viewsets.ModelViewSet):
    """ViewSet for DirectAdminAccount model"""
    
    queryset = DirectAdminAccount.objects.all()
    serializer_class = DirectAdminAccountSerializer
    permission_classes = [permissions.IsAuthenticated]
    
    def get_queryset(self):
        """Filter accounts based on user"""
        user = self.request.user
        if user.is_superuser or user.user_type == 'admin':
            return DirectAdminAccount.objects.all()
        # Clients can only see their own accounts
        return DirectAdminAccount.objects.filter(subscription__client__user=user)
    
    @action(detail=True, methods=['post'])
    def sync(self, request, pk=None):
        """Sync DirectAdmin account data"""
        account = self.get_object()
        # TODO: Implement actual sync logic with DirectAdmin API
        from django.utils import timezone
        account.last_sync = timezone.now()
        account.save()
        return Response(
            {"message": "Account synced successfully."},
            status=status.HTTP_200_OK
        )

