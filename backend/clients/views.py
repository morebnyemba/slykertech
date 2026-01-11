from rest_framework import viewsets, permissions
from .models import Client, ClientContact
from .serializers import ClientSerializer, ClientContactSerializer, ClientCreateSerializer


class ClientViewSet(viewsets.ModelViewSet):
    """ViewSet for Client model"""
    
    queryset = Client.objects.all()
    serializer_class = ClientSerializer
    permission_classes = [permissions.IsAuthenticated]
    
    def get_queryset(self):
        """Filter queryset based on user type"""
        user = self.request.user
        if user.is_superuser or user.user_type == 'admin':
            return Client.objects.all()
        # Clients can only see their own profile
        return Client.objects.filter(user=user)
    
    def get_serializer_class(self):
        if self.action == 'create':
            return ClientCreateSerializer
        return ClientSerializer


class ClientContactViewSet(viewsets.ModelViewSet):
    """ViewSet for ClientContact model"""
    
    queryset = ClientContact.objects.all()
    serializer_class = ClientContactSerializer
    permission_classes = [permissions.IsAuthenticated]
    
    def get_queryset(self):
        """Filter queryset based on user"""
        user = self.request.user
        if user.is_superuser or user.user_type == 'admin':
            return ClientContact.objects.all()
        # Clients can only see their own contacts
        return ClientContact.objects.filter(client__user=user)

