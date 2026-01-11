from rest_framework import viewsets, status
from rest_framework.decorators import action
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated
from .models import ResellerProfile, ResellerClient, ResellerCommission
from .serializers import ResellerProfileSerializer, ResellerClientSerializer, ResellerCommissionSerializer

class ResellerProfileViewSet(viewsets.ModelViewSet):
    queryset = ResellerProfile.objects.all()
    serializer_class = ResellerProfileSerializer
    permission_classes = [IsAuthenticated]
    
    def get_queryset(self):
        if self.request.user.is_staff:
            return ResellerProfile.objects.all()
        return ResellerProfile.objects.filter(user=self.request.user)
    
    @action(detail=True, methods=['post'])
    def provision_service(self, request, pk=None):
        reseller = self.get_object()
        # Service provisioning logic here
        return Response({'status': 'Service provisioning initiated'})

class ResellerClientViewSet(viewsets.ModelViewSet):
    queryset = ResellerClient.objects.all()
    serializer_class = ResellerClientSerializer
    permission_classes = [IsAuthenticated]
    
    def get_queryset(self):
        if self.request.user.is_staff:
            return ResellerClient.objects.all()
        try:
            reseller = self.request.user.reseller_profile
            return ResellerClient.objects.filter(reseller=reseller)
        except:
            return ResellerClient.objects.none()

class ResellerCommissionViewSet(viewsets.ReadOnlyModelViewSet):
    queryset = ResellerCommission.objects.all()
    serializer_class = ResellerCommissionSerializer
    permission_classes = [IsAuthenticated]
    
    def get_queryset(self):
        if self.request.user.is_staff:
            return ResellerCommission.objects.all()
        try:
            reseller = self.request.user.reseller_profile
            return ResellerCommission.objects.filter(reseller=reseller)
        except:
            return ResellerCommission.objects.none()
