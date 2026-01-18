from rest_framework import viewsets, status
from rest_framework.decorators import action
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated
from .models import (
    PartnerProfile, ResellerProfile, ResellerClient, ResellerCommission,
    AgencyPartner, AgencyReferral, TechnologyAlliance
)
from .serializers import (
    PartnerProfileSerializer, ResellerProfileSerializer, ResellerClientSerializer,
    ResellerCommissionSerializer, AgencyPartnerSerializer, AgencyReferralSerializer,
    TechnologyAllianceSerializer
)

class PartnerProfileViewSet(viewsets.ModelViewSet):
    queryset = PartnerProfile.objects.all()
    serializer_class = PartnerProfileSerializer
    permission_classes = [IsAuthenticated]
    
    def get_queryset(self):
        if self.request.user.is_staff:
            return PartnerProfile.objects.all()
        return PartnerProfile.objects.filter(user=self.request.user)

class ResellerProfileViewSet(viewsets.ModelViewSet):
    queryset = ResellerProfile.objects.all()
    serializer_class = ResellerProfileSerializer
    permission_classes = [IsAuthenticated]
    
    def get_queryset(self):
        if self.request.user.is_staff:
            return ResellerProfile.objects.all()
        try:
            partner = self.request.user.partner_profile
            if partner.partner_type == 'reseller':
                return ResellerProfile.objects.filter(partner=partner)
        except Exception:
            pass
        return ResellerProfile.objects.none()
    
    @action(detail=True, methods=['post'])
    def provision_service(self, request, pk=None):
        # Service provisioning logic here
        return Response({'status': 'Service provisioning initiated'})

class AgencyPartnerViewSet(viewsets.ModelViewSet):
    queryset = AgencyPartner.objects.all()
    serializer_class = AgencyPartnerSerializer
    permission_classes = [IsAuthenticated]
    
    def get_queryset(self):
        if self.request.user.is_staff:
            return AgencyPartner.objects.all()
        try:
            partner = self.request.user.partner_profile
            if partner.partner_type == 'agency':
                return AgencyPartner.objects.filter(partner=partner)
        except Exception:
            pass
        return AgencyPartner.objects.none()

class TechnologyAllianceViewSet(viewsets.ModelViewSet):
    queryset = TechnologyAlliance.objects.all()
    serializer_class = TechnologyAllianceSerializer
    permission_classes = [IsAuthenticated]
    
    def get_queryset(self):
        if self.request.user.is_staff:
            return TechnologyAlliance.objects.all()
        try:
            partner = self.request.user.partner_profile
            if partner.partner_type == 'technology':
                return TechnologyAlliance.objects.filter(partner=partner)
        except Exception:
            pass
        return TechnologyAlliance.objects.none()

class ResellerClientViewSet(viewsets.ModelViewSet):
    queryset = ResellerClient.objects.all()
    serializer_class = ResellerClientSerializer
    permission_classes = [IsAuthenticated]
    
    def get_queryset(self):
        if self.request.user.is_staff:
            return ResellerClient.objects.all()
        try:
            partner = self.request.user.partner_profile
            if partner.partner_type == 'reseller':
                reseller = partner.reseller_data
                return ResellerClient.objects.filter(reseller=reseller)
        except Exception:
            pass
        return ResellerClient.objects.none()

class ResellerCommissionViewSet(viewsets.ReadOnlyModelViewSet):
    queryset = ResellerCommission.objects.all()
    serializer_class = ResellerCommissionSerializer
    permission_classes = [IsAuthenticated]
    
    def get_queryset(self):
        if self.request.user.is_staff:
            return ResellerCommission.objects.all()
        try:
            partner = self.request.user.partner_profile
            if partner.partner_type == 'reseller':
                reseller = partner.reseller_data
                return ResellerCommission.objects.filter(reseller=reseller)
        except Exception:
            pass
        return ResellerCommission.objects.none()

class AgencyReferralViewSet(viewsets.ModelViewSet):
    queryset = AgencyReferral.objects.all()
    serializer_class = AgencyReferralSerializer
    permission_classes = [IsAuthenticated]
    
    def get_queryset(self):
        if self.request.user.is_staff:
            return AgencyReferral.objects.all()
        try:
            partner = self.request.user.partner_profile
            if partner.partner_type == 'agency':
                agency = partner.agency_data
                return AgencyReferral.objects.filter(agency=agency)
        except Exception:
            pass
        return AgencyReferral.objects.none()
