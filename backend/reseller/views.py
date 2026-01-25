from rest_framework import viewsets, status
from rest_framework.decorators import action
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated, AllowAny
from django.db.models import Sum, Count
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
    
    @action(detail=False, methods=['get'])
    def my_profile(self, request):
        """Get the current user's partner profile"""
        try:
            profile = PartnerProfile.objects.get(user=request.user)
            serializer = self.get_serializer(profile)
            return Response(serializer.data)
        except PartnerProfile.DoesNotExist:
            return Response({'detail': 'Partner profile not found'}, status=status.HTTP_404_NOT_FOUND)
    
    @action(detail=False, methods=['post'])
    def apply(self, request):
        """Apply to become a partner"""
        partner_type = request.data.get('partner_type', 'reseller')
        company_name = request.data.get('company_name', request.user.company_name or '')
        
        if PartnerProfile.objects.filter(user=request.user).exists():
            return Response({'detail': 'You already have a partner profile'}, status=status.HTTP_400_BAD_REQUEST)
        
        profile = PartnerProfile.objects.create(
            user=request.user,
            partner_type=partner_type,
            company_name=company_name,
            tier='bronze',
            is_active=False  # Pending approval
        )
        
        # Create type-specific profile
        if partner_type == 'reseller':
            ResellerProfile.objects.create(partner=profile)
        elif partner_type == 'agency':
            AgencyPartner.objects.create(
                partner=profile,
                specialization=request.data.get('specialization', 'General')
            )
        elif partner_type == 'technology':
            TechnologyAlliance.objects.create(
                partner=profile,
                product_name=request.data.get('product_name', ''),
                integration_type=request.data.get('integration_type', 'API')
            )
        
        serializer = self.get_serializer(profile)
        return Response(serializer.data, status=status.HTTP_201_CREATED)
    
    @action(detail=False, methods=['get'])
    def dashboard_stats(self, request):
        """Get partner dashboard statistics"""
        try:
            profile = PartnerProfile.objects.get(user=request.user)
            
            stats = {
                'partner_type': profile.partner_type,
                'tier': profile.tier,
                'is_active': profile.is_active,
                'company_name': profile.company_name,
            }
            
            if profile.partner_type == 'reseller':
                try:
                    reseller = profile.reseller_data
                    clients_count = ResellerClient.objects.filter(reseller=reseller).count()
                    commissions = ResellerCommission.objects.filter(reseller=reseller)
                    total_earnings = commissions.filter(status='paid').aggregate(total=Sum('amount'))['total'] or 0
                    pending_earnings = commissions.filter(status='pending').aggregate(total=Sum('amount'))['total'] or 0
                    
                    stats.update({
                        'total_clients': clients_count,
                        'max_clients': reseller.max_clients,
                        'discount_percentage': str(reseller.discount_percentage),
                        'commission_percentage': str(reseller.commission_percentage),
                        'total_earnings': str(total_earnings),
                        'pending_earnings': str(pending_earnings),
                        'api_rate_limit': reseller.api_rate_limit,
                    })
                except ResellerProfile.DoesNotExist:
                    pass
                    
            elif profile.partner_type == 'agency':
                try:
                    agency = profile.agency_data
                    referrals = AgencyReferral.objects.filter(agency=agency)
                    total_bonus = referrals.filter(status='paid').aggregate(total=Sum('bonus_amount'))['total'] or 0
                    pending_bonus = referrals.filter(status__in=['pending', 'qualified', 'converted']).aggregate(total=Sum('bonus_amount'))['total'] or 0
                    
                    stats.update({
                        'specialization': agency.specialization,
                        'total_referrals': agency.total_referrals,
                        'active_referrals': agency.active_referrals,
                        'referral_bonus_percentage': str(agency.referral_bonus_percentage),
                        'total_bonus_earned': str(total_bonus),
                        'pending_bonus': str(pending_bonus),
                    })
                except AgencyPartner.DoesNotExist:
                    pass
                    
            elif profile.partner_type == 'technology':
                try:
                    tech = profile.technology_data
                    stats.update({
                        'product_name': tech.product_name,
                        'integration_type': tech.integration_type,
                        'revenue_share_percentage': str(tech.revenue_share_percentage),
                    })
                except TechnologyAlliance.DoesNotExist:
                    pass
            
            return Response(stats)
            
        except PartnerProfile.DoesNotExist:
            return Response({'detail': 'Partner profile not found'}, status=status.HTTP_404_NOT_FOUND)

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
    
    @action(detail=False, methods=['get'])
    def stats(self, request):
        """Get reseller dashboard statistics"""
        try:
            partner = request.user.partner_profile
            reseller = partner.reseller_data
            
            clients = ResellerClient.objects.filter(reseller=reseller)
            commissions = ResellerCommission.objects.filter(reseller=reseller)
            
            stats = {
                'totalClients': clients.count(),
                'maxClients': reseller.max_clients,
                'activeServices': 0,  # TODO: Link to actual services
                'monthlyRevenue': float(commissions.filter(status='paid').aggregate(total=Sum('amount'))['total'] or 0),
                'pendingCommissions': float(commissions.filter(status='pending').aggregate(total=Sum('amount'))['total'] or 0),
                'walletBalance': 0,  # TODO: Link to actual wallet
                'discountPercentage': float(reseller.discount_percentage),
                'commissionPercentage': float(reseller.commission_percentage),
                'tier': reseller.tier,
                'apiRateLimit': reseller.api_rate_limit,
            }
            
            return Response(stats)
        except Exception:
            return Response({
                'totalClients': 0,
                'maxClients': 50,
                'activeServices': 0,
                'monthlyRevenue': 0,
                'pendingCommissions': 0,
                'walletBalance': 0,
            })
    
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
