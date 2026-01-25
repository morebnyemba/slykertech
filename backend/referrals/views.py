from rest_framework import viewsets, status, permissions
from rest_framework.decorators import action
from rest_framework.response import Response
from django.db.models import Sum, Count, Q
from django.utils import timezone
from .models import ReferralProfile, Referral, ReferralReward, ReferralSettings
from .serializers import (
    ReferralProfileSerializer, ReferralSerializer, CreateReferralSerializer,
    ReferralRewardSerializer, ReferralSettingsSerializer, ReferralStatsSerializer,
    ValidateReferralCodeSerializer
)


class ReferralProfileViewSet(viewsets.ModelViewSet):
    """ViewSet for managing referral profiles"""
    
    queryset = ReferralProfile.objects.all()
    serializer_class = ReferralProfileSerializer
    permission_classes = [permissions.IsAuthenticated]
    
    def get_queryset(self):
        user = self.request.user
        if user.is_staff:
            return ReferralProfile.objects.all()
        return ReferralProfile.objects.filter(user=user)
    
    @action(detail=False, methods=['get'])
    def my_profile(self, request):
        """Get or create the current user's referral profile"""
        profile, created = ReferralProfile.objects.get_or_create(user=request.user)
        serializer = self.get_serializer(profile)
        return Response(serializer.data)
    
    @action(detail=False, methods=['get'])
    def stats(self, request):
        """Get referral statistics for the current user"""
        try:
            profile = request.user.referral_profile
        except ReferralProfile.DoesNotExist:
            profile = ReferralProfile.objects.create(user=request.user)
        
        referrals = Referral.objects.filter(referrer=profile)
        pending_rewards = ReferralReward.objects.filter(
            referral_profile=profile,
            status='pending'
        ).aggregate(total=Sum('amount'))['total'] or 0
        
        stats = {
            'total_referrals': referrals.count(),
            'pending_referrals': referrals.filter(status='pending').count(),
            'signed_up_referrals': referrals.filter(status='signed_up').count(),
            'converted_referrals': referrals.filter(status='converted').count(),
            'total_earnings': profile.total_earnings,
            'pending_earnings': pending_rewards,
            'referral_code': profile.referral_code,
            'referral_link': f"/signup?ref={profile.referral_code}",
        }
        
        serializer = ReferralStatsSerializer(stats)
        return Response(serializer.data)
    
    @action(detail=False, methods=['get'])
    def leaderboard(self, request):
        """Get top referrers leaderboard"""
        top_referrers = ReferralProfile.objects.filter(
            is_active=True,
            successful_referrals__gt=0
        ).order_by('-successful_referrals')[:10]
        
        serializer = self.get_serializer(top_referrers, many=True)
        return Response(serializer.data)


class ReferralViewSet(viewsets.ModelViewSet):
    """ViewSet for managing referrals"""
    
    queryset = Referral.objects.all()
    serializer_class = ReferralSerializer
    permission_classes = [permissions.IsAuthenticated]
    
    def get_queryset(self):
        user = self.request.user
        if user.is_staff:
            return Referral.objects.all()
        try:
            profile = user.referral_profile
            return Referral.objects.filter(referrer=profile)
        except ReferralProfile.DoesNotExist:
            return Referral.objects.none()
    
    def create(self, request, *args, **kwargs):
        """Create a new referral invitation"""
        serializer = CreateReferralSerializer(data=request.data, context={'request': request})
        serializer.is_valid(raise_exception=True)
        
        # Get or create referral profile
        profile, _ = ReferralProfile.objects.get_or_create(user=request.user)
        
        # Check max referrals limit
        try:
            settings = ReferralSettings.objects.first()
            if settings and settings.max_referrals_per_user > 0:
                if profile.total_referrals >= settings.max_referrals_per_user:
                    return Response(
                        {"error": "You have reached the maximum number of referrals allowed."},
                        status=status.HTTP_400_BAD_REQUEST
                    )
        except ReferralSettings.DoesNotExist:
            pass
        
        # Create referral
        referral = Referral.objects.create(
            referrer=profile,
            referred_email=serializer.validated_data['referred_email'],
            referral_code_used=profile.referral_code,
            notes=serializer.validated_data.get('notes', '')
        )
        
        # Update total referrals count
        profile.total_referrals += 1
        profile.save()
        
        return Response(
            ReferralSerializer(referral).data,
            status=status.HTTP_201_CREATED
        )
    
    @action(detail=False, methods=['get'])
    def my_referrals(self, request):
        """Get all referrals made by the current user"""
        try:
            profile = request.user.referral_profile
            referrals = Referral.objects.filter(referrer=profile)
            serializer = self.get_serializer(referrals, many=True)
            return Response(serializer.data)
        except ReferralProfile.DoesNotExist:
            return Response([])
    
    @action(detail=False, methods=['post'], permission_classes=[permissions.AllowAny])
    def validate_code(self, request):
        """Validate a referral code"""
        serializer = ValidateReferralCodeSerializer(data=request.data)
        serializer.is_valid(raise_exception=True)
        
        code = serializer.validated_data['referral_code']
        profile = ReferralProfile.objects.get(referral_code=code)
        
        return Response({
            'valid': True,
            'referrer_name': profile.user.full_name or profile.user.email.split('@')[0]
        })


class ReferralRewardViewSet(viewsets.ReadOnlyModelViewSet):
    """ViewSet for viewing referral rewards"""
    
    queryset = ReferralReward.objects.all()
    serializer_class = ReferralRewardSerializer
    permission_classes = [permissions.IsAuthenticated]
    
    def get_queryset(self):
        user = self.request.user
        if user.is_staff:
            return ReferralReward.objects.all()
        try:
            profile = user.referral_profile
            return ReferralReward.objects.filter(referral_profile=profile)
        except ReferralProfile.DoesNotExist:
            return ReferralReward.objects.none()
    
    @action(detail=False, methods=['get'])
    def my_rewards(self, request):
        """Get all rewards for the current user"""
        try:
            profile = request.user.referral_profile
            rewards = ReferralReward.objects.filter(referral_profile=profile)
            serializer = self.get_serializer(rewards, many=True)
            return Response(serializer.data)
        except ReferralProfile.DoesNotExist:
            return Response([])
    
    @action(detail=False, methods=['get'])
    def pending(self, request):
        """Get pending rewards for the current user"""
        try:
            profile = request.user.referral_profile
            rewards = ReferralReward.objects.filter(referral_profile=profile, status='pending')
            serializer = self.get_serializer(rewards, many=True)
            return Response(serializer.data)
        except ReferralProfile.DoesNotExist:
            return Response([])


class ReferralSettingsViewSet(viewsets.ModelViewSet):
    """ViewSet for managing referral settings (admin only)"""
    
    queryset = ReferralSettings.objects.all()
    serializer_class = ReferralSettingsSerializer
    permission_classes = [permissions.IsAdminUser]
    
    @action(detail=False, methods=['get'], permission_classes=[permissions.AllowAny])
    def public(self, request):
        """Get public referral program settings"""
        try:
            settings = ReferralSettings.objects.first()
            if settings:
                return Response({
                    'is_program_active': settings.is_program_active,
                    'signup_bonus_amount': str(settings.signup_bonus_amount),
                    'conversion_bonus_percentage': str(settings.conversion_bonus_percentage),
                })
            return Response({
                'is_program_active': True,
                'signup_bonus_amount': '10.00',
                'conversion_bonus_percentage': '5.00',
            })
        except Exception:
            return Response({
                'is_program_active': True,
                'signup_bonus_amount': '10.00',
                'conversion_bonus_percentage': '5.00',
            })
