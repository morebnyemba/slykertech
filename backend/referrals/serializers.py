from rest_framework import serializers
from django.contrib.auth import get_user_model
from .models import ReferralProfile, Referral, ReferralReward, ReferralSettings

User = get_user_model()


class ReferralProfileSerializer(serializers.ModelSerializer):
    user_email = serializers.EmailField(source='user.email', read_only=True)
    user_name = serializers.CharField(source='user.full_name', read_only=True)
    referred_by_email = serializers.SerializerMethodField()
    referral_link = serializers.SerializerMethodField()
    
    class Meta:
        model = ReferralProfile
        fields = [
            'id', 'user', 'user_email', 'user_name', 'referral_code', 
            'referred_by', 'referred_by_email', 'referral_link',
            'total_referrals', 'successful_referrals', 'total_earnings',
            'is_active', 'created_at', 'updated_at'
        ]
        read_only_fields = ['referral_code', 'total_referrals', 'successful_referrals', 
                           'total_earnings', 'created_at', 'updated_at']
    
    def get_referred_by_email(self, obj):
        if obj.referred_by:
            return obj.referred_by.user.email
        return None
    
    def get_referral_link(self, obj):
        request = self.context.get('request')
        if request:
            return f"{request.scheme}://{request.get_host()}/signup?ref={obj.referral_code}"
        return f"/signup?ref={obj.referral_code}"


class ReferralSerializer(serializers.ModelSerializer):
    referrer_email = serializers.EmailField(source='referrer.user.email', read_only=True)
    referred_user_email = serializers.EmailField(source='referred_user.email', read_only=True)
    status_display = serializers.CharField(source='get_status_display', read_only=True)
    
    class Meta:
        model = Referral
        fields = [
            'id', 'referrer', 'referrer_email', 'referred_user', 'referred_user_email',
            'referred_email', 'referral_code_used', 'status', 'status_display',
            'reward_amount', 'notes', 'signup_date', 'conversion_date', 'reward_date',
            'created_at', 'updated_at'
        ]
        read_only_fields = ['referrer', 'referral_code_used', 'signup_date', 
                           'conversion_date', 'reward_date', 'created_at', 'updated_at']


class CreateReferralSerializer(serializers.Serializer):
    """Serializer for creating a new referral invitation"""
    referred_email = serializers.EmailField()
    notes = serializers.CharField(required=False, allow_blank=True)
    
    def validate_referred_email(self, value):
        # Check if user already exists
        if User.objects.filter(email=value).exists():
            raise serializers.ValidationError("A user with this email already exists.")
        # Check if referral already exists
        request = self.context.get('request')
        if request and hasattr(request.user, 'referral_profile'):
            if Referral.objects.filter(
                referrer=request.user.referral_profile,
                referred_email=value
            ).exists():
                raise serializers.ValidationError("You have already referred this email.")
        return value


class ReferralRewardSerializer(serializers.ModelSerializer):
    user_email = serializers.EmailField(source='referral_profile.user.email', read_only=True)
    reward_type_display = serializers.CharField(source='get_reward_type_display', read_only=True)
    status_display = serializers.CharField(source='get_status_display', read_only=True)
    
    class Meta:
        model = ReferralReward
        fields = [
            'id', 'referral_profile', 'user_email', 'referral', 'reward_type',
            'reward_type_display', 'amount', 'status', 'status_display',
            'description', 'paid_at', 'created_at', 'updated_at'
        ]
        read_only_fields = ['referral_profile', 'paid_at', 'created_at', 'updated_at']


class ReferralSettingsSerializer(serializers.ModelSerializer):
    class Meta:
        model = ReferralSettings
        fields = [
            'id', 'signup_bonus_amount', 'conversion_bonus_amount',
            'conversion_bonus_percentage', 'minimum_payout', 'referral_expiry_days',
            'max_referrals_per_user', 'is_program_active', 'created_at', 'updated_at'
        ]
        read_only_fields = ['created_at', 'updated_at']


class ReferralStatsSerializer(serializers.Serializer):
    """Serializer for referral statistics"""
    total_referrals = serializers.IntegerField()
    pending_referrals = serializers.IntegerField()
    signed_up_referrals = serializers.IntegerField()
    converted_referrals = serializers.IntegerField()
    total_earnings = serializers.DecimalField(max_digits=10, decimal_places=2)
    pending_earnings = serializers.DecimalField(max_digits=10, decimal_places=2)
    referral_code = serializers.CharField()
    referral_link = serializers.CharField()


class ValidateReferralCodeSerializer(serializers.Serializer):
    """Serializer for validating a referral code"""
    referral_code = serializers.CharField()
    
    def validate_referral_code(self, value):
        try:
            profile = ReferralProfile.objects.get(referral_code=value, is_active=True)
            return value
        except ReferralProfile.DoesNotExist:
            raise serializers.ValidationError("Invalid or expired referral code.")
