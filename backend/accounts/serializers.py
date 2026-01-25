from rest_framework import serializers
from django.contrib.auth import get_user_model
from django.contrib.auth.password_validation import validate_password

User = get_user_model()


class UserSerializer(serializers.ModelSerializer):
    """Serializer for User model"""
    
    class Meta:
        model = User
        fields = ['id', 'email', 'first_name', 'last_name', 'phone', 'mobile_number', 
                  'company_name', 'user_type', 'is_active', 'email_notifications', 
                  'whatsapp_notifications', 'sms_notifications', 'created_at', 'full_name']
        read_only_fields = ['id', 'created_at', 'full_name']


class UserRegistrationSerializer(serializers.ModelSerializer):
    """Serializer for user registration"""
    
    password = serializers.CharField(write_only=True, required=True, validators=[validate_password])
    password2 = serializers.CharField(write_only=True, required=True)
    referral_code = serializers.CharField(write_only=True, required=False, allow_blank=True)
    
    class Meta:
        model = User
        fields = ['email', 'password', 'password2', 'first_name', 'last_name', 
                  'phone', 'mobile_number', 'company_name', 'user_type', 
                  'email_notifications', 'whatsapp_notifications', 'sms_notifications',
                  'referral_code']
        extra_kwargs = {
            'first_name': {'required': True},
            'last_name': {'required': True},
            'mobile_number': {'required': True},
        }
    
    def validate(self, attrs):
        if attrs['password'] != attrs['password2']:
            raise serializers.ValidationError({"password": "Password fields didn't match."})
        return attrs
    
    def validate_referral_code(self, value):
        if value:
            from referrals.models import ReferralProfile
            try:
                ReferralProfile.objects.get(referral_code=value, is_active=True)
            except ReferralProfile.DoesNotExist:
                raise serializers.ValidationError("Invalid referral code.")
        return value
    
    def create(self, validated_data):
        referral_code = validated_data.pop('referral_code', None)
        validated_data.pop('password2')
        user = User.objects.create_user(**validated_data)
        
        # Handle referral if code was provided
        if referral_code:
            from referrals.models import ReferralProfile, Referral
            from django.utils import timezone
            try:
                referrer_profile = ReferralProfile.objects.get(referral_code=referral_code)
                
                # Create referral profile for new user
                new_user_profile, _ = ReferralProfile.objects.get_or_create(
                    user=user,
                    defaults={'referred_by': referrer_profile}
                )
                
                # Check if there's an existing referral invitation
                referral = Referral.objects.filter(
                    referrer=referrer_profile,
                    referred_email=user.email
                ).first()
                
                if referral:
                    referral.referred_user = user
                    referral.status = 'signed_up'
                    referral.signup_date = timezone.now()
                    referral.save()
                else:
                    # Create a new referral record
                    Referral.objects.create(
                        referrer=referrer_profile,
                        referred_user=user,
                        referred_email=user.email,
                        referral_code_used=referral_code,
                        status='signed_up',
                        signup_date=timezone.now()
                    )
                
                # Update referrer stats
                referrer_profile.successful_referrals += 1
                referrer_profile.save()
            except ReferralProfile.DoesNotExist:
                pass  # Invalid code, just proceed without referral
        
        return user


class ChangePasswordSerializer(serializers.Serializer):
    """Serializer for password change"""
    
    old_password = serializers.CharField(required=True)
    new_password = serializers.CharField(required=True, validators=[validate_password])
    new_password2 = serializers.CharField(required=True)
    
    def validate(self, attrs):
        if attrs['new_password'] != attrs['new_password2']:
            raise serializers.ValidationError({"new_password": "Password fields didn't match."})
        return attrs
