from rest_framework import serializers
from .models import ResellerProfile, ResellerClient, ResellerCommission
from django.contrib.auth import get_user_model

User = get_user_model()

class ResellerProfileSerializer(serializers.ModelSerializer):
    user_email = serializers.EmailField(source='user.email', read_only=True)
    
    class Meta:
        model = ResellerProfile
        fields = [
            'id', 'user', 'user_email', 'tier', 'discount_percentage',
            'commission_percentage', 'api_key', 'api_secret', 'api_rate_limit',
            'max_clients', 'branding_name', 'webhook_url', 'is_active',
            'created_at', 'updated_at'
        ]
        read_only_fields = ['api_key', 'api_secret', 'created_at', 'updated_at']
        extra_kwargs = {
            'api_secret': {'write_only': True}
        }

class ResellerClientSerializer(serializers.ModelSerializer):
    client_email = serializers.EmailField(source='client.email', read_only=True)
    reseller_name = serializers.CharField(source='reseller.branding_name', read_only=True)
    
    class Meta:
        model = ResellerClient
        fields = [
            'id', 'reseller', 'reseller_name', 'client', 'client_email',
            'custom_pricing', 'notes', 'created_at'
        ]
        read_only_fields = ['created_at']

class ResellerCommissionSerializer(serializers.ModelSerializer):
    reseller_email = serializers.EmailField(source='reseller.user.email', read_only=True)
    subscription_id = serializers.IntegerField(source='subscription.id', read_only=True)
    
    class Meta:
        model = ResellerCommission
        fields = [
            'id', 'reseller', 'reseller_email', 'subscription', 'subscription_id',
            'amount', 'status', 'paid_at', 'created_at'
        ]
        read_only_fields = ['created_at', 'paid_at']
