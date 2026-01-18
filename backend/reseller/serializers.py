from rest_framework import serializers
from .models import (
    PartnerProfile, ResellerProfile, ResellerClient, ResellerCommission,
    AgencyPartner, AgencyReferral, TechnologyAlliance
)
from django.contrib.auth import get_user_model

User = get_user_model()

class PartnerProfileSerializer(serializers.ModelSerializer):
    user_email = serializers.EmailField(source='user.email', read_only=True)
    partner_type_display = serializers.CharField(source='get_partner_type_display', read_only=True)
    
    class Meta:
        model = PartnerProfile
        fields = [
            'id', 'user', 'user_email', 'partner_type', 'partner_type_display',
            'tier', 'company_name', 'is_active', 'created_at', 'updated_at'
        ]
        read_only_fields = ['created_at', 'updated_at']

class ResellerProfileSerializer(serializers.ModelSerializer):
    user_email = serializers.EmailField(source='partner.user.email', read_only=True)
    company_name = serializers.CharField(source='partner.company_name', read_only=True)
    
    class Meta:
        model = ResellerProfile
        fields = [
            'id', 'partner', 'user_email', 'company_name', 'tier', 'discount_percentage',
            'commission_percentage', 'api_key', 'api_secret', 'api_rate_limit',
            'max_clients', 'branding_name', 'webhook_url',
        ]
        read_only_fields = ['api_key', 'api_secret']
        extra_kwargs = {
            'api_secret': {'write_only': True}
        }

class AgencyPartnerSerializer(serializers.ModelSerializer):
    user_email = serializers.EmailField(source='partner.user.email', read_only=True)
    company_name = serializers.CharField(source='partner.company_name', read_only=True)
    
    class Meta:
        model = AgencyPartner
        fields = [
            'id', 'partner', 'user_email', 'company_name', 'specialization',
            'referral_bonus_percentage', 'active_referrals', 'total_referrals'
        ]

class TechnologyAllianceSerializer(serializers.ModelSerializer):
    user_email = serializers.EmailField(source='partner.user.email', read_only=True)
    company_name = serializers.CharField(source='partner.company_name', read_only=True)
    
    class Meta:
        model = TechnologyAlliance
        fields = [
            'id', 'partner', 'user_email', 'company_name', 'product_name',
            'integration_type', 'api_key', 'revenue_share_percentage'
        ]
        read_only_fields = ['api_key']
        extra_kwargs = {
            'api_key': {'write_only': True}
        }

class ResellerClientSerializer(serializers.ModelSerializer):
    client_email = serializers.EmailField(source='client.email', read_only=True)
    reseller_name = serializers.CharField(source='reseller.partner.company_name', read_only=True)
    
    class Meta:
        model = ResellerClient
        fields = [
            'id', 'reseller', 'reseller_name', 'client', 'client_email',
            'custom_pricing', 'notes', 'created_at'
        ]
        read_only_fields = ['created_at']

class ResellerCommissionSerializer(serializers.ModelSerializer):
    reseller_name = serializers.CharField(source='reseller.partner.company_name', read_only=True)
    subscription_id = serializers.IntegerField(source='subscription.id', read_only=True)
    
    class Meta:
        model = ResellerCommission
        fields = [
            'id', 'reseller', 'reseller_name', 'subscription', 'subscription_id',
            'amount', 'status', 'paid_at', 'created_at'
        ]
        read_only_fields = ['created_at', 'paid_at']

class AgencyReferralSerializer(serializers.ModelSerializer):
    agency_name = serializers.CharField(source='agency.partner.company_name', read_only=True)
    client_email = serializers.EmailField(source='referred_client.email', read_only=True)
    
    class Meta:
        model = AgencyReferral
        fields = [
            'id', 'agency', 'agency_name', 'referred_client', 'client_email',
            'project_description', 'estimated_value', 'bonus_amount',
            'status', 'paid_at', 'created_at', 'updated_at'
        ]
        read_only_fields = ['created_at', 'updated_at', 'paid_at']

