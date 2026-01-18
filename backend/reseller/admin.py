from django.contrib import admin
from .models import (
    PartnerProfile, ResellerProfile, ResellerClient, ResellerCommission,
    AgencyPartner, AgencyReferral, TechnologyAlliance
)

@admin.register(PartnerProfile)
class PartnerProfileAdmin(admin.ModelAdmin):
    list_display = ['company_name', 'user', 'partner_type', 'tier', 'is_active', 'created_at']
    list_filter = ['partner_type', 'tier', 'is_active', 'created_at']
    search_fields = ['company_name', 'user__email']
    readonly_fields = ['created_at', 'updated_at']

@admin.register(ResellerProfile)
class ResellerProfileAdmin(admin.ModelAdmin):
    list_display = ['partner', 'tier', 'discount_percentage', 'commission_percentage', 'max_clients']
    list_filter = ['tier']
    search_fields = ['partner__company_name', 'partner__user__email', 'branding_name']
    readonly_fields = ['api_key', 'api_secret']

@admin.register(AgencyPartner)
class AgencyPartnerAdmin(admin.ModelAdmin):
    list_display = ['partner', 'specialization', 'active_referrals', 'total_referrals']
    search_fields = ['partner__company_name', 'specialization']

@admin.register(TechnologyAlliance)
class TechnologyAllianceAdmin(admin.ModelAdmin):
    list_display = ['partner', 'product_name', 'integration_type', 'revenue_share_percentage']
    search_fields = ['partner__company_name', 'product_name']
    readonly_fields = ['api_key']

@admin.register(ResellerClient)
class ResellerClientAdmin(admin.ModelAdmin):
    list_display = ['reseller', 'client', 'created_at']
    list_filter = ['created_at']
    search_fields = ['reseller__partner__company_name', 'client__email']

@admin.register(ResellerCommission)
class ResellerCommissionAdmin(admin.ModelAdmin):
    list_display = ['reseller', 'amount', 'status', 'created_at', 'paid_at']
    list_filter = ['status', 'created_at']
    search_fields = ['reseller__partner__company_name']

@admin.register(AgencyReferral)
class AgencyReferralAdmin(admin.ModelAdmin):
    list_display = ['agency', 'referred_client', 'status', 'bonus_amount', 'created_at']
    list_filter = ['status', 'created_at']
    search_fields = ['agency__partner__company_name', 'referred_client__email']
