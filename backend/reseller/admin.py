from django.contrib import admin
from .models import ResellerProfile, ResellerClient, ResellerCommission

@admin.register(ResellerProfile)
class ResellerProfileAdmin(admin.ModelAdmin):
    list_display = ['user', 'tier', 'discount_percentage', 'commission_percentage', 'is_active']
    list_filter = ['tier', 'is_active']
    search_fields = ['user__email', 'branding_name']
    readonly_fields = ['api_key', 'api_secret', 'created_at', 'updated_at']

@admin.register(ResellerClient)
class ResellerClientAdmin(admin.ModelAdmin):
    list_display = ['reseller', 'client', 'created_at']
    list_filter = ['created_at']
    search_fields = ['reseller__user__email', 'client__email']

@admin.register(ResellerCommission)
class ResellerCommissionAdmin(admin.ModelAdmin):
    list_display = ['reseller', 'amount', 'status', 'created_at', 'paid_at']
    list_filter = ['status', 'created_at']
    search_fields = ['reseller__user__email']
