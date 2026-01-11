from django.contrib import admin
from .models import APIConfiguration, IntegrationCredential, cPanelAccount, DirectAdminAccount


@admin.register(APIConfiguration)
class APIConfigurationAdmin(admin.ModelAdmin):
    list_display = ('provider', 'name', 'is_active', 'is_sandbox', 'created_at')
    list_filter = ('provider', 'is_active', 'is_sandbox')
    search_fields = ('name', 'provider')
    readonly_fields = ('created_at', 'updated_at')
    
    fieldsets = (
        ('Basic Information', {
            'fields': ('provider', 'name', 'is_active', 'is_sandbox')
        }),
        ('API Configuration', {
            'fields': ('api_url',)
        }),
        ('Encrypted Credentials', {
            'fields': ('encrypted_api_key', 'encrypted_api_secret', 'encrypted_access_token'),
            'classes': ('collapse',),
            'description': 'These fields are encrypted. Use set_api_key(), set_api_secret(), and set_access_token() methods.'
        }),
        ('Additional Configuration', {
            'fields': ('config_data',),
            'description': 'JSON field for additional provider-specific configuration'
        }),
        ('Timestamps', {
            'fields': ('created_at', 'updated_at'),
            'classes': ('collapse',)
        }),
    )


@admin.register(IntegrationCredential)
class IntegrationCredentialAdmin(admin.ModelAdmin):
    list_display = ('client', 'provider', 'name', 'host', 'is_active', 'last_verified')
    list_filter = ('provider', 'is_active', 'created_at')
    search_fields = ('client__company_name', 'name', 'host')
    readonly_fields = ('created_at', 'updated_at', 'last_verified')
    
    fieldsets = (
        ('Basic Information', {
            'fields': ('client', 'provider', 'name', 'is_active')
        }),
        ('Connection Details', {
            'fields': ('host', 'port', 'username')
        }),
        ('Credentials', {
            'fields': ('encrypted_password', 'encrypted_api_token'),
            'classes': ('collapse',),
            'description': 'These fields are encrypted. Use set_password() and set_api_token() methods.'
        }),
        ('Additional', {
            'fields': ('metadata', 'last_verified')
        }),
        ('Timestamps', {
            'fields': ('created_at', 'updated_at'),
            'classes': ('collapse',)
        }),
    )


@admin.register(cPanelAccount)
class cPanelAccountAdmin(admin.ModelAdmin):
    list_display = ('domain', 'cpanel_username', 'credential', 'disk_used', 'bandwidth_used', 'is_suspended')
    list_filter = ('is_suspended', 'created_at')
    search_fields = ('domain', 'cpanel_username', 'credential__client__company_name')
    readonly_fields = ('created_at', 'updated_at', 'last_sync')


@admin.register(DirectAdminAccount)
class DirectAdminAccountAdmin(admin.ModelAdmin):
    list_display = ('domain', 'da_username', 'credential', 'disk_used', 'bandwidth_used', 'is_suspended')
    list_filter = ('is_suspended', 'created_at')
    search_fields = ('domain', 'da_username', 'credential__client__company_name')
    readonly_fields = ('created_at', 'updated_at', 'last_sync')

