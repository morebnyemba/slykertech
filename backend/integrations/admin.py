from django.contrib import admin
from .models import IntegrationCredential, cPanelAccount, DirectAdminAccount


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

