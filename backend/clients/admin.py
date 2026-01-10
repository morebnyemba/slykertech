from django.contrib import admin
from .models import Client, ClientContact


class ClientContactInline(admin.TabularInline):
    model = ClientContact
    extra = 1


@admin.register(Client)
class ClientAdmin(admin.ModelAdmin):
    list_display = ('company_name', 'user', 'city', 'country', 'is_active', 'created_at')
    list_filter = ('is_active', 'country', 'created_at')
    search_fields = ('company_name', 'user__email', 'city', 'country')
    readonly_fields = ('created_at', 'updated_at')
    inlines = [ClientContactInline]
    
    fieldsets = (
        ('Basic Information', {
            'fields': ('user', 'company_name', 'company_website')
        }),
        ('Contact Details', {
            'fields': ('address', 'city', 'country', 'billing_email')
        }),
        ('Business Details', {
            'fields': ('tax_id', 'is_active', 'notes')
        }),
        ('Timestamps', {
            'fields': ('created_at', 'updated_at'),
            'classes': ('collapse',)
        }),
    )


@admin.register(ClientContact)
class ClientContactAdmin(admin.ModelAdmin):
    list_display = ('name', 'client', 'email', 'phone', 'position', 'is_primary')
    list_filter = ('is_primary', 'created_at')
    search_fields = ('name', 'email', 'client__company_name')

