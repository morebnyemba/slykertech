from django.contrib import admin
from .models import Service, ServiceSubscription, DNSRecord


@admin.register(Service)
class ServiceAdmin(admin.ModelAdmin):
    list_display = ('name', 'category', 'base_price', 'is_active', 'created_at')
    list_filter = ('category', 'is_active', 'created_at')
    search_fields = ('name', 'description')
    readonly_fields = ('created_at', 'updated_at')


class DNSRecordInline(admin.TabularInline):
    model = DNSRecord
    extra = 1


@admin.register(ServiceSubscription)
class ServiceSubscriptionAdmin(admin.ModelAdmin):
    list_display = ('client', 'service', 'status', 'billing_cycle', 'start_date', 'end_date', 'price')
    list_filter = ('status', 'billing_cycle', 'auto_renew', 'created_at')
    search_fields = ('client__company_name', 'service__name')
    readonly_fields = ('created_at', 'updated_at')
    inlines = [DNSRecordInline]
    
    fieldsets = (
        ('Subscription Details', {
            'fields': ('client', 'service', 'status', 'billing_cycle', 'price')
        }),
        ('Dates', {
            'fields': ('start_date', 'end_date', 'auto_renew')
        }),
        ('Additional Information', {
            'fields': ('notes', 'metadata')
        }),
        ('Timestamps', {
            'fields': ('created_at', 'updated_at'),
            'classes': ('collapse',)
        }),
    )


@admin.register(DNSRecord)
class DNSRecordAdmin(admin.ModelAdmin):
    list_display = ('domain', 'record_type', 'name', 'content', 'ttl', 'is_active')
    list_filter = ('record_type', 'is_active', 'created_at')
    search_fields = ('domain', 'name', 'content')
    readonly_fields = ('created_at', 'updated_at')

