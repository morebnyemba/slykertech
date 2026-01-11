from django.contrib import admin
from .models import Notification, WhatsAppMessage, NotificationTemplate, NotificationPreference


@admin.register(Notification)
class NotificationAdmin(admin.ModelAdmin):
    list_display = ('title', 'user', 'notification_type', 'category', 'status', 'created_at')
    list_filter = ('notification_type', 'category', 'status', 'created_at')
    search_fields = ('title', 'message', 'user__email')
    readonly_fields = ('sent_at', 'delivered_at', 'read_at', 'created_at')
    
    fieldsets = (
        ('Basic Information', {
            'fields': ('user', 'notification_type', 'category', 'status')
        }),
        ('Content', {
            'fields': ('title', 'message', 'link_url', 'link_text')
        }),
        ('Metadata', {
            'fields': ('metadata',)
        }),
        ('Timestamps', {
            'fields': ('sent_at', 'delivered_at', 'read_at', 'created_at'),
            'classes': ('collapse',)
        }),
    )


@admin.register(WhatsAppMessage)
class WhatsAppMessageAdmin(admin.ModelAdmin):
    list_display = ('recipient', 'status', 'message_id', 'sent_at', 'created_at')
    list_filter = ('status', 'created_at')
    search_fields = ('recipient', 'message', 'message_id')
    readonly_fields = ('message_id', 'sent_at', 'delivered_at', 'read_at', 'created_at', 'updated_at')


@admin.register(NotificationTemplate)
class NotificationTemplateAdmin(admin.ModelAdmin):
    list_display = ('name', 'notification_type', 'category', 'is_active', 'created_at')
    list_filter = ('notification_type', 'category', 'is_active')
    search_fields = ('name', 'subject', 'template')
    readonly_fields = ('created_at', 'updated_at')


@admin.register(NotificationPreference)
class NotificationPreferenceAdmin(admin.ModelAdmin):
    list_display = ('user', 'email_enabled', 'whatsapp_enabled', 'sms_enabled', 'in_app_enabled')
    list_filter = ('email_enabled', 'whatsapp_enabled', 'sms_enabled')
    search_fields = ('user__email',)
    readonly_fields = ('created_at', 'updated_at')
    
    fieldsets = (
        ('User', {
            'fields': ('user',)
        }),
        ('Channel Preferences', {
            'fields': ('email_enabled', 'whatsapp_enabled', 'sms_enabled', 'in_app_enabled')
        }),
        ('Category Preferences', {
            'fields': ('invoice_notifications', 'payment_notifications', 'service_notifications',
                      'account_notifications', 'system_notifications', 'marketing_notifications')
        }),
        ('Timestamps', {
            'fields': ('created_at', 'updated_at'),
            'classes': ('collapse',)
        }),
    )

