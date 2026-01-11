from django.db import models
from django.conf import settings
from django.utils.translation import gettext_lazy as _


class Notification(models.Model):
    """General notification model"""
    
    TYPE_CHOICES = [
        ('email', 'Email'),
        ('whatsapp', 'WhatsApp'),
        ('sms', 'SMS'),
        ('in_app', 'In-App'),
    ]
    
    STATUS_CHOICES = [
        ('pending', 'Pending'),
        ('sent', 'Sent'),
        ('delivered', 'Delivered'),
        ('failed', 'Failed'),
        ('read', 'Read'),
    ]
    
    CATEGORY_CHOICES = [
        ('invoice', 'Invoice'),
        ('payment', 'Payment'),
        ('service', 'Service'),
        ('account', 'Account'),
        ('system', 'System'),
        ('marketing', 'Marketing'),
    ]
    
    user = models.ForeignKey(settings.AUTH_USER_MODEL, on_delete=models.CASCADE, related_name='notifications')
    notification_type = models.CharField(max_length=20, choices=TYPE_CHOICES)
    category = models.CharField(max_length=20, choices=CATEGORY_CHOICES, default='system')
    status = models.CharField(max_length=20, choices=STATUS_CHOICES, default='pending')
    
    title = models.CharField(max_length=255)
    message = models.TextField()
    
    # Optional links
    link_url = models.URLField(blank=True, null=True)
    link_text = models.CharField(max_length=100, blank=True, null=True)
    
    # Metadata
    metadata = models.JSONField(default=dict, blank=True)
    
    # Timestamps
    sent_at = models.DateTimeField(null=True, blank=True)
    delivered_at = models.DateTimeField(null=True, blank=True)
    read_at = models.DateTimeField(null=True, blank=True)
    created_at = models.DateTimeField(auto_now_add=True)
    
    class Meta:
        verbose_name = _('notification')
        verbose_name_plural = _('notifications')
        ordering = ['-created_at']
        indexes = [
            models.Index(fields=['-created_at']),
            models.Index(fields=['user', 'status']),
        ]
    
    def __str__(self):
        return f"{self.notification_type} - {self.title} - {self.user.email}"
    
    def mark_as_read(self):
        """Mark notification as read"""
        from django.utils import timezone
        if self.status != 'read':
            self.status = 'read'
            self.read_at = timezone.now()
            self.save()


class WhatsAppMessage(models.Model):
    """WhatsApp message tracking"""
    
    STATUS_CHOICES = [
        ('queued', 'Queued'),
        ('sending', 'Sending'),
        ('sent', 'Sent'),
        ('delivered', 'Delivered'),
        ('read', 'Read'),
        ('failed', 'Failed'),
    ]
    
    notification = models.OneToOneField(Notification, on_delete=models.CASCADE, 
                                       related_name='whatsapp_message', null=True, blank=True)
    recipient = models.CharField(max_length=20, help_text="Phone number with country code")
    message = models.TextField()
    status = models.CharField(max_length=20, choices=STATUS_CHOICES, default='queued')
    
    # WhatsApp API response
    message_id = models.CharField(max_length=255, blank=True, null=True, 
                                  help_text="WhatsApp message ID from API")
    error_message = models.TextField(blank=True, null=True)
    
    # Timestamps
    sent_at = models.DateTimeField(null=True, blank=True)
    delivered_at = models.DateTimeField(null=True, blank=True)
    read_at = models.DateTimeField(null=True, blank=True)
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        verbose_name = _('WhatsApp message')
        verbose_name_plural = _('WhatsApp messages')
        ordering = ['-created_at']
    
    def __str__(self):
        return f"WhatsApp to {self.recipient} - {self.status}"


class NotificationTemplate(models.Model):
    """Templates for different notification types"""
    
    TYPE_CHOICES = [
        ('email', 'Email'),
        ('whatsapp', 'WhatsApp'),
        ('sms', 'SMS'),
    ]
    
    name = models.CharField(max_length=100, unique=True)
    notification_type = models.CharField(max_length=20, choices=TYPE_CHOICES)
    category = models.CharField(max_length=20, choices=Notification.CATEGORY_CHOICES)
    
    subject = models.CharField(max_length=255, blank=True, null=True, help_text="For email notifications")
    template = models.TextField(help_text="Template with {variables} for replacement")
    
    is_active = models.BooleanField(default=True)
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        verbose_name = _('notification template')
        verbose_name_plural = _('notification templates')
        ordering = ['name']
    
    def __str__(self):
        return f"{self.name} ({self.notification_type})"
    
    def render(self, context):
        """Render template with context variables"""
        rendered = self.template
        for key, value in context.items():
            rendered = rendered.replace(f"{{{key}}}", str(value))
        return rendered


class NotificationPreference(models.Model):
    """User notification preferences"""
    
    user = models.OneToOneField(settings.AUTH_USER_MODEL, on_delete=models.CASCADE, 
                               related_name='notification_preference')
    
    # Channel preferences
    email_enabled = models.BooleanField(default=True)
    whatsapp_enabled = models.BooleanField(default=False)
    sms_enabled = models.BooleanField(default=False)
    in_app_enabled = models.BooleanField(default=True)
    
    # Category preferences
    invoice_notifications = models.BooleanField(default=True)
    payment_notifications = models.BooleanField(default=True)
    service_notifications = models.BooleanField(default=True)
    account_notifications = models.BooleanField(default=True)
    system_notifications = models.BooleanField(default=True)
    marketing_notifications = models.BooleanField(default=False)
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        verbose_name = _('notification preference')
        verbose_name_plural = _('notification preferences')
    
    def __str__(self):
        return f"Preferences for {self.user.email}"
    
    def should_send(self, notification_type, category):
        """Check if notification should be sent based on preferences"""
        # Check channel
        channel_enabled = getattr(self, f"{notification_type}_enabled", False)
        if not channel_enabled:
            return False
        
        # Check category
        category_enabled = getattr(self, f"{category}_notifications", True)
        return category_enabled

