from django.db import models
from django.conf import settings
from django.contrib.auth.models import AbstractUser, BaseUserManager
from django.contrib.contenttypes.fields import GenericForeignKey
from django.contrib.contenttypes.models import ContentType
from django.utils.translation import gettext_lazy as _


class UserManager(BaseUserManager):
    """Custom user manager for email-based authentication"""
    
    def create_user(self, email, password=None, **extra_fields):
        if not email:
            raise ValueError(_('The Email field must be set'))
        email = self.normalize_email(email)
        user = self.model(email=email, **extra_fields)
        user.set_password(password)
        user.save(using=self._db)
        return user

    def create_superuser(self, email, password=None, **extra_fields):
        extra_fields.setdefault('is_staff', True)
        extra_fields.setdefault('is_superuser', True)
        extra_fields.setdefault('is_active', True)

        if extra_fields.get('is_staff') is not True:
            raise ValueError(_('Superuser must have is_staff=True.'))
        if extra_fields.get('is_superuser') is not True:
            raise ValueError(_('Superuser must have is_superuser=True.'))
        
        return self.create_user(email, password, **extra_fields)


class User(AbstractUser):
    """Custom user model with email as username"""
    
    USER_TYPE_CHOICES = [
        ('admin', 'Admin'),
        ('client', 'Client'),
        ('staff', 'Staff'),
    ]
    
    username = None
    email = models.EmailField(_('email address'), unique=True)
    user_type = models.CharField(max_length=20, choices=USER_TYPE_CHOICES, default='client')
    phone = models.CharField(max_length=20, blank=True, null=True)
    mobile_number = models.CharField(max_length=20, blank=True, null=True, help_text="Mobile number for WhatsApp notifications")
    company_name = models.CharField(max_length=255, blank=True, null=True)
    
    # Notification preferences
    email_notifications = models.BooleanField(default=True, help_text="Receive email notifications")
    whatsapp_notifications = models.BooleanField(default=False, help_text="Receive WhatsApp notifications")
    sms_notifications = models.BooleanField(default=False, help_text="Receive SMS notifications")
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    USERNAME_FIELD = 'email'
    REQUIRED_FIELDS = []
    
    objects = UserManager()
    
    class Meta:
        verbose_name = _('user')
        verbose_name_plural = _('users')
        ordering = ['-created_at']
    
    def __str__(self):
        return self.email
    
    @property
    def full_name(self):
        return f"{self.first_name} {self.last_name}".strip() or self.email


class AuditMixin(models.Model):
    """Abstract mixin that adds created_by and updated_by tracking to any model."""

    created_by = models.ForeignKey(
        settings.AUTH_USER_MODEL,
        on_delete=models.SET_NULL,
        null=True,
        blank=True,
        related_name='%(app_label)s_%(class)s_created',
        help_text='User who created this record',
    )
    updated_by = models.ForeignKey(
        settings.AUTH_USER_MODEL,
        on_delete=models.SET_NULL,
        null=True,
        blank=True,
        related_name='%(app_label)s_%(class)s_updated',
        help_text='User who last updated this record',
    )

    class Meta:
        abstract = True


class StatusChangeLog(models.Model):
    """Tracks status transitions on any model that has a status field."""

    content_type = models.ForeignKey(ContentType, on_delete=models.CASCADE)
    object_id = models.PositiveIntegerField()
    content_object = GenericForeignKey('content_type', 'object_id')

    old_status = models.CharField(max_length=50, blank=True, null=True)
    new_status = models.CharField(max_length=50)
    changed_by = models.ForeignKey(
        settings.AUTH_USER_MODEL,
        on_delete=models.SET_NULL,
        null=True,
        blank=True,
        related_name='status_changes',
    )
    reason = models.TextField(blank=True, help_text='Reason for the status change')
    metadata = models.JSONField(default=dict, blank=True, help_text='Additional context data')
    created_at = models.DateTimeField(auto_now_add=True)

    class Meta:
        verbose_name = _('status change log')
        verbose_name_plural = _('status change logs')
        ordering = ['-created_at']
        indexes = [
            models.Index(fields=['content_type', 'object_id']),
            models.Index(fields=['-created_at']),
        ]

    def __str__(self):
        return f'{self.content_type} #{self.object_id}: {self.old_status} → {self.new_status}'


class ActivityLog(models.Model):
    """Tracks user actions across the platform for auditing and analytics."""

    ACTION_CHOICES = [
        ('create', 'Created'),
        ('update', 'Updated'),
        ('delete', 'Deleted'),
        ('login', 'Logged In'),
        ('logout', 'Logged Out'),
        ('view', 'Viewed'),
        ('export', 'Exported'),
        ('provision', 'Provisioned'),
        ('backup', 'Backed Up'),
        ('restore', 'Restored'),
        ('payment', 'Payment'),
        ('status_change', 'Status Change'),
        ('other', 'Other'),
    ]

    user = models.ForeignKey(
        settings.AUTH_USER_MODEL,
        on_delete=models.SET_NULL,
        null=True,
        blank=True,
        related_name='activity_logs',
    )
    action = models.CharField(max_length=20, choices=ACTION_CHOICES)
    description = models.TextField(help_text='Human-readable description of the action')

    # Generic relation to the target object
    content_type = models.ForeignKey(ContentType, on_delete=models.SET_NULL, null=True, blank=True)
    object_id = models.PositiveIntegerField(null=True, blank=True)
    content_object = GenericForeignKey('content_type', 'object_id')

    ip_address = models.GenericIPAddressField(null=True, blank=True)
    user_agent = models.TextField(blank=True)
    metadata = models.JSONField(default=dict, blank=True, help_text='Additional context data')
    created_at = models.DateTimeField(auto_now_add=True)

    class Meta:
        verbose_name = _('activity log')
        verbose_name_plural = _('activity logs')
        ordering = ['-created_at']
        indexes = [
            models.Index(fields=['user', '-created_at']),
            models.Index(fields=['action', '-created_at']),
            models.Index(fields=['content_type', 'object_id']),
        ]

    def __str__(self):
        user_str = self.user.email if self.user else 'System'
        return f'{user_str} - {self.get_action_display()} - {self.description[:50]}'


class BackupLog(models.Model):
    """Tracks database backup history and status."""

    STATUS_CHOICES = [
        ('started', 'Started'),
        ('completed', 'Completed'),
        ('failed', 'Failed'),
    ]

    filename = models.CharField(max_length=500)
    file_size = models.BigIntegerField(null=True, blank=True, help_text='File size in bytes')
    status = models.CharField(max_length=20, choices=STATUS_CHOICES, default='started')
    database_name = models.CharField(max_length=255, default='slykertech')
    error_message = models.TextField(blank=True)
    duration_seconds = models.FloatField(null=True, blank=True)
    created_at = models.DateTimeField(auto_now_add=True)

    class Meta:
        verbose_name = _('backup log')
        verbose_name_plural = _('backup logs')
        ordering = ['-created_at']

    def __str__(self):
        return f'{self.filename} - {self.get_status_display()}'

