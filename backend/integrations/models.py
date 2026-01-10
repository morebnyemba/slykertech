from django.db import models
from django.conf import settings
from django.utils.translation import gettext_lazy as _
from cryptography.fernet import Fernet
from django.core.exceptions import ValidationError
import base64


class IntegrationCredential(models.Model):
    """Stores encrypted credentials for external service integrations"""
    
    PROVIDER_CHOICES = [
        ('cpanel', 'cPanel'),
        ('directadmin', 'DirectAdmin'),
        ('cloudflare', 'Cloudflare'),
        ('aws', 'AWS'),
        ('digitalocean', 'DigitalOcean'),
        ('other', 'Other'),
    ]
    
    client = models.ForeignKey('clients.Client', on_delete=models.CASCADE, related_name='integration_credentials')
    provider = models.CharField(max_length=50, choices=PROVIDER_CHOICES)
    name = models.CharField(max_length=255, help_text="Friendly name for this integration")
    host = models.CharField(max_length=255, blank=True, null=True)
    username = models.CharField(max_length=255, blank=True, null=True)
    encrypted_password = models.TextField(blank=True, null=True)
    encrypted_api_token = models.TextField(blank=True, null=True)
    port = models.IntegerField(null=True, blank=True)
    is_active = models.BooleanField(default=True)
    metadata = models.JSONField(default=dict, blank=True)
    last_verified = models.DateTimeField(null=True, blank=True)
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        verbose_name = _('integration credential')
        verbose_name_plural = _('integration credentials')
        ordering = ['client', 'provider', 'name']
        unique_together = [['client', 'provider', 'name']]
    
    def __str__(self):
        return f"{self.client.company_name} - {self.provider} - {self.name}"
    
    def encrypt_field(self, value):
        """Encrypt a field value"""
        if not value:
            return None
        # Use a dedicated encryption key from environment or settings
        from decouple import config
        encryption_key = config('ENCRYPTION_KEY', default=None)
        
        if not encryption_key:
            # Fallback for development - derive from SECRET_KEY
            key = settings.SECRET_KEY[:32].ljust(32, '0').encode()
        else:
            key = encryption_key[:32].ljust(32, '0').encode()
        
        cipher = Fernet(base64.urlsafe_b64encode(key))
        return cipher.encrypt(value.encode()).decode()
    
    def decrypt_field(self, encrypted_value):
        """Decrypt a field value"""
        if not encrypted_value:
            return None
        # Use a dedicated encryption key from environment or settings
        from decouple import config
        encryption_key = config('ENCRYPTION_KEY', default=None)
        
        if not encryption_key:
            # Fallback for development - derive from SECRET_KEY
            key = settings.SECRET_KEY[:32].ljust(32, '0').encode()
        else:
            key = encryption_key[:32].ljust(32, '0').encode()
        
        cipher = Fernet(base64.urlsafe_b64encode(key))
        return cipher.decrypt(encrypted_value.encode()).decode()

    
    def set_password(self, password):
        """Set encrypted password"""
        self.encrypted_password = self.encrypt_field(password)
    
    def get_password(self):
        """Get decrypted password"""
        return self.decrypt_field(self.encrypted_password)
    
    def set_api_token(self, token):
        """Set encrypted API token"""
        self.encrypted_api_token = self.encrypt_field(token)
    
    def get_api_token(self):
        """Get decrypted API token"""
        return self.decrypt_field(self.encrypted_api_token)


class cPanelAccount(models.Model):
    """cPanel account details for clients"""
    
    credential = models.ForeignKey(IntegrationCredential, on_delete=models.CASCADE, related_name='cpanel_accounts')
    subscription = models.ForeignKey('services.ServiceSubscription', on_delete=models.CASCADE, related_name='cpanel_accounts')
    cpanel_username = models.CharField(max_length=255)
    domain = models.CharField(max_length=255)
    disk_quota = models.IntegerField(help_text="Disk quota in MB", null=True, blank=True)
    disk_used = models.IntegerField(help_text="Disk used in MB", null=True, blank=True)
    bandwidth_quota = models.IntegerField(help_text="Bandwidth quota in MB", null=True, blank=True)
    bandwidth_used = models.IntegerField(help_text="Bandwidth used in MB", null=True, blank=True)
    email_accounts = models.IntegerField(default=0)
    databases = models.IntegerField(default=0)
    subdomains = models.IntegerField(default=0)
    is_suspended = models.BooleanField(default=False)
    last_sync = models.DateTimeField(null=True, blank=True)
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        verbose_name = _('cPanel account')
        verbose_name_plural = _('cPanel accounts')
        ordering = ['domain']
    
    def __str__(self):
        return f"{self.domain} - {self.cpanel_username}"


class DirectAdminAccount(models.Model):
    """DirectAdmin account details for clients"""
    
    credential = models.ForeignKey(IntegrationCredential, on_delete=models.CASCADE, related_name='directadmin_accounts')
    subscription = models.ForeignKey('services.ServiceSubscription', on_delete=models.CASCADE, related_name='directadmin_accounts')
    da_username = models.CharField(max_length=255)
    domain = models.CharField(max_length=255)
    disk_quota = models.IntegerField(help_text="Disk quota in MB", null=True, blank=True)
    disk_used = models.IntegerField(help_text="Disk used in MB", null=True, blank=True)
    bandwidth_quota = models.IntegerField(help_text="Bandwidth quota in MB", null=True, blank=True)
    bandwidth_used = models.IntegerField(help_text="Bandwidth used in MB", null=True, blank=True)
    email_accounts = models.IntegerField(default=0)
    databases = models.IntegerField(default=0)
    subdomains = models.IntegerField(default=0)
    is_suspended = models.BooleanField(default=False)
    last_sync = models.DateTimeField(null=True, blank=True)
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        verbose_name = _('DirectAdmin account')
        verbose_name_plural = _('DirectAdmin accounts')
        ordering = ['domain']
    
    def __str__(self):
        return f"{self.domain} - {self.da_username}"

