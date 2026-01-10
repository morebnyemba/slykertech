from django.db import models
from django.conf import settings
from django.utils.translation import gettext_lazy as _


class Client(models.Model):
    """Client profile extending the User model"""
    
    user = models.OneToOneField(settings.AUTH_USER_MODEL, on_delete=models.CASCADE, related_name='client_profile')
    company_name = models.CharField(max_length=255)
    company_website = models.URLField(blank=True, null=True)
    address = models.TextField(blank=True, null=True)
    city = models.CharField(max_length=100, blank=True, null=True)
    country = models.CharField(max_length=100, blank=True, null=True)
    billing_email = models.EmailField(blank=True, null=True)
    tax_id = models.CharField(max_length=100, blank=True, null=True)
    is_active = models.BooleanField(default=True)
    notes = models.TextField(blank=True, null=True)
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        verbose_name = _('client')
        verbose_name_plural = _('clients')
        ordering = ['-created_at']
    
    def __str__(self):
        return f"{self.company_name} - {self.user.email}"


class ClientContact(models.Model):
    """Additional contacts for a client"""
    
    client = models.ForeignKey(Client, on_delete=models.CASCADE, related_name='contacts')
    name = models.CharField(max_length=255)
    email = models.EmailField()
    phone = models.CharField(max_length=20, blank=True, null=True)
    position = models.CharField(max_length=100, blank=True, null=True)
    is_primary = models.BooleanField(default=False)
    created_at = models.DateTimeField(auto_now_add=True)
    
    class Meta:
        verbose_name = _('client contact')
        verbose_name_plural = _('client contacts')
        ordering = ['-is_primary', 'name']
    
    def __str__(self):
        return f"{self.name} - {self.client.company_name}"

