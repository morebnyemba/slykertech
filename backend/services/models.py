from django.db import models
from django.conf import settings
from django.utils.translation import gettext_lazy as _


class Service(models.Model):
    """Services offered by the company"""
    
    SERVICE_CATEGORY_CHOICES = [
        ('hosting', 'Web Hosting'),
        ('development', 'Development'),
        ('design', 'Design'),
        ('marketing', 'Marketing'),
        ('maintenance', 'Maintenance'),
        ('cloud', 'Cloud Services'),
        ('other', 'Other'),
    ]
    
    name = models.CharField(max_length=255)
    category = models.CharField(max_length=50, choices=SERVICE_CATEGORY_CHOICES)
    description = models.TextField()
    features = models.JSONField(default=list, blank=True)
    base_price = models.DecimalField(max_digits=10, decimal_places=2, null=True, blank=True)
    is_active = models.BooleanField(default=True)
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        verbose_name = _('service')
        verbose_name_plural = _('services')
        ordering = ['name']
    
    def __str__(self):
        return self.name


class ServiceSubscription(models.Model):
    """Client subscriptions to services"""
    
    STATUS_CHOICES = [
        ('active', 'Active'),
        ('pending', 'Pending'),
        ('suspended', 'Suspended'),
        ('cancelled', 'Cancelled'),
        ('expired', 'Expired'),
    ]
    
    BILLING_CYCLE_CHOICES = [
        ('monthly', 'Monthly'),
        ('quarterly', 'Quarterly'),
        ('semi_annual', 'Semi-Annual'),
        ('annual', 'Annual'),
        ('one_time', 'One Time'),
    ]
    
    client = models.ForeignKey('clients.Client', on_delete=models.CASCADE, related_name='subscriptions')
    service = models.ForeignKey(Service, on_delete=models.CASCADE, related_name='subscriptions')
    status = models.CharField(max_length=20, choices=STATUS_CHOICES, default='pending')
    billing_cycle = models.CharField(max_length=20, choices=BILLING_CYCLE_CHOICES, default='monthly')
    price = models.DecimalField(max_digits=10, decimal_places=2)
    start_date = models.DateField()
    end_date = models.DateField(null=True, blank=True)
    auto_renew = models.BooleanField(default=True)
    notes = models.TextField(blank=True, null=True)
    metadata = models.JSONField(default=dict, blank=True, help_text="Additional service-specific data")
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        verbose_name = _('service subscription')
        verbose_name_plural = _('service subscriptions')
        ordering = ['-created_at']
    
    def __str__(self):
        return f"{self.client.company_name} - {self.service.name}"


class DNSRecord(models.Model):
    """DNS records management for clients"""
    
    RECORD_TYPE_CHOICES = [
        ('A', 'A'),
        ('AAAA', 'AAAA'),
        ('CNAME', 'CNAME'),
        ('MX', 'MX'),
        ('TXT', 'TXT'),
        ('NS', 'NS'),
        ('SRV', 'SRV'),
        ('CAA', 'CAA'),
    ]
    
    subscription = models.ForeignKey(ServiceSubscription, on_delete=models.CASCADE, related_name='dns_records')
    domain = models.CharField(max_length=255)
    record_type = models.CharField(max_length=10, choices=RECORD_TYPE_CHOICES)
    name = models.CharField(max_length=255)
    content = models.TextField()
    ttl = models.IntegerField(default=3600)
    priority = models.IntegerField(null=True, blank=True)
    is_active = models.BooleanField(default=True)
    external_id = models.CharField(max_length=255, blank=True, null=True, help_text="ID from external DNS provider")
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        verbose_name = _('DNS record')
        verbose_name_plural = _('DNS records')
        ordering = ['domain', 'record_type', 'name']
    
    def __str__(self):
        return f"{self.domain} - {self.record_type} - {self.name}"

