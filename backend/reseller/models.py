from django.db import models
from django.contrib.auth import get_user_model
import secrets

User = get_user_model()

class PartnerProfile(models.Model):
    """Base model for all partner types"""
    PARTNER_TYPE_CHOICES = [
        ('reseller', 'Reseller'),
        ('agency', 'Agency Partner'),
        ('technology', 'Technology Alliance'),
    ]
    
    TIER_CHOICES = [
        ('bronze', 'Bronze'),
        ('silver', 'Silver'),
        ('gold', 'Gold'),
        ('platinum', 'Platinum'),
    ]
    
    user = models.OneToOneField(User, on_delete=models.CASCADE, related_name='partner_profile')
    partner_type = models.CharField(max_length=20, choices=PARTNER_TYPE_CHOICES, default='reseller')
    tier = models.CharField(max_length=20, choices=TIER_CHOICES, default='bronze', blank=True, null=True)
    company_name = models.CharField(max_length=255)
    is_active = models.BooleanField(default=True)
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    def __str__(self):
        return f"{self.company_name} - {self.get_partner_type_display()}"
    
    class Meta:
        verbose_name = 'Partner Profile'
        verbose_name_plural = 'Partner Profiles'

class ResellerProfile(models.Model):
    """Reseller-specific information"""
    TIER_CHOICES = [
        ('bronze', 'Bronze'),
        ('silver', 'Silver'),
        ('gold', 'Gold'),
        ('platinum', 'Platinum'),
    ]
    
    partner = models.OneToOneField(PartnerProfile, on_delete=models.CASCADE, related_name='reseller_data')
    tier = models.CharField(max_length=20, choices=TIER_CHOICES, default='bronze')
    discount_percentage = models.DecimalField(max_digits=5, decimal_places=2, default=10)
    commission_percentage = models.DecimalField(max_digits=5, decimal_places=2, default=5)
    api_key = models.CharField(max_length=255, unique=True, editable=False)
    api_secret = models.CharField(max_length=255, editable=False)
    api_rate_limit = models.IntegerField(default=1000)
    max_clients = models.IntegerField(default=50)
    branding_name = models.CharField(max_length=255, blank=True)
    webhook_url = models.URLField(blank=True)
    
    # For backward compatibility
    @property
    def user(self):
        return self.partner.user
    
    @property
    def is_active(self):
        return self.partner.is_active
    
    @property
    def created_at(self):
        return self.partner.created_at
    
    @property
    def updated_at(self):
        return self.partner.updated_at
    
    def save(self, *args, **kwargs):
        if not self.api_key:
            self.api_key = f"rsk_live_{secrets.token_urlsafe(32)}"
        if not self.api_secret:
            self.api_secret = f"rss_{secrets.token_urlsafe(48)}"
        super().save(*args, **kwargs)

    def __str__(self):
        return f"{self.partner.company_name} - {self.tier}"

class AgencyPartner(models.Model):
    """Agency partner-specific information"""
    partner = models.OneToOneField(PartnerProfile, on_delete=models.CASCADE, related_name='agency_data')
    specialization = models.CharField(max_length=255, help_text="Primary area of expertise")
    referral_bonus_percentage = models.DecimalField(max_digits=5, decimal_places=2, default=10)
    active_referrals = models.IntegerField(default=0)
    total_referrals = models.IntegerField(default=0)
    
    def __str__(self):
        return f"{self.partner.company_name} - Agency Partner"

class TechnologyAlliance(models.Model):
    """Technology alliance-specific information"""
    partner = models.OneToOneField(PartnerProfile, on_delete=models.CASCADE, related_name='technology_data')
    product_name = models.CharField(max_length=255)
    integration_type = models.CharField(max_length=255, help_text="Type of integration (API, Plugin, etc.)")
    api_key = models.CharField(max_length=255, unique=True, editable=False, blank=True)
    revenue_share_percentage = models.DecimalField(max_digits=5, decimal_places=2, default=15)
    
    def save(self, *args, **kwargs):
        if not self.api_key:
            self.api_key = f"ta_live_{secrets.token_urlsafe(32)}"
        super().save(*args, **kwargs)
    
    def __str__(self):
        return f"{self.partner.company_name} - {self.product_name}"

class ResellerClient(models.Model):
    reseller = models.ForeignKey(ResellerProfile, on_delete=models.CASCADE, related_name='clients')
    client = models.ForeignKey(User, on_delete=models.CASCADE, related_name='reseller_subscriptions')
    custom_pricing = models.JSONField(blank=True, null=True)
    notes = models.TextField(blank=True)
    created_at = models.DateTimeField(auto_now_add=True)

    class Meta:
        unique_together = ['reseller', 'client']

    def __str__(self):
        return f"{self.reseller.partner.company_name} -> {self.client.email}"

class ResellerCommission(models.Model):
    STATUS_CHOICES = [
        ('pending', 'Pending'),
        ('approved', 'Approved'),
        ('paid', 'Paid'),
    ]
    
    reseller = models.ForeignKey(ResellerProfile, on_delete=models.CASCADE, related_name='commissions')
    subscription = models.ForeignKey('services.ServiceSubscription', on_delete=models.CASCADE)
    amount = models.DecimalField(max_digits=10, decimal_places=2)
    status = models.CharField(max_length=20, choices=STATUS_CHOICES, default='pending')
    paid_at = models.DateTimeField(null=True, blank=True)
    created_at = models.DateTimeField(auto_now_add=True)

    def __str__(self):
        return f"{self.reseller.partner.company_name} - ${self.amount} - {self.status}"

class AgencyReferral(models.Model):
    """Track referrals from agency partners"""
    STATUS_CHOICES = [
        ('pending', 'Pending'),
        ('qualified', 'Qualified'),
        ('converted', 'Converted'),
        ('paid', 'Paid'),
    ]
    
    agency = models.ForeignKey(AgencyPartner, on_delete=models.CASCADE, related_name='referrals')
    referred_client = models.ForeignKey(User, on_delete=models.CASCADE, related_name='agency_referrals')
    project_description = models.TextField(blank=True)
    estimated_value = models.DecimalField(max_digits=10, decimal_places=2, null=True, blank=True)
    bonus_amount = models.DecimalField(max_digits=10, decimal_places=2, null=True, blank=True)
    status = models.CharField(max_length=20, choices=STATUS_CHOICES, default='pending')
    paid_at = models.DateTimeField(null=True, blank=True)
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    def __str__(self):
        return f"{self.agency.partner.company_name} - {self.referred_client.email} - {self.status}"
