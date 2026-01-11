from django.db import models
from django.contrib.auth import get_user_model
import secrets

User = get_user_model()

class ResellerProfile(models.Model):
    TIER_CHOICES = [
        ('bronze', 'Bronze'),
        ('silver', 'Silver'),
        ('gold', 'Gold'),
        ('platinum', 'Platinum'),
    ]
    
    user = models.OneToOneField(User, on_delete=models.CASCADE, related_name='reseller_profile')
    tier = models.CharField(max_length=20, choices=TIER_CHOICES, default='bronze')
    discount_percentage = models.DecimalField(max_digits=5, decimal_places=2, default=10)
    commission_percentage = models.DecimalField(max_digits=5, decimal_places=2, default=5)
    api_key = models.CharField(max_length=255, unique=True, editable=False)
    api_secret = models.CharField(max_length=255, editable=False)
    api_rate_limit = models.IntegerField(default=1000)
    max_clients = models.IntegerField(default=50)
    branding_name = models.CharField(max_length=255, blank=True)
    webhook_url = models.URLField(blank=True)
    is_active = models.BooleanField(default=True)
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)

    def save(self, *args, **kwargs):
        if not self.api_key:
            self.api_key = f"rsk_live_{secrets.token_urlsafe(32)}"
        if not self.api_secret:
            self.api_secret = f"rss_{secrets.token_urlsafe(48)}"
        super().save(*args, **kwargs)

    def __str__(self):
        return f"{self.user.email} - {self.tier}"

class ResellerClient(models.Model):
    reseller = models.ForeignKey(ResellerProfile, on_delete=models.CASCADE, related_name='clients')
    client = models.ForeignKey(User, on_delete=models.CASCADE, related_name='reseller_subscriptions')
    custom_pricing = models.JSONField(blank=True, null=True)
    notes = models.TextField(blank=True)
    created_at = models.DateTimeField(auto_now_add=True)

    class Meta:
        unique_together = ['reseller', 'client']

    def __str__(self):
        return f"{self.reseller.user.email} -> {self.client.email}"

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
        return f"{self.reseller.user.email} - ${self.amount} - {self.status}"
