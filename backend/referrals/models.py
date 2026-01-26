from django.db import models
from django.db.utils import ProgrammingError, OperationalError
from django.contrib.auth import get_user_model
from django.utils.translation import gettext_lazy as _
import secrets
import string

User = get_user_model()


def generate_referral_code():
    """Generate a unique 8-character referral code"""
    characters = string.ascii_uppercase + string.digits
    return ''.join(secrets.choice(characters) for _ in range(8))


class ReferralProfile(models.Model):
    """Profile to track user's referral information"""
    
    user = models.OneToOneField(User, on_delete=models.CASCADE, related_name='referral_profile')
    referral_code = models.CharField(max_length=20, unique=True, editable=False)
    referred_by = models.ForeignKey(
        'self', 
        on_delete=models.SET_NULL, 
        null=True, 
        blank=True, 
        related_name='referrals'
    )
    total_referrals = models.IntegerField(default=0)
    successful_referrals = models.IntegerField(default=0)
    total_earnings = models.DecimalField(max_digits=10, decimal_places=2, default=0)
    is_active = models.BooleanField(default=True)
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    def save(self, *args, **kwargs):
        if not self.referral_code:
            self.referral_code = generate_referral_code()
            # Ensure uniqueness
            while ReferralProfile.objects.filter(referral_code=self.referral_code).exists():
                self.referral_code = generate_referral_code()
        super().save(*args, **kwargs)
    
    class Meta:
        verbose_name = _('Referral Profile')
        verbose_name_plural = _('Referral Profiles')
        ordering = ['-created_at']
    
    def __str__(self):
        return f"{self.user.email} - {self.referral_code}"


class Referral(models.Model):
    """Track individual referrals"""
    
    STATUS_CHOICES = [
        ('pending', 'Pending'),
        ('signed_up', 'Signed Up'),
        ('converted', 'Converted'),
        ('rewarded', 'Rewarded'),
        ('expired', 'Expired'),
    ]
    
    referrer = models.ForeignKey(
        ReferralProfile, 
        on_delete=models.CASCADE, 
        related_name='sent_referrals'
    )
    referred_user = models.ForeignKey(
        User, 
        on_delete=models.CASCADE, 
        related_name='referred_by_user',
        null=True,
        blank=True
    )
    referred_email = models.EmailField(help_text="Email of the person being referred")
    referral_code_used = models.CharField(max_length=20)
    status = models.CharField(max_length=20, choices=STATUS_CHOICES, default='pending')
    reward_amount = models.DecimalField(max_digits=10, decimal_places=2, null=True, blank=True)
    notes = models.TextField(blank=True)
    signup_date = models.DateTimeField(null=True, blank=True)
    conversion_date = models.DateTimeField(null=True, blank=True)
    reward_date = models.DateTimeField(null=True, blank=True)
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        verbose_name = _('Referral')
        verbose_name_plural = _('Referrals')
        ordering = ['-created_at']
    
    def __str__(self):
        return f"{self.referrer.user.email} → {self.referred_email} ({self.status})"


class ReferralReward(models.Model):
    """Track rewards given for successful referrals"""
    
    REWARD_TYPE_CHOICES = [
        ('signup_bonus', 'Signup Bonus'),
        ('conversion_bonus', 'Conversion Bonus'),
        ('milestone_bonus', 'Milestone Bonus'),
    ]
    
    STATUS_CHOICES = [
        ('pending', 'Pending'),
        ('approved', 'Approved'),
        ('paid', 'Paid'),
        ('rejected', 'Rejected'),
    ]
    
    referral_profile = models.ForeignKey(
        ReferralProfile, 
        on_delete=models.CASCADE, 
        related_name='rewards'
    )
    referral = models.ForeignKey(
        Referral, 
        on_delete=models.CASCADE, 
        related_name='rewards',
        null=True,
        blank=True
    )
    reward_type = models.CharField(max_length=20, choices=REWARD_TYPE_CHOICES)
    amount = models.DecimalField(max_digits=10, decimal_places=2)
    status = models.CharField(max_length=20, choices=STATUS_CHOICES, default='pending')
    description = models.TextField(blank=True)
    paid_at = models.DateTimeField(null=True, blank=True)
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        verbose_name = _('Referral Reward')
        verbose_name_plural = _('Referral Rewards')
        ordering = ['-created_at']
    
    def __str__(self):
        return f"{self.referral_profile.user.email} - ${self.amount} ({self.status})"


class ReferralSettings(models.Model):
    """Global settings for the referral program"""
    
    signup_bonus_amount = models.DecimalField(
        max_digits=10, 
        decimal_places=2, 
        default=10.00,
        help_text="Bonus amount when referred user signs up"
    )
    conversion_bonus_amount = models.DecimalField(
        max_digits=10, 
        decimal_places=2, 
        default=25.00,
        help_text="Bonus amount when referred user makes first purchase"
    )
    conversion_bonus_percentage = models.DecimalField(
        max_digits=5, 
        decimal_places=2, 
        default=5.00,
        help_text="Percentage of first purchase as bonus"
    )
    minimum_payout = models.DecimalField(
        max_digits=10, 
        decimal_places=2, 
        default=50.00,
        help_text="Minimum amount required for payout"
    )
    referral_expiry_days = models.IntegerField(
        default=30,
        help_text="Days until a referral link expires"
    )
    max_referrals_per_user = models.IntegerField(
        default=100,
        help_text="Maximum referrals allowed per user (0 for unlimited)"
    )
    is_program_active = models.BooleanField(default=True)
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        verbose_name = _('Referral Settings')
        verbose_name_plural = _('Referral Settings')
    
    def save(self, *args, **kwargs):
        # Ensure only one settings record exists
        # Handle case where table doesn't exist yet (before migrations)
        try:
            if not self.pk and ReferralSettings.objects.exists():
                raise ValueError("Only one ReferralSettings instance is allowed")
        except (ProgrammingError, OperationalError):
            pass  # Table doesn't exist yet, allow save
        super().save(*args, **kwargs)
    
    def __str__(self):
        return "Referral Program Settings"
