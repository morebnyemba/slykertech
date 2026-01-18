from django.db import models
from django.contrib.auth import get_user_model
from django.utils import timezone
from dateutil.relativedelta import relativedelta

User = get_user_model()

class InvestmentPackage(models.Model):
    name = models.CharField(max_length=255)
    description = models.TextField()
    minimum_amount = models.DecimalField(max_digits=12, decimal_places=2)
    expected_return = models.DecimalField(max_digits=5, decimal_places=2, help_text="Expected return percentage")
    duration_months = models.IntegerField(help_text="Investment duration in months")
    is_active = models.BooleanField(default=True)
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        ordering = ['minimum_amount']
        verbose_name = 'Investment Package'
        verbose_name_plural = 'Investment Packages'
    
    def __str__(self):
        return f"{self.name} - {self.expected_return}% return"

class Investment(models.Model):
    STATUS_CHOICES = [
        ('active', 'Active'),
        ('matured', 'Matured'),
        ('withdrawn', 'Withdrawn'),
    ]
    
    investor = models.ForeignKey(User, on_delete=models.CASCADE, related_name='investments')
    package = models.ForeignKey(InvestmentPackage, on_delete=models.PROTECT, related_name='investments')
    amount = models.DecimalField(max_digits=12, decimal_places=2)
    start_date = models.DateTimeField(default=timezone.now)
    maturity_date = models.DateTimeField()
    status = models.CharField(max_length=20, choices=STATUS_CHOICES, default='active')
    current_value = models.DecimalField(max_digits=12, decimal_places=2)
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        ordering = ['-start_date']
        verbose_name = 'Investment'
        verbose_name_plural = 'Investments'
    
    def __str__(self):
        return f"{self.investor.email} - {self.package.name} - ${self.amount}"
    
    def save(self, *args, **kwargs):
        if not self.maturity_date:
            self.maturity_date = self.start_date + relativedelta(months=self.package.duration_months)
        if not self.current_value:
            self.current_value = self.amount
        super().save(*args, **kwargs)
    
    def calculate_current_value(self):
        """Calculate current investment value based on expected return"""
        months_elapsed = (timezone.now() - self.start_date).days / 30.44
        if months_elapsed >= self.package.duration_months:
            return self.amount * (1 + self.package.expected_return / 100)
        else:
            partial_return = (self.package.expected_return / 100) * (months_elapsed / self.package.duration_months)
            return self.amount * (1 + partial_return)

class BankingDetails(models.Model):
    investor = models.ForeignKey(User, on_delete=models.CASCADE, related_name='banking_details')
    bank_name = models.CharField(max_length=255)
    account_number = models.CharField(max_length=50)
    account_holder_name = models.CharField(max_length=255)
    swift_code = models.CharField(max_length=20, blank=True, null=True)
    branch_code = models.CharField(max_length=20, blank=True, null=True)
    is_primary = models.BooleanField(default=True)
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        ordering = ['-is_primary', '-created_at']
        verbose_name = 'Banking Details'
        verbose_name_plural = 'Banking Details'
    
    def __str__(self):
        return f"{self.investor.email} - {self.bank_name} - {self.account_number}"
