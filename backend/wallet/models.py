from django.db import models
from django.contrib.auth import get_user_model
from decimal import Decimal

User = get_user_model()

class Wallet(models.Model):
    CURRENCY_CHOICES = [
        ('USD', 'US Dollar'),
        ('ZWL', 'Zimbabwe Dollar'),
    ]
    
    user = models.OneToOneField(User, on_delete=models.CASCADE, related_name='wallet')
    balance = models.DecimalField(max_digits=12, decimal_places=2, default=Decimal('0.00'))
    currency = models.CharField(max_length=3, choices=CURRENCY_CHOICES, default='USD')
    low_balance_threshold = models.DecimalField(max_digits=10, decimal_places=2, default=Decimal('50.00'))
    auto_topup_enabled = models.BooleanField(default=False)
    auto_topup_amount = models.DecimalField(max_digits=10, decimal_places=2, default=Decimal('100.00'))
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)

    def __str__(self):
        return f"{self.user.email} - {self.currency} {self.balance}"

    def has_sufficient_balance(self, amount):
        return self.balance >= amount

    def debit(self, amount, description, reference=None):
        if not self.has_sufficient_balance(amount):
            raise ValueError("Insufficient balance")
        
        self.balance -= amount
        self.save()
        
        WalletTransaction.objects.create(
            wallet=self,
            transaction_type='debit',
            category='service_payment',
            amount=amount,
            balance_before=self.balance + amount,
            balance_after=self.balance,
            description=description,
            reference=reference
        )
        
        return self.balance

    def credit(self, amount, category, description, reference=None):
        self.balance += amount
        self.save()
        
        WalletTransaction.objects.create(
            wallet=self,
            transaction_type='credit',
            category=category,
            amount=amount,
            balance_before=self.balance - amount,
            balance_after=self.balance,
            description=description,
            reference=reference
        )
        
        return self.balance

class WalletTransaction(models.Model):
    TRANSACTION_TYPE_CHOICES = [
        ('credit', 'Credit'),
        ('debit', 'Debit'),
    ]
    
    CATEGORY_CHOICES = [
        ('top_up', 'Top Up'),
        ('refund', 'Refund'),
        ('commission', 'Commission'),
        ('service_payment', 'Service Payment'),
        ('renewal', 'Renewal'),
    ]
    
    wallet = models.ForeignKey(Wallet, on_delete=models.CASCADE, related_name='transactions')
    transaction_type = models.CharField(max_length=10, choices=TRANSACTION_TYPE_CHOICES)
    category = models.CharField(max_length=20, choices=CATEGORY_CHOICES)
    amount = models.DecimalField(max_digits=10, decimal_places=2)
    balance_before = models.DecimalField(max_digits=12, decimal_places=2)
    balance_after = models.DecimalField(max_digits=12, decimal_places=2)
    description = models.TextField()
    reference = models.CharField(max_length=255, blank=True)
    created_at = models.DateTimeField(auto_now_add=True)

    class Meta:
        ordering = ['-created_at']

    def __str__(self):
        return f"{self.wallet.user.email} - {self.transaction_type} - {self.amount}"
