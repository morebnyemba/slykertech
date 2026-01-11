from django.db import models
from django.conf import settings
from django.utils.translation import gettext_lazy as _
from decimal import Decimal


class Invoice(models.Model):
    """Invoice model for billing clients"""
    
    STATUS_CHOICES = [
        ('draft', 'Draft'),
        ('sent', 'Sent'),
        ('paid', 'Paid'),
        ('overdue', 'Overdue'),
        ('cancelled', 'Cancelled'),
    ]
    
    client = models.ForeignKey('clients.Client', on_delete=models.CASCADE, related_name='invoices')
    invoice_number = models.CharField(max_length=50, unique=True)
    status = models.CharField(max_length=20, choices=STATUS_CHOICES, default='draft')
    
    # Dates
    issue_date = models.DateField()
    due_date = models.DateField()
    paid_date = models.DateField(null=True, blank=True)
    
    # Amounts
    subtotal = models.DecimalField(max_digits=10, decimal_places=2, default=Decimal('0.00'))
    tax_rate = models.DecimalField(max_digits=5, decimal_places=2, default=Decimal('0.00'), help_text="Tax rate as percentage")
    tax_amount = models.DecimalField(max_digits=10, decimal_places=2, default=Decimal('0.00'))
    discount_amount = models.DecimalField(max_digits=10, decimal_places=2, default=Decimal('0.00'))
    total = models.DecimalField(max_digits=10, decimal_places=2, default=Decimal('0.00'))
    
    # Additional info
    notes = models.TextField(blank=True, null=True)
    terms = models.TextField(blank=True, null=True)
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        verbose_name = _('invoice')
        verbose_name_plural = _('invoices')
        ordering = ['-issue_date', '-invoice_number']
    
    def __str__(self):
        return f"Invoice {self.invoice_number} - {self.client.company_name}"
    
    def calculate_total(self):
        """Calculate invoice total"""
        self.tax_amount = (self.subtotal * self.tax_rate) / Decimal('100')
        self.total = self.subtotal + self.tax_amount - self.discount_amount
        return self.total
    
    def save(self, *args, **kwargs):
        if not self.invoice_number:
            # Auto-generate invoice number
            last_invoice = Invoice.objects.order_by('-id').first()
            if last_invoice:
                last_num = int(last_invoice.invoice_number.split('-')[-1])
                self.invoice_number = f"INV-{last_num + 1:06d}"
            else:
                self.invoice_number = "INV-000001"
        
        self.calculate_total()
        super().save(*args, **kwargs)


class InvoiceItem(models.Model):
    """Line items for invoices"""
    
    invoice = models.ForeignKey(Invoice, on_delete=models.CASCADE, related_name='items')
    subscription = models.ForeignKey('services.ServiceSubscription', on_delete=models.SET_NULL, 
                                    null=True, blank=True, related_name='invoice_items')
    description = models.CharField(max_length=255)
    quantity = models.DecimalField(max_digits=10, decimal_places=2, default=Decimal('1.00'))
    unit_price = models.DecimalField(max_digits=10, decimal_places=2)
    amount = models.DecimalField(max_digits=10, decimal_places=2)
    
    created_at = models.DateTimeField(auto_now_add=True)
    
    class Meta:
        verbose_name = _('invoice item')
        verbose_name_plural = _('invoice items')
        ordering = ['id']
    
    def __str__(self):
        return f"{self.description} - {self.amount}"
    
    def save(self, *args, **kwargs):
        self.amount = self.quantity * self.unit_price
        super().save(*args, **kwargs)
        
        # Update invoice subtotal
        if self.invoice:
            items = self.invoice.items.all()
            self.invoice.subtotal = sum(item.amount for item in items)
            self.invoice.save()


class Payment(models.Model):
    """Payment records for invoices"""
    
    METHOD_CHOICES = [
        ('cash', 'Cash'),
        ('bank_transfer', 'Bank Transfer'),
        ('credit_card', 'Credit Card'),
        ('paypal', 'PayPal'),
        ('stripe', 'Stripe'),
        ('mobile_money', 'Mobile Money'),
        ('other', 'Other'),
    ]
    
    STATUS_CHOICES = [
        ('pending', 'Pending'),
        ('completed', 'Completed'),
        ('failed', 'Failed'),
        ('refunded', 'Refunded'),
    ]
    
    invoice = models.ForeignKey(Invoice, on_delete=models.CASCADE, related_name='payments')
    payment_method = models.CharField(max_length=20, choices=METHOD_CHOICES)
    amount = models.DecimalField(max_digits=10, decimal_places=2)
    status = models.CharField(max_length=20, choices=STATUS_CHOICES, default='pending')
    
    transaction_id = models.CharField(max_length=255, blank=True, null=True, 
                                     help_text="External transaction reference")
    payment_date = models.DateTimeField(auto_now_add=True)
    
    notes = models.TextField(blank=True, null=True)
    metadata = models.JSONField(default=dict, blank=True)
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        verbose_name = _('payment')
        verbose_name_plural = _('payments')
        ordering = ['-payment_date']
    
    def __str__(self):
        return f"Payment {self.id} - {self.invoice.invoice_number} - {self.amount}"
    
    def save(self, *args, **kwargs):
        super().save(*args, **kwargs)
        
        # Update invoice status if fully paid
        if self.status == 'completed':
            total_paid = sum(p.amount for p in self.invoice.payments.filter(status='completed'))
            if total_paid >= self.invoice.total:
                self.invoice.status = 'paid'
                if not self.invoice.paid_date:
                    self.invoice.paid_date = self.payment_date.date()
                self.invoice.save()


class BillingProfile(models.Model):
    """Billing profile and payment settings for clients"""
    
    client = models.OneToOneField('clients.Client', on_delete=models.CASCADE, related_name='billing_profile')
    
    # Payment preferences
    auto_pay = models.BooleanField(default=False, help_text="Automatically charge for recurring services")
    payment_method = models.CharField(max_length=20, blank=True, null=True)
    
    # Billing details
    billing_email = models.EmailField(blank=True, null=True)
    billing_phone = models.CharField(max_length=20, blank=True, null=True)
    billing_address = models.TextField(blank=True, null=True)
    
    # Payment gateway info (encrypted)
    stripe_customer_id = models.CharField(max_length=255, blank=True, null=True)
    paypal_email = models.EmailField(blank=True, null=True)
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        verbose_name = _('billing profile')
        verbose_name_plural = _('billing profiles')
    
    def __str__(self):
        return f"Billing Profile - {self.client.company_name}"

