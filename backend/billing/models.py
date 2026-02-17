from django.db import models
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
        ('paynow', 'Paynow'),
        ('ecocash', 'EcoCash'),
        ('onemoney', 'OneMoney'),
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


class Cart(models.Model):
    """Shopping cart for services with polymorphic support"""
    
    STATUS_CHOICES = [
        ('active', 'Active'),
        ('checkout', 'In Checkout'),
        ('completed', 'Completed'),
        ('abandoned', 'Abandoned'),
    ]
    
    client = models.ForeignKey('clients.Client', on_delete=models.CASCADE, related_name='carts', null=True, blank=True)
    session_id = models.CharField(max_length=255, blank=True, null=True, help_text="For non-authenticated users")
    status = models.CharField(max_length=20, choices=STATUS_CHOICES, default='active')
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        verbose_name = _('cart')
        verbose_name_plural = _('carts')
        ordering = ['-updated_at']
    
    def __str__(self):
        if self.client:
            return f"Cart - {self.client.company_name}"
        return f"Cart - Session {self.session_id}"
    
    def get_total(self):
        """Calculate total cart value"""
        return sum(item.get_price() for item in self.items.all())
    
    def get_item_count(self):
        """Get total number of items in cart"""
        return self.items.count()


class CartItem(models.Model):
    """Individual items in shopping cart with service-specific metadata"""
    
    cart = models.ForeignKey(Cart, on_delete=models.CASCADE, related_name='items')
    service = models.ForeignKey('services.Service', on_delete=models.CASCADE)
    
    # Polymorphic service metadata
    # For Hosting: {type: 'shared|vps|dedicated', ram: '4GB', cpu: '2 cores', region: 'US', os: 'Ubuntu'}
    # For Development: {type: 'web|mobile|desktop|hybrid', brief: 'Project requirements...'}
    # For Domains: {action: 'registration|transfer', domain_name: 'example.com', epp_code: 'ABC123'}
    service_metadata = models.JSONField(default=dict, help_text="Service-specific configuration")
    
    quantity = models.IntegerField(default=1)
    unit_price = models.DecimalField(max_digits=10, decimal_places=2)
    billing_cycle = models.CharField(max_length=20, default='monthly')
    
    created_at = models.DateTimeField(auto_now_add=True)
    
    class Meta:
        verbose_name = _('cart item')
        verbose_name_plural = _('cart items')
        ordering = ['id']
    
    def __str__(self):
        return f"{self.service.name} - {self.cart}"
    
    def get_price(self):
        """Calculate item price"""
        return self.quantity * self.unit_price
    
    def clean(self):
        """Validate service-specific metadata"""
        from django.core.exceptions import ValidationError
        
        if self.service.category == 'domain':
            # Validate domain transfer has EPP code
            if self.service_metadata.get('action') == 'transfer':
                if not self.service_metadata.get('epp_code'):
                    raise ValidationError({
                        'service_metadata': 'EPP/Auth code is required for domain transfers'
                    })
        
        super().clean()


class Expense(models.Model):
    """Track operational expenses for profit margin calculations"""
    
    CATEGORY_CHOICES = [
        ('server', 'Server/Hosting Costs'),
        ('domain', 'Domain Registry Costs'),
        ('license', 'Software Licenses'),
        ('cpanel', 'cPanel/Control Panel'),
        ('ssl', 'SSL Certificates'),
        ('bandwidth', 'Bandwidth/CDN'),
        ('marketing', 'Marketing & Advertising'),
        ('salary', 'Salaries & Wages'),
        ('office', 'Office & Utilities'),
        ('other', 'Other'),
    ]
    
    RECURRING_CHOICES = [
        ('none', 'One-time'),
        ('monthly', 'Monthly'),
        ('quarterly', 'Quarterly'),
        ('annual', 'Annual'),
    ]
    
    name = models.CharField(max_length=255, help_text="Expense description")
    category = models.CharField(max_length=50, choices=CATEGORY_CHOICES)
    amount = models.DecimalField(max_digits=10, decimal_places=2)
    recurring = models.CharField(max_length=20, choices=RECURRING_CHOICES, default='none')
    
    # Link to service if applicable
    service = models.ForeignKey('services.Service', on_delete=models.SET_NULL, 
                               null=True, blank=True, related_name='expenses',
                               help_text="Associated service (e.g., cPanel license for hosting)")
    
    # Vendor/Provider info
    vendor = models.CharField(max_length=255, blank=True, null=True, 
                             help_text="Provider/Vendor name (e.g., cPanel Inc, Namecheap)")
    
    # Dates
    expense_date = models.DateField(help_text="Date of expense")
    next_due_date = models.DateField(null=True, blank=True, help_text="Next payment due for recurring")
    
    # Reference
    reference_number = models.CharField(max_length=255, blank=True, null=True,
                                       help_text="Invoice/Reference number from vendor")
    
    notes = models.TextField(blank=True, null=True)
    is_paid = models.BooleanField(default=True)
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        verbose_name = _('expense')
        verbose_name_plural = _('expenses')
        ordering = ['-expense_date']
    
    def __str__(self):
        return f"{self.name} - {self.amount} ({self.expense_date})"


class Promotion(models.Model):
    """Promotions, discounts, and coupon codes"""
    
    TYPE_CHOICES = [
        ('discount', 'Discount'),
        ('bundle', 'Bundle'),
        ('free_service', 'Free Service'),
        ('referral', 'Referral'),
    ]
    
    DISCOUNT_TYPE_CHOICES = [
        ('percentage', 'Percentage'),
        ('fixed', 'Fixed Amount'),
    ]
    
    name = models.CharField(max_length=255, help_text="Promotion name")
    code = models.CharField(max_length=50, unique=True, blank=True, null=True, 
                           help_text="Coupon/promo code (leave blank if not a coupon)")
    promotion_type = models.CharField(max_length=20, choices=TYPE_CHOICES, default='discount')
    discount_type = models.CharField(max_length=20, choices=DISCOUNT_TYPE_CHOICES, default='percentage')
    discount_value = models.DecimalField(max_digits=10, decimal_places=2, 
                                        help_text="Discount amount (% or fixed currency)")
    
    description = models.TextField(blank=True, null=True)
    
    # Date range
    start_date = models.DateTimeField()
    end_date = models.DateTimeField()
    
    # Constraints
    is_active = models.BooleanField(default=True)
    usage_limit = models.IntegerField(null=True, blank=True, help_text="Max number of uses (null = unlimited)")
    usage_count = models.IntegerField(default=0, help_text="Number of times used")
    minimum_order_amount = models.DecimalField(max_digits=10, decimal_places=2, null=True, 
                                              blank=True, help_text="Minimum order total to apply")
    
    # Multi-select relationships
    applicable_services = models.ManyToManyField('services.Service', blank=True, 
                                                related_name='promotions',
                                                help_text="Leave blank to apply to all services")
    applicable_categories = models.JSONField(default=list, blank=True, 
                                            help_text="Service categories (domain, hosting, dev, etc.)")
    
    # Bundle-specific
    bundle_services = models.ManyToManyField('services.Service', blank=True, 
                                           related_name='bundled_in_promotions',
                                           help_text="Services in this bundle")
    
    # Free service specific
    free_service = models.ForeignKey('services.Service', on_delete=models.SET_NULL, 
                                    null=True, blank=True, related_name='+',
                                    help_text="Service given for free (free_service type)")
    free_service_duration = models.IntegerField(null=True, blank=True, 
                                               help_text="Duration in months")
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        verbose_name = _('promotion')
        verbose_name_plural = _('promotions')
        ordering = ['-start_date']
    
    def __str__(self):
        return f"{self.name} ({self.code or 'no code'})"
    
    def is_valid(self):
        """Check if promotion is currently valid"""
        from django.utils import timezone
        now = timezone.now()
        return self.is_active and self.start_date <= now <= self.end_date
    
    def can_be_used(self):
        """Check if promotion can still be used"""
        if not self.is_valid():
            return False
        if self.usage_limit and self.usage_count >= self.usage_limit:
            return False
        return True

