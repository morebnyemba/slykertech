"""
WHMCS-style models for hosting and domain management
Following industry-standard patterns from WHMCS
"""
from django.db import models
from django.utils.translation import gettext_lazy as _
from decimal import Decimal


class HostingProduct(models.Model):
    """
    WHMCS-style hosting product/package definition
    Defines resource limits and pricing for hosting packages
    """
    
    HOSTING_TYPE_CHOICES = [
        ('shared', 'Shared Hosting'),
        ('vps', 'VPS Hosting'),
        ('dedicated', 'Dedicated Server'),
        ('cloud', 'Cloud Hosting'),
        ('reseller', 'Reseller Hosting'),
    ]
    
    name = models.CharField(max_length=255, help_text="e.g., Basic Hosting, Business Hosting")
    slug = models.SlugField(unique=True)
    description = models.TextField()
    hosting_type = models.CharField(max_length=20, choices=HOSTING_TYPE_CHOICES, default='shared',
                                    help_text="Type of hosting package")
    
    # Resource Limits (like WHMCS product configuration)
    disk_space = models.IntegerField(help_text="Disk space in MB, 0 = unlimited")
    bandwidth = models.IntegerField(help_text="Bandwidth in MB per month, 0 = unlimited")
    
    # VPS/Dedicated Server Specs
    cpu_cores = models.IntegerField(default=0, help_text="Number of CPU cores (for VPS/Dedicated), 0 for shared hosting")
    ram_gb = models.IntegerField(default=0, help_text="RAM in GB (for VPS/Dedicated), 0 for shared hosting")
    cpu_type = models.CharField(max_length=200, blank=True, null=True, help_text="CPU type/model (for Dedicated servers)")
    
    STORAGE_TYPE_CHOICES = [
        ('HDD', 'HDD'),
        ('SSD', 'SSD'),
        ('NVMe', 'NVMe SSD'),
    ]
    storage_type = models.CharField(max_length=50, default='SSD', choices=STORAGE_TYPE_CHOICES, help_text="Storage type")
    
    # Shared Hosting Limits
    email_accounts = models.IntegerField(help_text="Number of email accounts, 0 = unlimited")
    databases = models.IntegerField(help_text="Number of databases, 0 = unlimited")
    ftp_accounts = models.IntegerField(help_text="Number of FTP accounts, 0 = unlimited")
    subdomains = models.IntegerField(help_text="Number of subdomains, 0 = unlimited")
    addon_domains = models.IntegerField(help_text="Number of addon domains")
    parked_domains = models.IntegerField(help_text="Number of parked domains")
    
    # Features (like WHMCS product features)
    ssl_certificate = models.BooleanField(default=False, help_text="Free SSL certificate included")
    dedicated_ip = models.BooleanField(default=False, help_text="Dedicated IP address included")
    cpanel_access = models.BooleanField(default=True)
    ssh_access = models.BooleanField(default=False)
    cron_jobs = models.BooleanField(default=True)
    backups_included = models.BooleanField(default=True, help_text="Automated backups")
    
    # Pricing (like WHMCS product pricing)
    monthly_price = models.DecimalField(max_digits=10, decimal_places=2)
    quarterly_price = models.DecimalField(max_digits=10, decimal_places=2, null=True, blank=True)
    semi_annual_price = models.DecimalField(max_digits=10, decimal_places=2, null=True, blank=True)
    annual_price = models.DecimalField(max_digits=10, decimal_places=2, null=True, blank=True)
    biennial_price = models.DecimalField(max_digits=10, decimal_places=2, null=True, blank=True)
    triennial_price = models.DecimalField(max_digits=10, decimal_places=2, null=True, blank=True)
    
    # Setup fees (like WHMCS)
    setup_fee_monthly = models.DecimalField(max_digits=10, decimal_places=2, default=Decimal('0.00'))
    setup_fee_quarterly = models.DecimalField(max_digits=10, decimal_places=2, default=Decimal('0.00'))
    setup_fee_annual = models.DecimalField(max_digits=10, decimal_places=2, default=Decimal('0.00'))
    
    # Server configuration
    server_id = models.CharField(max_length=50, blank=True, null=True, help_text="Server identifier for provisioning")
    cpanel_package_name = models.CharField(max_length=100, blank=True, null=True, help_text="cPanel package name")
    
    # Display settings
    is_featured = models.BooleanField(default=False)
    sort_order = models.IntegerField(default=0)
    is_active = models.BooleanField(default=True)
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        verbose_name = _('hosting product')
        verbose_name_plural = _('hosting products')
        ordering = ['sort_order', 'name']
    
    def __str__(self):
        return self.name
    
    def get_price(self, billing_cycle):
        """Get price for specific billing cycle (WHMCS pattern)"""
        price_map = {
            'monthly': self.monthly_price,
            'quarterly': self.quarterly_price,
            'semi_annual': self.semi_annual_price,
            'annual': self.annual_price,
            'biennial': self.biennial_price,
            'triennial': self.triennial_price,
        }
        return price_map.get(billing_cycle, self.monthly_price)
    
    def get_setup_fee(self, billing_cycle):
        """Get setup fee for specific billing cycle"""
        fee_map = {
            'monthly': self.setup_fee_monthly,
            'quarterly': self.setup_fee_quarterly,
            'annual': self.setup_fee_annual,
        }
        return fee_map.get(billing_cycle, Decimal('0.00'))


class DomainProduct(models.Model):
    """
    WHMCS-style domain product configuration
    Defines pricing per TLD (Top Level Domain)
    """
    tld = models.CharField(max_length=50, unique=True, help_text="e.g., .com, .net, .co.zw")
    description = models.CharField(max_length=255, blank=True)
    
    # Pricing (like WHMCS domain pricing)
    registration_price_1yr = models.DecimalField(max_digits=10, decimal_places=2, help_text="1 year registration")
    registration_price_2yr = models.DecimalField(max_digits=10, decimal_places=2, null=True, blank=True)
    registration_price_3yr = models.DecimalField(max_digits=10, decimal_places=2, null=True, blank=True)
    registration_price_5yr = models.DecimalField(max_digits=10, decimal_places=2, null=True, blank=True)
    registration_price_10yr = models.DecimalField(max_digits=10, decimal_places=2, null=True, blank=True)
    
    renewal_price = models.DecimalField(max_digits=10, decimal_places=2, help_text="Annual renewal price")
    transfer_price = models.DecimalField(max_digits=10, decimal_places=2, help_text="Domain transfer price")
    redemption_price = models.DecimalField(max_digits=10, decimal_places=2, null=True, blank=True,
                                          help_text="Grace/redemption period fee for expired domains")
    
    # Addons (like WHMCS)
    whois_privacy_price = models.DecimalField(max_digits=10, decimal_places=2, default=Decimal('0.00'),
                                             help_text="ID Protection/WHOIS privacy annual fee")
    
    # Settings (like WHMCS domain settings)
    auto_renew_default = models.BooleanField(default=True, help_text="Auto-renew enabled by default")
    epp_code_required = models.BooleanField(default=True, help_text="Requires EPP/auth code for transfer")
    grace_period_days = models.IntegerField(default=30, help_text="Grace period after expiry")
    redemption_period_days = models.IntegerField(default=30, help_text="Redemption period after grace")
    
    # Registration settings
    min_registration_years = models.IntegerField(default=1)
    max_registration_years = models.IntegerField(default=10)
    
    # Display
    is_active = models.BooleanField(default=True)
    is_featured = models.BooleanField(default=False)
    sort_order = models.IntegerField(default=0)
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        verbose_name = _('domain product')
        verbose_name_plural = _('domain products')
        ordering = ['sort_order', 'tld']
    
    def __str__(self):
        return self.tld
    
    def get_registration_price(self, years=1):
        """Get registration price for specified years"""
        price_map = {
            1: self.registration_price_1yr,
            2: self.registration_price_2yr,
            3: self.registration_price_3yr,
            5: self.registration_price_5yr,
            10: self.registration_price_10yr,
        }
        return price_map.get(years, self.registration_price_1yr * years)


class ServiceAddon(models.Model):
    """
    WHMCS-style service addons
    Additional features that can be added to services
    """
    ADDON_TYPE_CHOICES = [
        ('disk_space', 'Extra Disk Space'),
        ('bandwidth', 'Extra Bandwidth'),
        ('email_accounts', 'Additional Email Accounts'),
        ('databases', 'Additional Databases'),
        ('ip_address', 'Dedicated IP Address'),
        ('ssl_certificate', 'SSL Certificate'),
        ('backup_service', 'Backup Service'),
        ('cdn', 'CDN Service'),
        ('other', 'Other'),
    ]
    
    BILLING_TYPE_CHOICES = [
        ('one_time', 'One Time Fee'),
        ('recurring', 'Recurring Fee'),
        ('free', 'Free'),
    ]
    
    name = models.CharField(max_length=255)
    description = models.TextField()
    addon_type = models.CharField(max_length=50, choices=ADDON_TYPE_CHOICES)
    billing_type = models.CharField(max_length=20, choices=BILLING_TYPE_CHOICES, default='recurring')
    
    # Pricing (like WHMCS addon pricing)
    monthly_price = models.DecimalField(max_digits=10, decimal_places=2, default=Decimal('0.00'))
    quarterly_price = models.DecimalField(max_digits=10, decimal_places=2, null=True, blank=True)
    annual_price = models.DecimalField(max_digits=10, decimal_places=2, null=True, blank=True)
    one_time_price = models.DecimalField(max_digits=10, decimal_places=2, null=True, blank=True)
    
    # Configuration
    quantity = models.IntegerField(default=1, help_text="Amount to add (e.g., 5000 MB disk, 1 IP)")
    is_active = models.BooleanField(default=True)
    
    # Restrictions
    requires_hosting = models.BooleanField(default=True, help_text="Requires active hosting service")
    compatible_products = models.ManyToManyField(HostingProduct, blank=True,
                                                help_text="Compatible hosting products (empty = all)")
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        verbose_name = _('service addon')
        verbose_name_plural = _('service addons')
        ordering = ['name']
    
    def __str__(self):
        return self.name
    
    def get_price(self, billing_cycle):
        """Get addon price for specific billing cycle"""
        if self.billing_type == 'one_time':
            return self.one_time_price or Decimal('0.00')
        
        price_map = {
            'monthly': self.monthly_price,
            'quarterly': self.quarterly_price,
            'annual': self.annual_price,
        }
        return price_map.get(billing_cycle, self.monthly_price)


class DomainRegistration(models.Model):
    """
    WHMCS-style domain registration management
    Tracks domain registrations with all WHMCS features
    """
    STATUS_CHOICES = [
        ('active', 'Active'),
        ('pending', 'Pending Registration'),
        ('pending_transfer', 'Pending Transfer'),
        ('expired', 'Expired'),
        ('cancelled', 'Cancelled'),
        ('transferred_away', 'Transferred Away'),
        ('fraud', 'Fraud'),
        ('pending_delete', 'Pending Delete'),
        ('redemption', 'Redemption Period'),
    ]
    
    client = models.ForeignKey('clients.Client', on_delete=models.CASCADE, related_name='domains')
    domain_name = models.CharField(max_length=255, help_text="Full domain name (e.g., example.com)")
    domain_product = models.ForeignKey(DomainProduct, on_delete=models.PROTECT, related_name='registrations')
    
    # Registration details (like WHMCS)
    registration_date = models.DateField()
    expiry_date = models.DateField()
    registration_years = models.IntegerField(default=1)
    
    # Status and settings
    status = models.CharField(max_length=20, choices=STATUS_CHOICES, default='pending')
    auto_renew = models.BooleanField(default=True, help_text="Automatically renew before expiry")
    
    # WHOIS/ID Protection (like WHMCS)
    whois_privacy = models.BooleanField(default=False, help_text="ID Protection/WHOIS privacy enabled")
    whois_privacy_expiry = models.DateField(null=True, blank=True)
    
    # Transfer details (like WHMCS)
    epp_code = models.CharField(max_length=100, blank=True, null=True, help_text="EPP/Authorization code")
    is_transfer = models.BooleanField(default=False, help_text="Was this a transfer-in?")
    
    # Nameservers (like WHMCS)
    nameserver1 = models.CharField(max_length=255, blank=True, default='ns1.yourdomain.com')
    nameserver2 = models.CharField(max_length=255, blank=True, default='ns2.yourdomain.com')
    nameserver3 = models.CharField(max_length=255, blank=True, null=True)
    nameserver4 = models.CharField(max_length=255, blank=True, null=True)
    nameserver5 = models.CharField(max_length=255, blank=True, null=True)
    
    # Renewal reminders (like WHMCS)
    renewal_reminder_30d = models.BooleanField(default=False)
    renewal_reminder_14d = models.BooleanField(default=False)
    renewal_reminder_7d = models.BooleanField(default=False)
    renewal_reminder_1d = models.BooleanField(default=False)
    
    # Registrar details
    registrar_id = models.CharField(max_length=100, blank=True, null=True, help_text="Domain ID at registrar")
    
    notes = models.TextField(blank=True, null=True)
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        verbose_name = _('domain registration')
        verbose_name_plural = _('domain registrations')
        ordering = ['-expiry_date']
        unique_together = ['domain_name', 'client']
    
    def __str__(self):
        return self.domain_name
    
    def is_expired(self):
        """Check if domain is expired"""
        from datetime import date
        return self.expiry_date < date.today()
    
    def days_until_expiry(self):
        """Days until domain expires"""
        from datetime import date
        delta = self.expiry_date - date.today()
        return delta.days
    
    def in_grace_period(self):
        """Check if domain is in grace period"""
        days_expired = abs(self.days_until_expiry())
        return self.is_expired() and days_expired <= self.domain_product.grace_period_days
    
    def in_redemption_period(self):
        """Check if domain is in redemption period"""
        days_expired = abs(self.days_until_expiry())
        grace_days = self.domain_product.grace_period_days
        redemption_days = self.domain_product.redemption_period_days
        return self.is_expired() and (grace_days < days_expired <= grace_days + redemption_days)


class DomainTransferRequest(models.Model):
    """
    Model for handling domain transfer requests
    Stores all information needed to process a domain transfer
    """
    STATUS_CHOICES = [
        ('pending', 'Pending Review'),
        ('awaiting_epp', 'Awaiting EPP Code'),
        ('in_progress', 'Transfer In Progress'),
        ('completed', 'Transfer Completed'),
        ('failed', 'Transfer Failed'),
        ('cancelled', 'Cancelled'),
    ]
    
    # Client making the transfer request
    client = models.ForeignKey('clients.Client', on_delete=models.CASCADE, 
                              related_name='transfer_requests',
                              null=True, blank=True,
                              help_text="Associated client (if logged in)")
    
    # Domain information
    domain_name = models.CharField(max_length=255, help_text="Full domain name to transfer (e.g., example.com)")
    
    # Contact information (for non-logged-in users)
    contact_email = models.EmailField(help_text="Contact email for transfer updates")
    contact_name = models.CharField(max_length=255, help_text="Contact person's name")
    contact_phone = models.CharField(max_length=50, blank=True, null=True, help_text="Contact phone number")
    
    # Transfer credentials
    epp_code = models.CharField(max_length=100, blank=True, null=True, 
                               help_text="EPP/Authorization code from current registrar")
    current_registrar = models.CharField(max_length=255, blank=True, null=True,
                                        help_text="Current domain registrar")
    
    # Domain ownership verification
    admin_email = models.EmailField(blank=True, null=True,
                                   help_text="Admin email listed in WHOIS (for verification)")
    owns_domain = models.BooleanField(default=False, 
                                     help_text="User confirms they own this domain")
    
    # Transfer status and processing
    status = models.CharField(max_length=20, choices=STATUS_CHOICES, default='pending')
    status_message = models.TextField(blank=True, null=True,
                                     help_text="Additional status information or error messages")
    
    # Nameserver preferences
    update_nameservers = models.BooleanField(default=True,
                                            help_text="Update nameservers to our servers after transfer")
    nameserver1 = models.CharField(max_length=255, blank=True, null=True)
    nameserver2 = models.CharField(max_length=255, blank=True, null=True)
    
    # Additional options
    whois_privacy = models.BooleanField(default=False, 
                                       help_text="Enable WHOIS privacy after transfer")
    auto_renew = models.BooleanField(default=True,
                                    help_text="Enable auto-renewal after transfer")
    
    # Admin notes
    admin_notes = models.TextField(blank=True, null=True,
                                  help_text="Internal notes for staff")
    
    # Timestamps
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    completed_at = models.DateTimeField(blank=True, null=True,
                                       help_text="When the transfer was completed")
    
    class Meta:
        verbose_name = _('domain transfer request')
        verbose_name_plural = _('domain transfer requests')
        ordering = ['-created_at']
    
    def __str__(self):
        return f"Transfer: {self.domain_name} ({self.get_status_display()})"
    
    def get_tld(self):
        """Extract TLD from domain name"""
        if '.' in self.domain_name:
            return self.domain_name.split('.')[-1]
        return None
