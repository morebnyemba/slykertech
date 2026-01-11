# WHMCS Patterns Implementation Guide

This guide documents the WHMCS-style patterns implemented for web hosting and domain registration management in the Slyker Tech platform.

## Overview

WHMCS (Web Host Manager Complete Solution) is the industry standard for web hosting billing and automation. This implementation follows WHMCS patterns to provide professional-grade hosting and domain management.

## Key WHMCS Patterns Implemented

### 1. Product-Based Hosting Packages

**WHMCS Pattern**: Hosting services are organized into pre-configured packages with specific resource limits.

**Implementation**: `HostingProduct` model

```python
from services.whmcs_models import HostingProduct

# Create a hosting package (like WHMCS product configuration)
basic_hosting = HostingProduct.objects.create(
    name="Basic Hosting",
    slug="basic-hosting",
    description="Perfect for small websites",
    
    # Resource limits
    disk_space=5000,  # 5 GB in MB
    bandwidth=50000,  # 50 GB in MB
    email_accounts=10,
    databases=5,
    ftp_accounts=5,
    subdomains=10,
    addon_domains=1,
    parked_domains=2,
    
    # Features
    ssl_certificate=True,
    dedicated_ip=False,
    cpanel_access=True,
    ssh_access=False,
    backups_included=True,
    
    # Pricing (like WHMCS pricing)
    monthly_price=9.99,
    quarterly_price=27.99,
    semi_annual_price=49.99,
    annual_price=99.99,
    
    # Setup fees
    setup_fee_monthly=5.00,
    setup_fee_annual=0.00,  # Free setup for annual
    
    # Server configuration
    cpanel_package_name="basic",
)
```

### 2. TLD-Specific Domain Pricing

**WHMCS Pattern**: Different pricing for each Top Level Domain with support for multi-year registration.

**Implementation**: `DomainProduct` model

```python
from services.whmcs_models import DomainProduct

# Configure .com domain pricing (like WHMCS domain pricing)
com_domain = DomainProduct.objects.create(
    tld=".com",
    description="Commercial domains",
    
    # Multi-year registration pricing
    registration_price_1yr=14.99,
    registration_price_2yr=28.99,
    registration_price_3yr=42.99,
    registration_price_5yr=69.99,
    registration_price_10yr=129.99,
    
    # Other pricing
    renewal_price=14.99,
    transfer_price=14.99,
    redemption_price=149.99,  # Recovery fee for expired domains
    
    # Addons
    whois_privacy_price=5.00,  # Annual ID protection fee
    
    # Settings
    auto_renew_default=True,
    epp_code_required=True,
    grace_period_days=30,  # After expiry
    redemption_period_days=30,  # After grace period
    
    # Registration limits
    min_registration_years=1,
    max_registration_years=10,
)

# Zimbabwe domains
zw_domain = DomainProduct.objects.create(
    tld=".co.zw",
    registration_price_1yr=25.00,
    renewal_price=25.00,
    transfer_price=25.00,
    whois_privacy_price=0.00,  # Not available for .co.zw
    auto_renew_default=True,
)
```

### 3. Service Addons

**WHMCS Pattern**: Additional features that can be purchased separately.

**Implementation**: `ServiceAddon` model

```python
from services.whmcs_models import ServiceAddon

# Create addons (like WHMCS product addons)
extra_disk = ServiceAddon.objects.create(
    name="Extra 5GB Disk Space",
    description="Add 5GB of additional disk space to your hosting",
    addon_type='disk_space',
    billing_type='recurring',
    
    monthly_price=2.99,
    quarterly_price=8.49,
    annual_price=29.99,
    
    quantity=5000,  # 5GB in MB
    requires_hosting=True,
)

dedicated_ip = ServiceAddon.objects.create(
    name="Dedicated IP Address",
    description="Get your own dedicated IP address",
    addon_type='ip_address',
    billing_type='recurring',
    
    monthly_price=3.99,
    annual_price=39.99,
    
    quantity=1,
    requires_hosting=True,
)

ssl_cert = ServiceAddon.objects.create(
    name="Premium SSL Certificate",
    description="Comodo PositiveSSL Certificate",
    addon_type='ssl_certificate',
    billing_type='recurring',
    
    annual_price=49.99,
    
    quantity=1,
)
```

### 4. Automated Billing Lifecycle

**WHMCS Pattern**: Automatic invoice generation, renewal reminders, and suspension for non-payment.

**Implementation**: Enhanced `ServiceSubscription` model with WHMCS fields

```python
from services.models import ServiceSubscription
from datetime import date, timedelta

# Create subscription with WHMCS-style fields
subscription = ServiceSubscription.objects.create(
    client=client,
    service=hosting_service,
    hosting_product=basic_hosting,  # Link to package
    
    billing_cycle='annual',
    price=basic_hosting.annual_price,
    start_date=date.today(),
    end_date=date.today() + timedelta(days=365),
    
    # WHMCS-style renewal management
    auto_renew=True,
    next_due_date=date.today() + timedelta(days=365),
    next_invoice_date=date.today() + timedelta(days=335),  # 30 days before
    
    # WHMCS-style suspension settings
    grace_period_days=14,  # 14 days after due date
    
    # Renewal reminders (sent by cron job)
    renewal_reminder_30d=False,
    renewal_reminder_14d=False,
    renewal_reminder_7d=False,
    renewal_reminder_1d=False,
)
```

### 5. Renewal Reminder System

**WHMCS Pattern**: Automated reminders sent at 30, 14, 7, and 1 days before service expiry.

**Implementation**: Django management command (like WHMCS cron)

```python
# services/management/commands/send_renewal_reminders.py
from django.core.management.base import BaseCommand
from datetime import date, timedelta
from services.models import ServiceSubscription
from notifications.whatsapp_service import WhatsAppService

class Command(BaseCommand):
    help = 'Send renewal reminders (run daily via cron)'
    
    def handle(self, *args, **options):
        today = date.today()
        
        # 30 days before expiry
        services_30d = ServiceSubscription.objects.filter(
            next_due_date=today + timedelta(days=30),
            renewal_reminder_30d=False,
            auto_renew=False,
            status='active'
        )
        for sub in services_30d:
            self.send_reminder(sub, days_left=30)
            sub.renewal_reminder_30d = True
            sub.save()
        
        # 14 days before expiry
        services_14d = ServiceSubscription.objects.filter(
            next_due_date=today + timedelta(days=14),
            renewal_reminder_14d=False,
            status='active'
        )
        for sub in services_14d:
            self.send_reminder(sub, days_left=14)
            sub.renewal_reminder_14d = True
            sub.save()
        
        # Similar for 7 days and 1 day...
        
    def send_reminder(self, subscription, days_left):
        # Send email and WhatsApp notification
        pass
```

**Cron Setup**:
```bash
# Add to crontab (run daily at 8 AM)
0 8 * * * cd /path/to/backend && python manage.py send_renewal_reminders
```

### 6. Automatic Suspension and Termination

**WHMCS Pattern**: Services are automatically suspended for non-payment, then terminated after extended non-payment.

**Implementation**: Django management command

```python
# services/management/commands/check_suspensions.py
from django.core.management.base import BaseCommand
from datetime import date
from services.models import ServiceSubscription

class Command(BaseCommand):
    help = 'Check for expired services and suspend/terminate (run daily)'
    
    def handle(self, *args, **options):
        today = date.today()
        
        # Find services past due date
        overdue_services = ServiceSubscription.objects.filter(
            next_due_date__lt=today,
            status='active'
        )
        
        for sub in overdue_services:
            days_overdue = (today - sub.next_due_date).days
            
            if days_overdue <= sub.grace_period_days:
                # Still in grace period - send warning
                self.send_warning(sub, days_until_suspension=sub.grace_period_days - days_overdue)
            
            elif days_overdue <= sub.grace_period_days + 30:
                # Grace period expired - suspend service
                if sub.status != 'suspended':
                    self.suspend_service(sub)
            
            else:
                # Extended non-payment - terminate service
                self.terminate_service(sub)
    
    def suspend_service(self, subscription):
        subscription.status = 'suspended'
        subscription.suspension_date = date.today()
        subscription.save()
        
        # Suspend cPanel account if hosting
        if subscription.service.provisioning_type == 'cpanel':
            cpanel_account = subscription.cpanel_accounts.first()
            if cpanel_account:
                # Call cPanel API to suspend account
                pass
        
        # Send suspension notification
        pass
    
    def terminate_service(self, subscription):
        subscription.status = 'terminated'
        subscription.termination_date = date.today()
        subscription.save()
        
        # Terminate cPanel account
        # Send termination notification
        pass
```

### 7. Domain Registration Workflow

**WHMCS Pattern**: Complete domain lifecycle management with expiry tracking and renewal.

**Implementation**: `DomainRegistration` model

```python
from services.whmcs_models import DomainRegistration

# Register a domain (like WHMCS domain registration)
domain = DomainRegistration.objects.create(
    client=client,
    domain_name="example.com",
    domain_product=com_domain,
    
    registration_date=date.today(),
    expiry_date=date.today() + timedelta(days=365),
    registration_years=1,
    
    status='active',
    auto_renew=True,
    
    # WHOIS privacy (like WHMCS ID protection)
    whois_privacy=True,
    whois_privacy_expiry=date.today() + timedelta(days=365),
    
    # Nameservers (like WHMCS)
    nameserver1='ns1.yourdomain.com',
    nameserver2='ns2.yourdomain.com',
    
    # For transfers
    is_transfer=False,
    epp_code='',  # Generated on client request
)

# Generate invoice for domain
invoice = Invoice.objects.create(
    client=client,
    due_date=date.today() + timedelta(days=7)
)

# Domain registration fee
InvoiceItem.objects.create(
    invoice=invoice,
    description=f"Domain Registration - {domain.domain_name} (1 year)",
    quantity=1,
    unit_price=com_domain.registration_price_1yr,
)

# WHOIS privacy addon
if domain.whois_privacy:
    InvoiceItem.objects.create(
        invoice=invoice,
        description=f"WHOIS Privacy - {domain.domain_name}",
        quantity=1,
        unit_price=com_domain.whois_privacy_price,
    )
```

### 8. Package Upgrades with Prorated Billing

**WHMCS Pattern**: Clients can upgrade hosting packages mid-cycle with prorated charges.

**Implementation**: Upgrade service

```python
def upgrade_hosting_package(subscription, new_hosting_product):
    """
    Upgrade hosting package with prorated billing (WHMCS pattern)
    """
    from decimal import Decimal
    
    # Calculate prorated amount
    days_remaining = (subscription.end_date - date.today()).days
    total_days = (subscription.end_date - subscription.start_date).days
    prorata_percentage = Decimal(days_remaining) / Decimal(total_days)
    
    # Price difference
    old_monthly = subscription.price / 12  # Convert to monthly
    new_monthly = new_hosting_product.get_price(subscription.billing_cycle) / 12
    price_difference = new_monthly - old_monthly
    
    # Prorated charge
    prorated_charge = price_difference * prorata_percentage * 12
    
    # Create upgrade invoice
    invoice = Invoice.objects.create(
        client=subscription.client,
        due_date=date.today() + timedelta(days=1)
    )
    
    InvoiceItem.objects.create(
        invoice=invoice,
        subscription=subscription,
        description=f"Upgrade to {new_hosting_product.name} (Prorated)",
        quantity=1,
        unit_price=prorated_charge,
    )
    
    # Update subscription after payment
    subscription.hosting_product = new_hosting_product
    subscription.price = new_hosting_product.get_price(subscription.billing_cycle)
    subscription.metadata.update({
        'disk_space': new_hosting_product.disk_space,
        'bandwidth': new_hosting_product.bandwidth,
        'email_accounts': new_hosting_product.email_accounts,
    })
    subscription.save()
    
    # Update cPanel account resources
    from services.provisioning import provisioning_service
    provisioning_service.upgrade_cpanel_resources(subscription)
    
    return invoice
```

## Benefits of WHMCS Patterns

### 1. Industry Standard
- Familiar to hosting industry professionals
- Battle-tested workflows
- Proven billing practices

### 2. Automated Lifecycle
- Automatic invoice generation
- Scheduled renewal reminders
- Automated suspension/termination
- Reduces manual administrative work

### 3. Flexible Pricing
- Multiple billing cycles per product
- Prorated upgrades/downgrades
- Setup fees support
- Addon pricing

### 4. Professional Features
- TLD-specific domain pricing
- WHOIS privacy management
- Grace and redemption periods
- EPP code handling

### 5. Client Convenience
- Auto-renewal options
- Package upgrade paths
- Service addons
- Transparent pricing

## Cron Jobs (WHMCS Pattern)

WHMCS runs several automated tasks via cron. Implement these Django management commands:

```bash
# Add to crontab
0 8 * * * cd /path/to/backend && python manage.py send_renewal_reminders
0 9 * * * cd /path/to/backend && python manage.py check_suspensions
0 10 * * * cd /path/to/backend && python manage.py generate_renewal_invoices
0 11 * * * cd /path/to/backend && python manage.py check_domain_renewals
```

## Admin Interface

Register WHMCS models in Django admin:

```python
# services/admin.py
from django.contrib import admin
from .whmcs_models import HostingProduct, DomainProduct, ServiceAddon, DomainRegistration

@admin.register(HostingProduct)
class HostingProductAdmin(admin.ModelAdmin):
    list_display = ['name', 'monthly_price', 'annual_price', 'disk_space', 'bandwidth', 'is_active']
    list_filter = ['is_active', 'is_featured']
    search_fields = ['name', 'description']

@admin.register(DomainProduct)
class DomainProductAdmin(admin.ModelAdmin):
    list_display = ['tld', 'registration_price_1yr', 'renewal_price', 'is_active']
    list_filter = ['is_active', 'is_featured']
    search_fields = ['tld']

@admin.register(ServiceAddon)
class ServiceAddonAdmin(admin.ModelAdmin):
    list_display = ['name', 'addon_type', 'billing_type', 'monthly_price', 'is_active']
    list_filter = ['addon_type', 'billing_type', 'is_active']

@admin.register(DomainRegistration)
class DomainRegistrationAdmin(admin.ModelAdmin):
    list_display = ['domain_name', 'client', 'expiry_date', 'status', 'auto_renew']
    list_filter = ['status', 'auto_renew', 'whois_privacy']
    search_fields = ['domain_name', 'client__company_name']
    date_hierarchy = 'expiry_date'
```

## Migration to WHMCS Patterns

### Step 1: Create WHMCS Models
```bash
cd backend
python manage.py makemigrations
python manage.py migrate
```

### Step 2: Populate Hosting Products
```python
from services.whmcs_models import HostingProduct

# Create hosting packages
HostingProduct.objects.create(
    name="Basic Hosting",
    slug="basic",
    disk_space=5000,
    bandwidth=50000,
    monthly_price=9.99,
    annual_price=99.99,
    # ... other fields
)
```

### Step 3: Configure Domain TLDs
```python
from services.whmcs_models import DomainProduct

# Popular TLDs
for tld_data in [
    {'.com': 14.99}, {'.net': 14.99}, {'.org': 14.99},
    {'.co.zw': 25.00}, {'.biz': 16.99},
]:
    for tld, price in tld_data.items():
        DomainProduct.objects.create(
            tld=tld,
            registration_price_1yr=price,
            renewal_price=price,
            transfer_price=price,
        )
```

### Step 4: Link Existing Subscriptions
```python
# Update existing hosting subscriptions to use HostingProduct
from services.models import ServiceSubscription, Service
from services.whmcs_models import HostingProduct

hosting_service = Service.objects.get(name='Web Hosting')
basic_product = HostingProduct.objects.get(slug='basic')

ServiceSubscription.objects.filter(
    service=hosting_service,
    hosting_product__isnull=True
).update(hosting_product=basic_product)
```

## Conclusion

This WHMCS-style implementation provides enterprise-grade hosting and domain management with automated billing, professional lifecycle management, and industry-standard workflows. The patterns are proven in production environments serving millions of customers worldwide.
