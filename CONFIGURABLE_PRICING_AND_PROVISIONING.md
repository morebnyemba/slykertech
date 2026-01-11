# Configurable Pricing and Automatic Provisioning Guide

This guide explains the new configurable pricing system and automatic provisioning features added to the Slyker Tech backend.

## Table of Contents

1. [Configurable Pricing](#configurable-pricing)
2. [Payment Types](#payment-types)
3. [Automatic Provisioning](#automatic-provisioning)
4. [Service Examples](#service-examples)
5. [API Usage](#api-usage)
6. [Troubleshooting](#troubleshooting)

---

## Configurable Pricing

Each service now supports multiple pricing tiers/options through the `pricing_options` JSON field.

### Structure

```python
pricing_options = {
    'tier_name': {
        'price': Decimal('99.99'),
        'description': 'Description of what this tier includes'
    },
    # ... more tiers
}
```

### Example: Web Hosting Pricing

```python
pricing_options = {
    'monthly': {
        'price': 9.99,
        'description': 'Basic hosting - 5GB storage'
    },
    'quarterly': {
        'price': 27.99,
        'description': '3 months - 10GB storage'
    },
    'annual': {
        'price': 99.99,
        'description': '12 months - 20GB storage + free domain'
    }
}
```

### Accessing Pricing Options

```python
from services.models import Service

service = Service.objects.get(name='Web Hosting')

# Get all pricing options
pricing_options = service.pricing_options

# Get specific tier price
monthly_price = pricing_options['monthly']['price']

# Display to user
for tier_name, tier_data in pricing_options.items():
    print(f"{tier_name}: ${tier_data['price']} - {tier_data['description']}")
```

---

## Payment Types

Services are categorized by payment structure:

### Payment Type Options

1. **one_time**: Single payment for service delivery
   - Examples: Web development, Mobile app development, Cyber security audit
   - Billing cycle: `one_time` only

2. **recurring**: Ongoing subscription service
   - Examples: Web hosting, Domain registration, IT support
   - Billing cycles: `monthly`, `quarterly`, `semi_annual`, `annual`

3. **both**: Service can be offered as one-time or recurring
   - Flexible billing based on client preference

### Service Classification

**One-Time Payment Services**:
- Custom Software Development
- Mobile App Development
- E-commerce Solutions
- Web Design & Development
- Database Solutions
- Cyber Security
- Business Intelligence
- Video Production

**Recurring Payment Services**:
- Web Hosting (requires provisioning)
- Domain Registration (requires provisioning)
- Digital Marketing
- Cloud Services
- IT Support & Maintenance

### Validating Payment Type

```python
from services.models import Service, ServiceSubscription

service = Service.objects.get(name='Web Hosting')

# Check if service allows recurring billing
if service.payment_type in ['recurring', 'both']:
    # Allow monthly, quarterly, annual options
    allowed_cycles = ['monthly', 'quarterly', 'semi_annual', 'annual']
elif service.payment_type == 'one_time':
    # Only allow one-time billing
    allowed_cycles = ['one_time']
```

---

## Automatic Provisioning

The system automatically provisions resources when subscriptions are created for services that require it.

### How It Works

1. **Service Configuration**:
   ```python
   service.requires_provisioning = True
   service.provisioning_type = 'cpanel'  # or 'directadmin', 'domain'
   ```

2. **Subscription Created**:
   ```python
   subscription = ServiceSubscription.objects.create(
       client=client,
       service=service,
       # ... other fields
   )
   ```

3. **Signal Triggered**: Django signal automatically calls `ProvisioningService`

4. **Resource Created**: cPanel account, DirectAdmin account, or domain registered

5. **Status Updated**: Subscription moves from 'pending' → 'provisioning' → 'active'

### Provisioning Types

#### 1. cPanel Account Creation

```python
subscription = ServiceSubscription.objects.create(
    client=client,
    service=web_hosting_service,
    billing_cycle='monthly',
    price=9.99,
    start_date=date.today(),
    metadata={
        'domain': 'clientwebsite.com',
        'cpanel_username': 'clientweb',  # Optional, auto-generated if not provided
        'disk_quota': 5120,  # 5GB in MB
        'bandwidth_quota': 51200,  # 50GB in MB
        'plan': 'basic'
    }
)

# Provisioning happens automatically!
# - Generates secure 16-character password
# - Creates cPanel account via WHM API
# - Stores encrypted credentials
# - Updates subscription status
```

**Result**:
- cPanel account created on server
- `IntegrationCredential` created with encrypted password
- `cPanelAccount` record linked to subscription
- `subscription.provisioning_completed = True`
- `subscription.status = 'active'`

#### 2. DirectAdmin Account Creation

```python
subscription = ServiceSubscription.objects.create(
    client=client,
    service=hosting_service,
    metadata={
        'domain': 'clientsite.com',
        'da_username': 'clientuser',
        'disk_quota': 5120,
        'bandwidth_quota': 51200
    }
)

# Auto-provisions DirectAdmin account
```

#### 3. Domain Registration

```python
subscription = ServiceSubscription.objects.create(
    client=client,
    service=domain_service,
    metadata={
        'domain': 'clientdomain.com',
        'years': 1
    }
)

# Auto-registers domain via Namecheap API
```

### Provisioning Status Tracking

```python
from services.models import ServiceSubscription

subscription = ServiceSubscription.objects.get(id=123)

if subscription.status == 'provisioning':
    print("Provisioning in progress...")
elif subscription.provisioning_completed:
    print("Provisioning completed successfully!")
    # Get provisioned resources
    if subscription.service.provisioning_type == 'cpanel':
        cpanel_account = subscription.cpanel_accounts.first()
        print(f"cPanel URL: https://{cpanel_account.domain}:2083")
        print(f"Username: {cpanel_account.cpanel_username}")
        # Get encrypted password from credential
        credential = cpanel_account.credential
        password = credential.get_password()
elif subscription.provisioning_error:
    print(f"Provisioning failed: {subscription.provisioning_error}")
```

### Manual Provisioning

If automatic provisioning fails or you need to retry:

```python
from services.provisioning import provisioning_service

subscription = ServiceSubscription.objects.get(id=123)
success, message = provisioning_service.provision_subscription(subscription)

if success:
    print(f"Provisioned: {message}")
else:
    print(f"Error: {message}")
    # Check subscription.provisioning_error for details
```

---

## Service Examples

### Example 1: Web Hosting with Auto-Provisioning

```python
from services.models import Service, ServiceSubscription
from clients.models import Client
from datetime import date, timedelta

# Get service
service = Service.objects.get(name='Web Hosting')

# Create subscription
subscription = ServiceSubscription.objects.create(
    client=client,
    service=service,
    billing_cycle='monthly',  # Recurring service
    price=9.99,  # From pricing_options['monthly']['price']
    start_date=date.today(),
    end_date=date.today() + timedelta(days=30),
    auto_renew=True,
    metadata={
        'domain': 'myclient.com',
        'disk_quota': 5120,  # 5GB
        'bandwidth_quota': 51200,  # 50GB
        'plan': 'basic'
    }
)

# Wait a few seconds for provisioning
time.sleep(5)

# Check status
subscription.refresh_from_db()
print(f"Status: {subscription.status}")
print(f"Provisioned: {subscription.provisioning_completed}")

# Get cPanel details
if subscription.provisioning_completed:
    cpanel_account = subscription.cpanel_accounts.first()
    credential = cpanel_account.credential
    print(f"cPanel Access:")
    print(f"  URL: https://{cpanel_account.domain}:2083")
    print(f"  Username: {cpanel_account.cpanel_username}")
    print(f"  Password: {credential.get_password()}")  # Decrypted
```

### Example 2: Web Development (One-Time Payment)

```python
# Get service with pricing options
service = Service.objects.get(name='Custom Software Development')

# Display pricing tiers to client
for tier, details in service.pricing_options.items():
    print(f"{tier.title()}: ${details['price']} - {details['description']}")

# Client selects 'standard' tier
selected_tier = 'standard'
price = service.pricing_options[selected_tier]['price']

# Create subscription
subscription = ServiceSubscription.objects.create(
    client=client,
    service=service,
    billing_cycle='one_time',  # One-time payment
    price=price,  # 5999.99
    start_date=date.today(),
    status='pending',
    metadata={
        'selected_tier': selected_tier,
        'project_requirements': 'Custom CRM with integrations'
    }
)

# No automatic provisioning for development services
# Instead, create a ProjectTracker
from services.models import ProjectTracker

project = ProjectTracker.objects.create(
    subscription=subscription,
    title='Custom CRM Development',
    description='Custom CRM with integrations',
    status='not_started',
    priority='high',
    estimated_hours=200
)
```

---

## API Usage

### REST API Endpoints

#### Get Service with Pricing Options

```bash
GET /api/services/services/{id}/
```

Response:
```json
{
    "id": 1,
    "name": "Web Hosting",
    "category": "hosting",
    "description": "Reliable web hosting with cPanel access",
    "base_price": "9.99",
    "payment_type": "recurring",
    "pricing_options": {
        "monthly": {
            "price": 9.99,
            "description": "Basic hosting - 5GB"
        },
        "quarterly": {
            "price": 27.99,
            "description": "3 months - 10GB"
        },
        "annual": {
            "price": 99.99,
            "description": "12 months - 20GB + free domain"
        }
    },
    "requires_provisioning": true,
    "provisioning_type": "cpanel",
    "features": [
        "cPanel Access",
        "Email Accounts",
        "MySQL Databases",
        "24/7 Support",
        "SSL Certificate"
    ]
}
```

#### Create Subscription with Auto-Provisioning

```bash
POST /api/services/subscriptions/
Content-Type: application/json

{
    "service": 1,
    "billing_cycle": "monthly",
    "price": 9.99,
    "start_date": "2024-01-15",
    "metadata": {
        "domain": "clientsite.com",
        "disk_quota": 5120,
        "bandwidth_quota": 51200
    }
}
```

Response:
```json
{
    "id": 42,
    "service": 1,
    "status": "provisioning",
    "billing_cycle": "monthly",
    "price": "9.99",
    "provisioning_completed": false,
    "provisioning_error": null,
    "metadata": {
        "domain": "clientsite.com",
        "disk_quota": 5120,
        "bandwidth_quota": 51200
    }
}
```

After a few seconds, check again:
```bash
GET /api/services/subscriptions/42/
```

Response:
```json
{
    "id": 42,
    "status": "active",
    "provisioning_completed": true,
    "metadata": {
        "domain": "clientsite.com",
        "cpanel_account_id": 15,
        "cpanel_username": "clientsi",
        "provisioned_at": "2024-01-15T10:30:45.123Z"
    }
}
```

---

## Troubleshooting

### Provisioning Failed

If `provisioning_error` is set:

```python
subscription = ServiceSubscription.objects.get(id=123)
if subscription.provisioning_error:
    print(f"Error: {subscription.provisioning_error}")
    
    # Common errors:
    # - "No cPanel server credentials configured"
    #   → Add server IntegrationCredential with provider='cpanel'
    #
    # - "cPanel account creation failed: Username already exists"
    #   → Change cpanel_username in metadata
    #
    # - "Domain xyz.com is not available for registration"
    #   → Domain already registered
```

### Manual Retry

```python
from services.provisioning import provisioning_service

# Fix the issue (e.g., update metadata)
subscription.metadata['cpanel_username'] = 'newusername'
subscription.save()

# Reset provisioning status
subscription.provisioning_completed = False
subscription.provisioning_error = None
subscription.save()

# Retry provisioning
success, message = provisioning_service.provision_subscription(subscription)
```

### Check Server Credentials

```python
from integrations.models import IntegrationCredential

# Check if cPanel server credentials exist
server_creds = IntegrationCredential.objects.filter(
    provider='cpanel',
    is_active=True,
    client__isnull=True  # Server-level credential
)

if not server_creds.exists():
    print("No cPanel server credentials configured!")
    print("Add one via Django admin or:")
    
    cred = IntegrationCredential.objects.create(
        provider='cpanel',
        name='Main cPanel Server',
        host='server.example.com',
        username='root',
        port=2087,
        is_active=True
    )
    cred.set_api_token('your-whm-api-token')
    cred.save()
```

### Disable Auto-Provisioning Temporarily

```python
# In services/signals.py, comment out the signal:

# @receiver(post_save, sender=ServiceSubscription)
# def auto_provision_subscription(sender, instance, created, **kwargs):
#     # ... provisioning code
```

Or set `requires_provisioning=False` on the service:

```python
service = Service.objects.get(name='Web Hosting')
service.requires_provisioning = False
service.save()
```

---

## Best Practices

1. **Always set server credentials first** before enabling auto-provisioning
2. **Test in sandbox/development** environment before production
3. **Monitor provisioning errors** and set up alerts
4. **Use metadata field** for service-specific configuration
5. **Keep pricing_options updated** with current offerings
6. **Set appropriate quotas** based on pricing tier
7. **Enable auto_renew** for recurring services
8. **Log provisioning events** for audit trail

---

## Support

For issues or questions:
- Check Django logs for detailed error messages
- Review `provisioning_error` field on failed subscriptions
- Verify API credentials in Django admin
- Test API connectivity manually using integration tests

```bash
cd backend
python manage.py test integrations.integration_tests
```
