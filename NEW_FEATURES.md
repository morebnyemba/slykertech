# New Features Update

## Recent Additions to Slyker Tech Backend

This document describes the latest features added to the Django backend.

---

## 🆕 Billing System

A complete invoicing and payment management system.

### Features
- **Invoice Generation**: Create invoices with line items, tax calculations, and discounts
- **Automatic Invoice Numbering**: Sequential invoice numbers (INV-000001, etc.)
- **Payment Tracking**: Record and track payments across multiple methods
- **Billing Profiles**: Store client billing preferences and payment information
- **Multiple Payment Methods**: Cash, Bank Transfer, Credit Card, PayPal, Stripe, Mobile Money

### Models
- `Invoice`: Invoice with status tracking (draft, sent, paid, overdue, cancelled)
- `InvoiceItem`: Line items for services/subscriptions
- `Payment`: Payment records with transaction tracking
- `BillingProfile`: Client billing preferences and payment settings

### API Endpoints

```bash
# List all invoices
GET /api/billing/invoices/

# Create invoice
POST /api/billing/invoices/
{
  "client": 1,
  "issue_date": "2024-01-15",
  "due_date": "2024-02-15",
  "tax_rate": 15.00,
  "discount_amount": 0.00,
  "notes": "Thank you for your business"
}

# Send invoice to client
POST /api/billing/invoices/{id}/send/

# Mark invoice as paid
POST /api/billing/invoices/{id}/mark_paid/

# Record payment
POST /api/billing/payments/
{
  "invoice": 1,
  "payment_method": "bank_transfer",
  "amount": 199.99,
  "transaction_id": "TXN123456"
}
```

---

## 📱 WhatsApp Integration

Send notifications via WhatsApp Business API.

### Features
- **WhatsApp Business API**: Direct integration with Meta's Business API
- **Message Templates**: Pre-built templates for common notifications
- **Status Tracking**: Track message status (sent, delivered, read)
- **Automatic Notifications**: Invoice, payment, and service notifications

### Configuration

Set these in your `.env` file:

```env
WHATSAPP_API_URL=https://graph.facebook.com/v18.0
WHATSAPP_PHONE_NUMBER_ID=your_phone_number_id
WHATSAPP_ACCESS_TOKEN=your_access_token
WHATSAPP_BUSINESS_ACCOUNT_ID=your_business_account_id
```

### Usage Examples

**Python API:**
```python
from notifications.whatsapp_service import WhatsAppService

whatsapp = WhatsAppService()

# Send invoice notification
whatsapp.send_invoice_notification(
    to="263771234567",
    invoice_number="INV-000001",
    amount="199.99",
    due_date="2024-02-15"
)

# Send payment confirmation
whatsapp.send_payment_confirmation(
    to="263771234567",
    invoice_number="INV-000001",
    amount="199.99"
)

# Send service notification
whatsapp.send_service_notification(
    to="263771234567",
    service_name="Web Hosting",
    action="activated",
    details="Your service is now active"
)
```

**REST API:**
```bash
# Send WhatsApp message
POST /api/notifications/whatsapp/send_message/
{
  "recipient": "263771234567",
  "message": "Hello from Slyker Tech!"
}
```

---

## 🌐 Namecheap API Integration

Complete domain management through Namecheap API.

### Features
- **Domain Registration**: Register new domains
- **Domain Renewal**: Renew existing domains
- **DNS Management**: Manage DNS host records
- **Nameserver Configuration**: Set custom nameservers
- **Domain Lock**: Enable/disable registrar lock
- **WhoisGuard**: Manage WhoisGuard privacy protection

### Configuration

```env
NAMECHEAP_API_USER=your_api_username
NAMECHEAP_API_KEY=your_api_key
NAMECHEAP_USERNAME=your_account_username
NAMECHEAP_CLIENT_IP=your_whitelisted_ip
```

### Usage Examples

```python
from integrations.api_clients import NamecheapAPIClient

client = NamecheapAPIClient(
    api_user="apiuser",
    api_key="apikey",
    username="username",
    client_ip="1.2.3.4"
)

# Check domain availability
result = client.check_domain("example.com")

# Register domain
result = client.register_domain(
    domain="example.com",
    years=1,
    # Add contact details...
)

# Get DNS records
result = client.get_dns_hosts("example.com")

# Set DNS records
hosts = [
    {
        'HostName': '@',
        'RecordType': 'A',
        'Address': '1.2.3.4',
        'TTL': 1800
    },
    {
        'HostName': 'www',
        'RecordType': 'CNAME',
        'Address': 'example.com',
        'TTL': 1800
    }
]
result = client.set_dns_hosts("example.com", hosts)

# Set nameservers
result = client.set_nameservers(
    "example.com",
    ["ns1.example.com", "ns2.example.com"]
)
```

---

## 🔔 Notification System

Multi-channel notification system with user preferences.

### Features
- **Multiple Channels**: Email, WhatsApp, SMS, In-App notifications
- **Notification Templates**: Reusable templates with variable substitution
- **User Preferences**: Per-user control of notification channels and categories
- **Status Tracking**: Track notification lifecycle (pending, sent, delivered, read)
- **Categories**: Invoice, Payment, Service, Account, System, Marketing

### Models
- `Notification`: General notification model
- `WhatsAppMessage`: WhatsApp-specific tracking
- `NotificationTemplate`: Reusable message templates
- `NotificationPreference`: User notification preferences

### API Endpoints

```bash
# List notifications
GET /api/notifications/notifications/

# Mark notification as read
POST /api/notifications/notifications/{id}/mark_read/

# Mark all as read
POST /api/notifications/notifications/mark_all_read/

# Get unread count
GET /api/notifications/notifications/unread_count/

# Get my preferences
GET /api/notifications/preferences/my_preferences/

# Update preferences
PUT /api/notifications/preferences/{id}/
{
  "email_enabled": true,
  "whatsapp_enabled": true,
  "sms_enabled": false,
  "invoice_notifications": true,
  "payment_notifications": true,
  "marketing_notifications": false
}
```

---

## 📞 Mobile Number Field

Mobile number is now required during registration for WhatsApp notifications.

### User Model Updates

New fields added to `User` model:
- `mobile_number`: Required field for WhatsApp notifications
- `email_notifications`: Enable/disable email notifications
- `whatsapp_notifications`: Enable/disable WhatsApp notifications
- `sms_notifications`: Enable/disable SMS notifications

### Registration Example

```bash
POST /api/accounts/register/
{
  "email": "client@example.com",
  "password": "SecurePass123!",
  "password2": "SecurePass123!",
  "first_name": "John",
  "last_name": "Doe",
  "mobile_number": "263771234567",  # REQUIRED
  "phone": "+263 77 123 4567",
  "company_name": "Example Corp",
  "user_type": "client",
  "email_notifications": true,
  "whatsapp_notifications": true,
  "sms_notifications": false
}
```

---

## 🎯 Integration Example: Complete Flow

Here's how all new features work together:

```python
# 1. Create invoice for client
invoice = Invoice.objects.create(
    client=client,
    issue_date=today,
    due_date=today + timedelta(days=30),
    tax_rate=15.00
)

# Add invoice items
InvoiceItem.objects.create(
    invoice=invoice,
    description="Web Hosting - Monthly",
    quantity=1,
    unit_price=49.99
)

# 2. Send invoice via email and WhatsApp
invoice.status = 'sent'
invoice.save()

# Create notification
notification = Notification.objects.create(
    user=client.user,
    notification_type='email',
    category='invoice',
    title=f'New Invoice #{invoice.invoice_number}',
    message=f'You have a new invoice for ${invoice.total}'
)

# Send WhatsApp notification if enabled
if client.user.whatsapp_notifications:
    whatsapp = WhatsAppService()
    whatsapp.send_invoice_notification(
        to=client.user.mobile_number,
        invoice_number=invoice.invoice_number,
        amount=str(invoice.total),
        due_date=invoice.due_date.strftime('%Y-%m-%d')
    )

# 3. Record payment
payment = Payment.objects.create(
    invoice=invoice,
    payment_method='bank_transfer',
    amount=invoice.total,
    status='completed',
    transaction_id='TXN123456'
)

# 4. Send payment confirmation
if client.user.whatsapp_notifications:
    whatsapp.send_payment_confirmation(
        to=client.user.mobile_number,
        invoice_number=invoice.invoice_number,
        amount=str(payment.amount)
    )
```

---

## 📋 Migration Guide

### For Existing Installations

1. Update code:
```bash
git pull origin main
```

2. Install new dependencies (if needed):
```bash
pip install -r requirements.txt
```

3. Run migrations:
```bash
python manage.py migrate
```

4. Update `.env` file with new settings:
```bash
# WhatsApp
WHATSAPP_API_URL=https://graph.facebook.com/v18.0
WHATSAPP_PHONE_NUMBER_ID=
WHATSAPP_ACCESS_TOKEN=

# Namecheap
NAMECHEAP_API_USER=
NAMECHEAP_API_KEY=
NAMECHEAP_USERNAME=
NAMECHEAP_CLIENT_IP=
```

5. Restart your server

---

## 🔧 Admin Interface

All new models are available in Django admin:

- `/admin/billing/` - Invoices, Payments, Billing Profiles
- `/admin/notifications/` - Notifications, Templates, Preferences, WhatsApp Messages

---

## 📚 Additional Resources

- [Backend README](README.md) - Complete API documentation
- [Integration Guide](../INTEGRATION_GUIDE.md) - Frontend integration
- [Deployment Guide](../DEPLOYMENT_GUIDE.md) - Production deployment
- [API Testing Guide](../API_TESTING.md) - Testing examples

---

## 🆘 Support

For questions or issues with new features:
- Email: support@slykertech.co.zw
- GitHub Issues: https://github.com/morebnyemba/slykertech/issues
