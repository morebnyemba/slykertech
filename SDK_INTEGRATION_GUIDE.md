# SDK Integration Guide

This document explains the official SDK integrations for WhatsApp and Paynow in the Slyker Tech backend.

## WhatsApp Integration (Heyoo SDK)

### Overview
The WhatsApp integration now uses the **Heyoo** library, which is a community-maintained Python wrapper for the WhatsApp Business Cloud API. Heyoo provides a cleaner and more Pythonic interface compared to direct HTTP requests.

### Installation
```bash
pip install heyoo==0.1.2
```

### Configuration

#### Via Database (Recommended)
Configure WhatsApp via Django admin at `/admin/integrations/apiconfiguration/`:

1. **Provider**: whatsapp
2. **Name**: Production WhatsApp API
3. **API URL**: Not required (Heyoo uses default)
4. **Access Token**: Your WhatsApp Business API token
5. **Config Data** (JSON):
   ```json
   {
     "phone_number_id": "your_phone_number_id",
     "business_account_id": "your_business_account_id"
   }
   ```

#### Via Environment Variables (Fallback)
```env
WHATSAPP_PHONE_NUMBER_ID=your_phone_number_id
WHATSAPP_ACCESS_TOKEN=your_access_token
WHATSAPP_BUSINESS_ACCOUNT_ID=your_business_account_id
```

### Usage Examples

#### Send Simple Text Message
```python
from notifications.whatsapp_service import WhatsAppService

whatsapp = WhatsAppService()
result = whatsapp.send_message(
    to="263771234567",
    message="Hello! Your invoice is ready."
)

if result['success']:
    print(f"Message sent! ID: {result['message_id']}")
else:
    print(f"Error: {result['error']}")
```

#### Send Template Message
```python
result = whatsapp.send_template_message(
    to="263771234567",
    template_name="invoice_notification",
    params=["INV-000123", "$150.00", "2024-01-15"]
)
```

#### Send Invoice Notification
```python
result = whatsapp.send_invoice_notification(
    to="263771234567",
    invoice_number="INV-000123",
    amount="150.00",
    due_date="2024-01-15"
)
```

#### Send Payment Confirmation
```python
result = whatsapp.send_payment_confirmation(
    to="263771234567",
    invoice_number="INV-000123",
    amount="150.00"
)
```

### API Methods

The `WhatsAppService` class provides:

- `send_message(to, message, template_name=None, template_params=None)` - Send text or template message
- `send_template_message(to, template_name, params=None)` - Send approved template
- `get_message_status(message_id)` - Get message status (requires webhook setup)
- `send_invoice_notification(to, invoice_number, amount, due_date)` - Pre-formatted invoice message
- `send_payment_confirmation(to, invoice_number, amount)` - Pre-formatted payment confirmation
- `send_service_notification(to, service_name, action, details)` - Pre-formatted service update

### Response Format

All methods return a dictionary with:
```python
{
    "success": True/False,
    "message_id": "wamid.xxx...",  # If successful
    "error": "Error message",      # If failed
    "data": {...}                  # Full API response
}
```

---

## Paynow Integration (Official SDK)

### Overview
The Paynow integration uses the **official Paynow Python SDK** for Zimbabwe's leading payment gateway. It supports web payments, mobile money (EcoCash, OneMoney), and payment status tracking.

### Installation
```bash
pip install paynow==1.0.8
```

### Configuration

#### Via Database (Recommended)
Configure Paynow via Django admin at `/admin/integrations/apiconfiguration/`:

1. **Provider**: paynow
2. **Name**: Production Paynow Gateway
3. **API Key**: Your Paynow integration key (stored encrypted)
4. **Config Data** (JSON):
   ```json
   {
     "integration_id": "your_integration_id",
     "return_url": "https://yourdomain.com/payment/return",
     "result_url": "https://yourdomain.com/payment/result"
   }
   ```

#### Via Environment Variables (Fallback)
```env
PAYNOW_INTEGRATION_ID=your_integration_id
PAYNOW_INTEGRATION_KEY=your_integration_key
PAYNOW_RETURN_URL=https://yourdomain.com/payment/return
PAYNOW_RESULT_URL=https://yourdomain.com/payment/result
```

### Usage Examples

#### Web Payment (Card/Bank)
```python
from billing.paynow_service import PaynowService
from decimal import Decimal

paynow = PaynowService()

result = paynow.create_payment(
    email="customer@example.com",
    amount=Decimal("150.00"),
    reference="INV-000123",
    description="Payment for Invoice INV-000123"
)

if result['success']:
    # Redirect customer to payment page
    redirect_url = result['redirect_url']
    poll_url = result['poll_url']  # For checking status
    print(f"Redirect to: {redirect_url}")
else:
    print(f"Error: {result['error']}")
```

#### Mobile Money Payment (EcoCash/OneMoney)
```python
result = paynow.create_mobile_payment(
    phone="263771234567",
    amount=Decimal("150.00"),
    reference="INV-000123",
    description="Payment for Invoice INV-000123",
    method="ecocash"  # or "onemoney"
)

if result['success']:
    poll_url = result['poll_url']
    instructions = result['instructions']
    print(f"Instructions: {instructions}")
else:
    print(f"Error: {result['error']}")
```

#### Check Payment Status
```python
# Using poll_url from payment creation
status_result = paynow.check_payment_status(poll_url)

if status_result['success']:
    if status_result['paid']:
        print("Payment completed!")
    else:
        print(f"Status: {status_result['status']}")
else:
    print(f"Error: {status_result['error']}")
```

#### Process Invoice Payment
```python
# Easy helper for invoice payments
result = paynow.process_invoice_payment(
    invoice=invoice_object,
    email="customer@example.com",
    mobile_payment=False  # Set True for mobile money
)
```

### API Endpoints

#### Initiate Paynow Payment
```http
POST /api/billing/invoices/{id}/paynow_payment/
Content-Type: application/json

{
  "email": "customer@example.com",
  "mobile_payment": false
}
```

Response:
```json
{
  "message": "Payment initiated successfully",
  "payment_id": 123,
  "redirect_url": "https://www.paynow.co.zw/Payment/...",
  "poll_url": "https://www.paynow.co.zw/Interface/CheckPayment/..."
}
```

#### Initiate Mobile Payment
```http
POST /api/billing/invoices/{id}/paynow_payment/
Content-Type: application/json

{
  "mobile_payment": true,
  "phone": "263771234567",
  "method": "ecocash"
}
```

Response:
```json
{
  "message": "Payment initiated successfully",
  "payment_id": 123,
  "poll_url": "https://www.paynow.co.zw/Interface/CheckPayment/...",
  "instructions": "Dial *151# and follow prompts..."
}
```

#### Check Payment Status
```http
POST /api/billing/payments/{id}/check_paynow_status/
```

Response:
```json
{
  "payment_id": 123,
  "paid": true,
  "status": "Paid",
  "amount": "150.00",
  "reference": "INV-000123"
}
```

### Payment Flow

1. **Customer initiates payment** on your website/app
2. **Backend creates payment** via Paynow SDK
3. **Customer redirected** to Paynow (web) or receives USSD prompt (mobile)
4. **Customer completes payment** on Paynow
5. **Webhook notification** sent to your `result_url` (configure in Paynow dashboard)
6. **Check payment status** via poll URL
7. **Update invoice** status when payment confirmed

### Payment Methods

The system supports:
- **Web Payments**: Visa, Mastercard, bank transfers
- **EcoCash**: Zimbabwe's leading mobile money platform
- **OneMoney**: Alternative mobile money platform
- **Payment Tracking**: Automatic status updates via polling

### API Methods

The `PaynowService` class provides:

- `create_payment(email, amount, reference, description, additional_info)` - Create web payment
- `create_mobile_payment(phone, amount, reference, description, method)` - Create mobile payment
- `create_express_checkout(method, amount, reference, email, description, token, phone, account_number)` - **Express checkout (no redirect)**
- `tokenize_card(card_number, cardholder_name, expiry_month, expiry_year, cvv)` - **Tokenize card for express checkout**
- `check_payment_status(poll_url)` - Check payment status
- `process_invoice_payment(invoice, email, mobile_payment, phone, method)` - Process invoice payment

---

## Express Checkout (NEW)

### Overview
Express Checkout allows customers to complete payments directly within your app without being redirected to Paynow's website. This provides a smoother user experience, especially for card payments.

### Supported Methods
- **Visa/Mastercard (vmc)**: Requires tokenized card
- **EcoCash**: Requires phone number
- **OneMoney**: Requires phone number
- **InnBucks**: Requires account number
- **Zimswitch**: Direct bank transfer

### Card Tokenization

Before using express checkout with cards, you need to tokenize the card:

```python
from billing.paynow_service import PaynowService

paynow = PaynowService()

# Tokenize card
token_result = paynow.tokenize_card(
    card_number="4111111111111111",
    cardholder_name="John Doe",
    expiry_month="12",
    expiry_year="2025",
    cvv="123"
)

if token_result['success']:
    token = token_result['token']
    card_last4 = token_result['card_last4']
    print(f"Card ending in {card_last4} tokenized successfully")
else:
    print(f"Tokenization failed: {token_result['error']}")
```

### Express Checkout - Visa/Mastercard

```python
# Process express checkout with tokenized card
result = paynow.create_express_checkout(
    method='vmc',
    amount=Decimal("150.00"),
    reference="INV-000123",
    email="customer@example.com",
    description="Payment for Invoice INV-000123",
    token=token  # From tokenization step
)

if result['success']:
    poll_url = result['poll_url']
    # Check status immediately or via polling
    status = paynow.check_payment_status(poll_url)
    if status['paid']:
        print("Payment successful!")
else:
    print(f"Error: {result['error']}")
```

### Express Checkout - Mobile Money

```python
# EcoCash express checkout
result = paynow.create_express_checkout(
    method='ecocash',
    amount=Decimal("150.00"),
    reference="INV-000123",
    email="customer@example.com",
    description="Payment for Invoice INV-000123",
    phone="263771234567"
)

# OneMoney express checkout
result = paynow.create_express_checkout(
    method='onemoney',
    amount=Decimal("150.00"),
    reference="INV-000123",
    email="customer@example.com",
    description="Payment for Invoice INV-000123",
    phone="263771234567"
)
```

### Express Checkout - InnBucks

```python
result = paynow.create_express_checkout(
    method='innbucks',
    amount=Decimal("150.00"),
    reference="INV-000123",
    email="customer@example.com",
    description="Payment for Invoice INV-000123",
    account_number="12345"
)
```

### API Endpoints

#### Tokenize Card
```http
POST /api/billing/invoices/tokenize_card/
Content-Type: application/json

{
  "card_number": "4111111111111111",
  "cardholder_name": "John Doe",
  "expiry_month": "12",
  "expiry_year": "2025",
  "cvv": "123"
}
```

Response:
```json
{
  "message": "Card tokenized successfully",
  "token": "TOKEN_STRING",
  "card_last4": "1111",
  "expiry": "12/2025"
}
```

#### Express Checkout (Card)
```http
POST /api/billing/invoices/{id}/express_checkout/
Content-Type: application/json

{
  "method": "vmc",
  "email": "customer@example.com",
  "token": "TOKEN_STRING"
}
```

#### Express Checkout (Mobile)
```http
POST /api/billing/invoices/{id}/express_checkout/
Content-Type: application/json

{
  "method": "ecocash",
  "email": "customer@example.com",
  "phone": "263771234567"
}
```

Response:
```json
{
  "message": "Express checkout initiated successfully",
  "payment_id": 123,
  "poll_url": "https://www.paynow.co.zw/Interface/CheckPayment/...",
  "instructions": "Complete payment on your phone",
  "payment_method": "ecocash"
}
```

### Benefits of Express Checkout

✅ **No Redirect**: Customer stays on your website/app  
✅ **Faster Payment**: Fewer steps to complete payment  
✅ **Better UX**: Seamless checkout experience  
✅ **Mobile Friendly**: Perfect for mobile app integration  
✅ **Secure**: Uses Paynow tokenization for card security  

### Security Notes

- Card tokens are single-use for security
- Never store raw card details
- All card data is encrypted in transit
- Paynow handles PCI compliance
- Tokens expire after use or timeout

---

### Response Format

All methods return a dictionary with:
```python
{
    "success": True/False,
    "redirect_url": "...",  # For web payments
    "poll_url": "...",      # For status checking
    "reference": "INV-xxx",
    "error": "...",         # If failed
    "data": {...}           # Additional data
}
```

---

## Benefits of Using Official SDKs

### WhatsApp (Heyoo)
✅ **Cleaner Code**: Pythonic interface instead of raw HTTP
✅ **Error Handling**: Better exception handling
✅ **Type Safety**: Proper method signatures
✅ **Active Maintenance**: Community-maintained and updated
✅ **Documentation**: Well-documented methods

### Paynow (Official SDK)
✅ **Official Support**: Maintained by Paynow team
✅ **Security**: Built-in security best practices
✅ **Reliability**: Tested and battle-proven
✅ **Features**: Full API coverage including mobile money
✅ **Updates**: Stays current with API changes

---

## Migration Notes

### From Previous Implementation

If upgrading from the direct HTTP implementation:

1. **Update requirements.txt**:
   ```bash
   pip install heyoo==0.1.2 paynow==1.0.8
   ```

2. **Run migrations**:
   ```bash
   python manage.py migrate billing
   python manage.py migrate integrations
   ```

3. **Update API configurations** in database (optional but recommended)

4. **Test integrations** in sandbox mode first

### Backward Compatibility

- Environment variables still work as fallback
- API response format unchanged
- All existing code continues to work
- Database configurations take precedence over env vars

---

## Testing

### WhatsApp Testing
```python
# Test message sending
from notifications.whatsapp_service import WhatsAppService

whatsapp = WhatsAppService()
test_result = whatsapp.send_message(
    to="your_test_number",
    message="Test message from SDK"
)
print(test_result)
```

### Paynow Testing
```python
# Test payment creation (use sandbox credentials)
from billing.paynow_service import PaynowService
from decimal import Decimal

paynow = PaynowService()
test_payment = paynow.create_payment(
    email="test@example.com",
    amount=Decimal("1.00"),
    reference="TEST-001",
    description="Test payment"
)
print(test_payment)
```

---

## Troubleshooting

### WhatsApp Issues

**Error: "Heyoo SDK is not installed"**
```bash
pip install heyoo==0.1.2
```

**Error: "WhatsApp API not configured"**
- Check phone_number_id and access_token in database or env vars
- Verify API configuration is marked as active

### Paynow Issues

**Error: "Paynow SDK is not installed"**
```bash
pip install paynow==1.0.8
```

**Error: "Paynow not configured"**
- Check integration_id and integration_key in database or env vars
- Verify credentials are correct in Paynow dashboard

**Payment not completing**
- Check result_url is publicly accessible
- Verify webhook configuration in Paynow dashboard
- Use poll_url to manually check status

---

## Production Checklist

### WhatsApp
- [ ] Production WhatsApp Business API credentials configured
- [ ] Phone number verified and registered
- [ ] Message templates approved
- [ ] Webhook endpoint configured for status updates
- [ ] Rate limits understood and monitored

### Paynow
- [ ] Production Paynow credentials configured
- [ ] Return URL and Result URL publicly accessible
- [ ] Webhook handler implemented
- [ ] SSL/HTTPS enabled on all endpoints
- [ ] Payment status polling configured
- [ ] Error handling and logging in place

---

## Support

### WhatsApp
- Heyoo Documentation: https://github.com/Neurotech-HQ/heyoo
- WhatsApp Business API: https://developers.facebook.com/docs/whatsapp

### Paynow
- Paynow SDK: https://github.com/paynow/Paynow-Python
- Paynow Documentation: https://developers.paynow.co.zw/

---

## Summary

Both integrations now use official SDKs which provide:
- Better code maintainability
- Improved error handling
- Official support and updates
- Cleaner, more Pythonic interfaces
- Production-ready implementations

The database-first configuration approach allows easy management without code changes or deployments.
