# Slyker Tech Platform Transformation - Implementation Guide

This document describes the major changes made to transform the Slyker Tech platform into a professional service marketplace with advanced features including live chat, polymorphic cart system, and dynamic service pages.

## Table of Contents
1. [Overview](#overview)
2. [Backend Changes](#backend-changes)
3. [Frontend Changes](#frontend-changes)
4. [Setup Instructions](#setup-instructions)
5. [Testing Guide](#testing-guide)
6. [Architecture Decisions](#architecture-decisions)

## Overview

The Slyker Tech platform has been transformed into a comprehensive service marketplace supporting:
- **Multiple Service Types**: Hosting (Shared, VPS, Dedicated), Domain Services (Registration, Transfer), and Development (Web, Mobile, Desktop, Hybrid)
- **Real-time Communication**: WebSocket-based live chat with department routing
- **Polymorphic Shopping Cart**: Single cart supporting mixed service types
- **Enhanced Admin Interface**: Branded admin dashboard with improved UX

## Backend Changes

### 1. Admin Dashboard Enhancement

**Packages Added:**
- `django-jazzmin==3.0.1` - Modern admin interface
- `whitenoise==6.8.2` - Static file serving

**Configuration** (`backend/config/settings.py`):
```python
INSTALLED_APPS = [
    'jazzmin',  # Must be before django.contrib.admin
    # ... other apps
]

MIDDLEWARE = [
    'django.middleware.security.SecurityMiddleware',
    'whitenoise.middleware.WhiteNoiseMiddleware',  # Static files
    # ... other middleware
]

STATICFILES_STORAGE = 'whitenoise.storage.CompressedManifestStaticFilesStorage'
```

**Features:**
- Custom Slyker Tech branding
- Improved navigation with icons
- Dark mode support
- Better mobile responsiveness

### 2. Polymorphic Service Model

**File**: `backend/services/models.py`

**Changes:**
- Added `domain` category to `SERVICE_CATEGORY_CHOICES`
- Added `service_metadata` JSONField for service-specific configuration:
  ```python
  service_metadata = models.JSONField(
      default=dict, 
      blank=True,
      help_text="Service-specific configuration and options"
  )
  ```

**Metadata Structure Examples:**
```python
# Hosting Service
{
    "types": ["shared", "vps", "dedicated"],
    "regions": ["US", "EU", "Asia"],
    "os_options": ["Ubuntu", "CentOS"]
}

# Domain Service  
{
    "actions": ["registration", "transfer"],
    "requires_epp_for_transfer": true,
    "tlds": [".com", ".net"]
}

# Development Service
{
    "types": ["web", "mobile", "desktop", "hybrid"],
    "requires_brief": true
}
```

### 3. Cart System

**File**: `backend/billing/models.py`

**New Models:**

**Cart Model:**
```python
class Cart(models.Model):
    client = ForeignKey('clients.Client', null=True, blank=True)
    session_id = CharField(max_length=255, blank=True, null=True)
    status = CharField(choices=STATUS_CHOICES, default='active')
    created_at = DateTimeField(auto_now_add=True)
    updated_at = DateTimeField(auto_now=True)
```

**CartItem Model:**
```python
class CartItem(models.Model):
    cart = ForeignKey(Cart, on_delete=CASCADE)
    service = ForeignKey('services.Service', on_delete=CASCADE)
    service_metadata = JSONField(default=dict)  # Polymorphic data
    quantity = IntegerField(default=1)
    unit_price = DecimalField(max_digits=10, decimal_places=2)
    billing_cycle = CharField(max_length=20, default='monthly')
```

**EPP Code Validation:**
```python
def clean(self):
    if self.service.category == 'domain':
        if self.service_metadata.get('action') == 'transfer':
            if not self.service_metadata.get('epp_code'):
                raise ValidationError({
                    'service_metadata': 'EPP/Auth code is required for domain transfers'
                })
```

### 4. LiveChat WebSocket Consumer

**File**: `backend/livechat/consumers.py`

**Features:**
- Real-time messaging via WebSocket
- Department-based routing (sales, support, billing, management)
- Staff-only access to Management department
- Typing indicators
- Connection state management

**Routing** (`backend/config/routing.py`):
```python
websocket_urlpatterns = [
    path('ws/chat/<str:room_name>/', LiveChatConsumer.as_asgi()),
]
```

### 5. API Endpoints

**New Endpoints** (`backend/billing/urls.py`):
```
GET    /api/billing/carts/current/          # Get current user/session cart
POST   /api/billing/carts/{id}/add_item/    # Add item to cart
DELETE /api/billing/carts/{id}/remove_item/ # Remove item from cart
POST   /api/billing/carts/{id}/clear/       # Clear all cart items
```

## Frontend Changes

### 1. Cart State Management

**File**: `src/lib/stores/cart-store.ts`

**Zustand Store Features:**
- Persistent cart state in localStorage
- Real-time cart synchronization with backend
- Cart operations: fetch, add, remove, clear
- Item count and total calculation

**Usage Example:**
```typescript
const { cart, addItem, removeItem, getItemCount } = useCartStore();

// Add hosting service
await addItem({
  service: 1,
  service_metadata: {
    type: 'vps',
    region: 'US',
    os: 'Ubuntu 22.04',
    ram: '8GB',
    cpu: '4 cores'
  },
  quantity: 1,
  unit_price: 50,
  billing_cycle: 'monthly'
}, token);
```

### 2. LiveChat Widget

**File**: `src/components/LiveChatWidget.tsx`

**Features:**
- Persistent across page navigation
- WebSocket connection management
- Department selection UI
- Staff-only Management access
- Typing indicators
- Message history

**Integration** (`src/app/layout.tsx`):
```tsx
<ThemeProvider>
  {/* ... page content ... */}
  <LiveChatWidget />  {/* Persistent across all pages */}
</ThemeProvider>
```

### 3. Header Enhancements

**File**: `src/components/Header.tsx`

**Changes:**
- ✅ Removed "Get Started" button
- ✅ Added cart icon with real-time badge count
- ✅ Added categorized Services dropdown
- ✅ Login/Logout toggle based on JWT state

**Cart Badge:**
```tsx
<Link href="/cart" className="relative">
  <FaShoppingCart className="text-xl" />
  {cartItemCount > 0 && (
    <span className="absolute -top-2 -right-2 bg-red-500 text-white text-xs rounded-full w-5 h-5">
      {cartItemCount}
    </span>
  )}
</Link>
```

### 4. Service Pages

#### Hosting Services (`src/app/services/hosting/page.tsx`)

**Plans Offered:**
- **Shared Starter**: $10/mo - Small websites
- **VPS Business**: $50/mo - Growing businesses
- **Dedicated Enterprise**: $200/mo - Maximum performance

**Configuration Options:**
- Region selection (US, EU, Asia)
- OS selection (Ubuntu, CentOS, Debian, Windows Server)
- Billing cycle (Monthly, Quarterly, Annual)

#### Domain Services (`src/app/services/domains/page.tsx`)

**Actions Supported:**
- **Registration**: New domain registration
- **Transfer**: Domain transfer with EPP code

**TLDs Available:**
- .com ($12.99), .net ($13.99), .org ($12.99)
- .co.zw ($15.00), .io ($39.99), .tech ($24.99)

**Transfer Requirements:**
- Domain name validation
- Mandatory EPP/Auth code input
- Transfer eligibility checking

#### Development Services (`src/app/services/development/page.tsx`)

**Service Types:**
- **Web Development**: $2000+ - Custom web applications
- **Mobile Development**: $3000+ - iOS & Android apps
- **Desktop Development**: $2500+ - Cross-platform desktop
- **Hybrid Solutions**: $4500+ - Multi-platform solutions

**Project Brief Requirements:**
- Project name
- Timeline selection
- Detailed requirements description

### 5. Cart & Checkout Pages

#### Cart Page (`src/app/cart/page.tsx`)

**Features:**
- Service-specific metadata display
- Contextual information based on service type
- Item removal functionality
- Clear cart option
- Order summary with totals

**Metadata Display:**
```tsx
{/* Hosting Services */}
<p>Type: {metadata.type}</p>
<p>Region: {metadata.region}</p>
<p>OS: {metadata.os}</p>
<p>RAM: {metadata.ram}</p>
<p>CPU: {metadata.cpu}</p>

{/* Domain Services */}
<p>Action: {metadata.action}</p>
<p>Domain: {metadata.domain_name}</p>
<p>EPP Code: ***{metadata.epp_code.slice(-4)}</p>

{/* Development Services */}
<p>Type: {metadata.type}</p>
<p>Project: {metadata.project_name}</p>
<p>Timeline: {metadata.timeline}</p>
```

#### Checkout Page (`src/app/checkout/page.tsx`)

**Features:**
- Auto-fill for authenticated users
- Contact information form
- Multiple payment methods:
  - Paynow (EcoCash, OneMoney, Visa/Mastercard)
  - Bank Transfer
  - Cryptocurrency
- Order summary
- Secure checkout indicator

### 6. Utility Functions

**File**: `src/lib/utils/user-roles.ts`

**Functions:**
```typescript
// Check if user is staff (admin or staff role)
isUserStaff(user): boolean

// Check if user has admin privileges  
isUserAdmin(user): boolean
```

**Usage:**
```typescript
import { isUserStaff } from '@/lib/utils/user-roles';

const isStaff = isUserStaff(user);
if (!isStaff) {
  // Restrict access to Management department
}
```

## Setup Instructions

### Backend Setup

1. **Install Dependencies:**
```bash
cd backend
pip install -r requirements.txt
```

2. **Run Migrations:**
```bash
python manage.py makemigrations
python manage.py migrate
```

3. **Create Superuser:**
```bash
python manage.py createsuperuser
```

4. **Collect Static Files:**
```bash
python manage.py collectstatic --noinput
```

5. **Start Development Server:**
```bash
# With Daphne (WebSocket support)
daphne config.asgi:application -b 0.0.0.0 -p 8000

# Or with Django dev server (no WebSocket)
python manage.py runserver
```

### Frontend Setup

1. **Install Dependencies:**
```bash
npm install
```

2. **Configure Environment:**
```bash
cp .env.example .env.local
```

Edit `.env.local`:
```env
NEXT_PUBLIC_API_URL=http://localhost:8000/api
NEXT_PUBLIC_WS_URL=ws://localhost:8000/ws
```

3. **Start Development Server:**
```bash
npm run dev
```

4. **Build for Production:**
```bash
npm run build
npm start
```

## Testing Guide

### Test Polymorphic Cart

1. Add a VPS hosting service:
   - Navigate to `/services/hosting`
   - Select "VPS Business"
   - Choose region and OS
   - Add to cart

2. Add a domain transfer:
   - Navigate to `/services/domains`
   - Select "Transfer Domain"
   - Enter domain name and EPP code
   - Add to cart

3. Add a web development project:
   - Navigate to `/services/development`
   - Select "Web Development"
   - Fill project brief
   - Add to cart

4. Verify cart contents:
   - Navigate to `/cart`
   - Confirm all three items are present with correct metadata

### Test LiveChat Persistence

1. Open LiveChat widget
2. Select a department (e.g., "Sales")
3. Send a test message
4. Navigate to different pages
5. Verify:
   - Widget remains open
   - Conversation persists
   - Messages are retained

### Test Management Department Access

1. **As Non-Staff User:**
   - Open LiveChat
   - Verify "Management" department is NOT visible

2. **As Staff User:**
   - Login as staff/admin
   - Open LiveChat
   - Verify "Management" department IS visible
   - Can access Management chat

### Test Checkout Contextual Fields

1. Add different service types to cart
2. Navigate to `/checkout`
3. Verify correct fields displayed for each item type
4. Complete checkout flow

## Architecture Decisions

### Why Polymorphic Cart?

**Decision**: Use JSONField for service metadata instead of separate tables per service type.

**Rationale:**
- Flexibility: Easy to add new service types without schema changes
- Simplicity: Single cart/item model handles all services
- Performance: Fewer joins, simpler queries
- Scalability: Easy to extend with new service attributes

**Trade-offs:**
- Less type safety (mitigated with validation)
- No foreign key constraints on metadata (mitigated with clean() methods)

### Why WebSocket for LiveChat?

**Decision**: Use Django Channels with WebSocket instead of polling.

**Rationale:**
- Real-time: Instant message delivery
- Efficiency: No repeated HTTP requests
- Scalability: Better server resource utilization
- Features: Typing indicators, presence detection

**Implementation:**
- Redis as channel layer backend
- Department-based routing
- Automatic reconnection handling

### Why Zustand for Cart State?

**Decision**: Use Zustand instead of Context API or Redux.

**Rationale:**
- Simplicity: Less boilerplate than Redux
- Performance: Optimized re-renders
- Persistence: Built-in localStorage support
- TypeScript: Excellent type inference

## Environment Variables

### Backend (.env)
```env
SECRET_KEY=your-secret-key
DEBUG=False
ALLOWED_HOSTS=localhost,127.0.0.1,api.slykertech.co.zw
DATABASE_URL=postgresql://user:pass@localhost:5432/slykertech
REDIS_HOST=redis
REDIS_PORT=6379
CORS_ALLOWED_ORIGINS=http://localhost:3000,https://slykertech.co.zw
```

### Frontend (.env.local)
```env
NEXT_PUBLIC_API_URL=https://api.slykertech.co.zw/api
NEXT_PUBLIC_WS_URL=wss://api.slykertech.co.zw/ws
```

## Troubleshooting

### WebSocket Connection Fails

**Symptom**: LiveChat doesn't connect

**Solutions:**
1. Verify WebSocket URL in `.env.local`
2. Check Daphne is running (not Django dev server)
3. Verify Redis is running
4. Check CORS configuration allows WebSocket

### Cart Not Persisting

**Symptom**: Cart items disappear on refresh

**Solutions:**
1. Check localStorage is enabled
2. Verify cart store persistence configuration
3. Check browser console for errors
4. Clear localStorage and retry

### Admin Dashboard Not Themed

**Symptom**: Admin looks like default Django

**Solutions:**
1. Verify `jazzmin` is before `django.contrib.admin` in INSTALLED_APPS
2. Run `collectstatic` command
3. Check whitenoise middleware is configured
4. Clear browser cache

## Future Enhancements

Potential improvements for future iterations:

1. **Payment Integration**: Real payment processor integration (Stripe, Paynow API)
2. **Email Notifications**: Send order confirmations, chat transcripts
3. **Advanced Search**: Full-text search for services
4. **Service Recommendations**: AI-based service suggestions
5. **Analytics Dashboard**: User behavior analytics
6. **Mobile App**: React Native mobile application
7. **API Rate Limiting**: Throttling for security
8. **Automated Testing**: Comprehensive test coverage

## Support

For questions or issues:
- Email: support@slykertech.co.zw
- Phone: +263 78 721 1325
- LiveChat: Available on the website

---

**Last Updated**: January 2026  
**Version**: 1.0.0  
**Maintainer**: Slyker Tech Development Team
