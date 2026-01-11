# Frontend Dashboard Integration Guide

This document explains how the frontend dashboards integrate with the Django backend API.

## Architecture Overview

The frontend is built with Next.js 14+ and uses App Router. All backend communication goes through the centralized `api-service.ts` module.

```
Frontend (Next.js)  ←→  API Service  ←→  Backend (Django REST)
```

## Dashboard Pages Created

### 1. Client Dashboard (`/dashboard/client`)
Main dashboard for clients to manage their services, view invoices, track projects, and receive notifications.

**Features:**
- Service subscription overview
- Invoice list and payment
- Project progress tracking
- Notification center
- Quick actions

### 2. Services Management (`/dashboard/client/services`)
View and manage service subscriptions.

### 3. Invoice Management (`/dashboard/client/invoices`)
View invoices and make payments via Paynow.

### 4. Project Tracking (`/dashboard/client/projects`)
Track project progress with milestones and tasks.

## API Integration

### Setup Environment Variables

Create `.env.local` in the root directory:

```env
NEXT_PUBLIC_API_URL=http://localhost:8000/api
```

For production:
```env
NEXT_PUBLIC_API_URL=https://your-domain.com/api
```

### Using the API Service

```typescript
import { apiService } from '@/lib/api-service';

// Authentication
const loginResponse = await apiService.login('user@example.com', 'password');
if (loginResponse.data?.access) {
  apiService.setToken(loginResponse.data.access);
}

// Fetch services
const servicesResponse = await apiService.getServices();
const services = servicesResponse.data;

// Create subscription
const subscriptionData = {
  service: serviceId,
  billing_cycle: 'monthly',
  price: 99.99
};
await apiService.createSubscription(subscriptionData);

// Get invoices
const invoicesResponse = await apiService.getInvoices();
const invoices = invoicesResponse.data;

// Pay with Paynow
const paymentData = {
  email: 'customer@example.com',
  mobile_payment: false
};
await apiService.payInvoiceWithPaynow(invoiceId, paymentData);
```

## Dynamic Services Page

The `/services` page now fetches services dynamically from the backend instead of using static data.

### Migration from Static to Dynamic

**Before (Static):**
```tsx
import servicesData from './servicesData';
// Static data hardcoded
```

**After (Dynamic):**
```tsx
import { apiService } from '@/lib/api-service';

const services = await apiService.getServices();
// Data from backend
```

## Authentication Flow

1. User enters credentials on login page
2. Frontend sends POST to `/api/token/`
3. Backend returns JWT access and refresh tokens
4. Frontend stores token in localStorage
5. All subsequent requests include token in Authorization header
6. Token refresh handled automatically on 401 responses

## Available API Endpoints

### Authentication
- `POST /api/token/` - Login
- `POST /api/token/refresh/` - Refresh token
- `POST /api/accounts/register/` - Register new user

### Services
- `GET /api/services/services/` - List all services
- `GET /api/services/services/{id}/` - Get service details
- `GET /api/services/subscriptions/` - List user subscriptions
- `POST /api/services/subscriptions/` - Create subscription

### Billing
- `GET /api/billing/invoices/` - List invoices
- `GET /api/billing/invoices/{id}/` - Get invoice details
- `POST /api/billing/invoices/{id}/paynow_payment/` - Pay with Paynow
- `POST /api/billing/invoices/{id}/express_checkout/` - Express checkout
- `GET /api/billing/payments/` - List payments

### Projects
- `GET /api/services/projects/` - List projects
- `GET /api/services/projects/{id}/` - Get project details
- `POST /api/services/projects/{id}/update_progress/` - Update progress
- `GET /api/services/milestones/` - List milestones
- `GET /api/services/tasks/` - List tasks

### Notifications
- `GET /api/notifications/notifications/` - List notifications
- `GET /api/notifications/notifications/unread_count/` - Unread count
- `POST /api/notifications/notifications/{id}/mark_read/` - Mark as read
- `POST /api/notifications/notifications/mark_all_read/` - Mark all read

### DNS Management
- `GET /api/services/dns-records/` - List DNS records
- `POST /api/services/dns-records/` - Create DNS record
- `PUT /api/services/dns-records/{id}/` - Update DNS record
- `DELETE /api/services/dns-records/{id}/` - Delete DNS record

### Integrations
- `GET /api/integrations/cpanel/` - List cPanel accounts
- `GET /api/integrations/directadmin/` - List DirectAdmin accounts
- `GET /api/integrations/cloudflare/` - List Cloudflare integrations
- `GET /api/integrations/namecheap/` - List Namecheap domains

## Error Handling

The API service returns a consistent response format:

```typescript
interface ApiResponse<T> {
  data?: T;        // Success data
  error?: string;  // Error message
  status: number;  // HTTP status code
}
```

Example error handling:

```typescript
const response = await apiService.getServices();

if (response.error) {
  console.error('Error:', response.error);
  // Show error message to user
} else {
  // Use response.data
  const services = response.data;
}
```

## Testing Backend Connection

To verify the backend is accessible:

```typescript
// In browser console or React component
import { apiService } from '@/lib/api-service';

const testConnection = async () => {
  const response = await apiService.getServices();
  console.log('Connection test:', response.status === 200 ? 'Success' : 'Failed');
  console.log('Response:', response);
};

testConnection();
```

## Running Both Servers

### Development Mode

Terminal 1 - Backend:
```bash
cd backend
source venv/bin/activate
python manage.py runserver 8000
```

Terminal 2 - Frontend:
```bash
npm run dev
```

Access:
- Frontend: http://localhost:3000
- Backend API: http://localhost:8000/api
- Django Admin: http://localhost:8000/admin

## Production Deployment

1. **Backend**: Deploy Django to your server (see DEPLOYMENT_GUIDE.md)
2. **Frontend**: Build and deploy Next.js
3. **Environment**: Set `NEXT_PUBLIC_API_URL` to production backend URL
4. **CORS**: Configure Django CORS_ALLOWED_ORIGINS to include frontend domain

```python
# backend/config/settings.py
CORS_ALLOWED_ORIGINS = [
    "https://your-frontend-domain.com",
]
```

## Security Considerations

- ✅ JWT tokens for authentication
- ✅ HTTPS in production (enforced)
- ✅ CORS restricted to frontend domain
- ✅ API credentials encrypted in database
- ✅ Input validation on both frontend and backend
- ✅ XSS protection with React's built-in escaping
- ✅ CSRF tokens for state-changing operations

## Next Steps

1. Implement remaining dashboard pages:
   - `/dashboard/client/services` - Full service management
   - `/dashboard/client/invoices` - Invoice list and payment
   - `/dashboard/client/projects` - Project tracking
   - `/dashboard/client/notifications` - Notification center
   - `/dashboard/client/settings` - User settings

2. Add authentication pages:
   - `/auth/login` - Login page
   - `/auth/register` - Registration page
   - `/auth/forgot-password` - Password reset

3. Implement real-time features:
   - WebSocket notifications
   - Live project updates
   - Real-time payment status

4. Add admin dashboard:
   - `/dashboard/admin` - Admin overview
   - Client management
   - Service management
   - System monitoring

## Troubleshooting

### CORS Errors
If you see CORS errors, ensure:
1. Backend CORS_ALLOWED_ORIGINS includes your frontend URL
2. Backend is running on port 8000
3. Frontend is using correct API_URL

### Authentication Issues
If authentication fails:
1. Check JWT token is being sent in headers
2. Verify token hasn't expired
3. Check backend logs for authentication errors

### Connection Refused
If connection is refused:
1. Ensure backend server is running
2. Check firewall settings
3. Verify API URL is correct

### 404 Errors
If endpoints return 404:
1. Verify backend URLs match frontend API calls
2. Check Django URL patterns
3. Ensure apps are registered in INSTALLED_APPS
