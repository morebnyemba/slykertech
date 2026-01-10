# Django Backend API Documentation

## Overview

This is the Django backend for Slyker Tech's client portal. It provides a RESTful API for managing clients, services, subscriptions, and integrations with external platforms like cPanel, DirectAdmin, and Cloudflare.

## Setup Instructions

### Prerequisites
- Python 3.8+
- pip
- PostgreSQL (optional, SQLite is default for development)

### Installation

1. Navigate to the backend directory:
```bash
cd backend
```

2. Create a virtual environment (recommended):
```bash
python -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate
```

3. Install dependencies:
```bash
pip install -r requirements.txt
```

4. Create a `.env` file from the example:
```bash
cp .env.example .env
```

5. Edit `.env` and configure your settings (SECRET_KEY, database, etc.)

6. Run migrations:
```bash
python manage.py migrate
```

7. Create a superuser:
```bash
python manage.py createsuperuser
```

8. Run the development server:
```bash
python manage.py runserver 8000
```

The API will be available at `http://localhost:8000`

## Architecture

### Apps

1. **accounts** - User authentication and management
2. **clients** - Client profiles and contacts
3. **services** - Services, subscriptions, and DNS records
4. **integrations** - External API integrations (cPanel, DirectAdmin, Cloudflare)

### Database Models

#### accounts.User
- Custom user model with email-based authentication
- Fields: email, user_type (admin/client/staff), phone, company_name
- Extends Django's AbstractUser

#### clients.Client
- Client profile information
- Links to User model (OneToOne)
- Fields: company details, billing info, address

#### clients.ClientContact
- Additional contacts for clients
- Multiple contacts per client

#### services.Service
- Services offered by the company
- Fields: name, category, description, features, pricing

#### services.ServiceSubscription
- Client subscriptions to services
- Fields: client, service, status, billing_cycle, dates

#### services.DNSRecord
- DNS records for client domains
- Fields: domain, record_type, name, content, ttl

#### integrations.IntegrationCredential
- Encrypted credentials for external services
- Fields: provider, host, username, encrypted passwords/tokens

#### integrations.cPanelAccount
- cPanel account details for clients

#### integrations.DirectAdminAccount
- DirectAdmin account details for clients

## API Endpoints

### Authentication

#### Register a new user
```
POST /api/accounts/register/
Body: {
  "email": "user@example.com",
  "password": "password123",
  "password2": "password123",
  "first_name": "John",
  "last_name": "Doe",
  "user_type": "client"
}
```

#### Get JWT token
```
POST /api/token/
Body: {
  "email": "user@example.com",
  "password": "password123"
}
Response: {
  "access": "eyJ0eXAiOiJKV1QiLCJhbG...",
  "refresh": "eyJ0eXAiOiJKV1QiLCJhbG..."
}
```

#### Refresh JWT token
```
POST /api/token/refresh/
Body: {
  "refresh": "eyJ0eXAiOiJKV1QiLCJhbG..."
}
```

### User Management

#### Get current user profile
```
GET /api/accounts/users/me/
Headers: Authorization: Bearer {access_token}
```

#### Change password
```
POST /api/accounts/users/change_password/
Headers: Authorization: Bearer {access_token}
Body: {
  "old_password": "oldpass123",
  "new_password": "newpass123",
  "new_password2": "newpass123"
}
```

### Clients

#### List all clients (admin only)
```
GET /api/clients/clients/
Headers: Authorization: Bearer {access_token}
```

#### Get client details
```
GET /api/clients/clients/{id}/
Headers: Authorization: Bearer {access_token}
```

#### Update client
```
PUT /api/clients/clients/{id}/
Headers: Authorization: Bearer {access_token}
Body: { client data }
```

### Services

#### List all services
```
GET /api/services/services/
```

#### Get service details
```
GET /api/services/services/{id}/
```

#### List subscriptions
```
GET /api/services/subscriptions/
Headers: Authorization: Bearer {access_token}
```

#### Create subscription
```
POST /api/services/subscriptions/
Headers: Authorization: Bearer {access_token}
Body: {
  "client": 1,
  "service": 1,
  "status": "active",
  "billing_cycle": "monthly",
  "price": "99.99",
  "start_date": "2024-01-01"
}
```

#### Suspend subscription
```
POST /api/services/subscriptions/{id}/suspend/
Headers: Authorization: Bearer {access_token}
```

#### Activate subscription
```
POST /api/services/subscriptions/{id}/activate/
Headers: Authorization: Bearer {access_token}
```

### DNS Records

#### List DNS records
```
GET /api/services/dns-records/
Headers: Authorization: Bearer {access_token}
```

#### Create DNS record
```
POST /api/services/dns-records/
Headers: Authorization: Bearer {access_token}
Body: {
  "subscription": 1,
  "domain": "example.com",
  "record_type": "A",
  "name": "www",
  "content": "192.0.2.1",
  "ttl": 3600
}
```

#### Update DNS record
```
PUT /api/services/dns-records/{id}/
Headers: Authorization: Bearer {access_token}
Body: { dns record data }
```

#### Delete DNS record
```
DELETE /api/services/dns-records/{id}/
Headers: Authorization: Bearer {access_token}
```

### Integrations

#### List integration credentials
```
GET /api/integrations/credentials/
Headers: Authorization: Bearer {access_token}
```

#### Create integration credential
```
POST /api/integrations/credentials/
Headers: Authorization: Bearer {access_token}
Body: {
  "client": 1,
  "provider": "cpanel",
  "name": "Main cPanel",
  "host": "server.example.com",
  "username": "cpanel_user",
  "api_token": "token123"
}
```

#### Verify credentials
```
POST /api/integrations/credentials/{id}/verify/
Headers: Authorization: Bearer {access_token}
```

#### List cPanel accounts
```
GET /api/integrations/cpanel/
Headers: Authorization: Bearer {access_token}
```

#### Sync cPanel account
```
POST /api/integrations/cpanel/{id}/sync/
Headers: Authorization: Bearer {access_token}
```

#### List DirectAdmin accounts
```
GET /api/integrations/directadmin/
Headers: Authorization: Bearer {access_token}
```

#### Sync DirectAdmin account
```
POST /api/integrations/directadmin/{id}/sync/
Headers: Authorization: Bearer {access_token}
```

## Security

- All passwords and API tokens are encrypted using Fernet encryption
- JWT tokens are used for authentication
- CORS is configured to allow requests from the Next.js frontend
- Rate limiting should be implemented for production
- Use HTTPS in production
- Set strong SECRET_KEY in production

## Admin Interface

Access the Django admin at: `http://localhost:8000/admin`

Use the superuser credentials you created during setup.

## Testing

Run tests with:
```bash
python manage.py test
```

## Production Deployment

1. Set `DEBUG=False` in `.env`
2. Configure a production database (PostgreSQL recommended)
3. Set a strong `SECRET_KEY`
4. Configure allowed hosts
5. Set up HTTPS
6. Configure static file serving
7. Use a production WSGI server (Gunicorn, uWSGI)
8. Set up proper logging
9. Configure backup strategies

## API Client Libraries

The backend includes API client wrappers in `integrations/api_clients.py`:

- `cPanelAPIClient` - For interacting with cPanel UAPI
- `DirectAdminAPIClient` - For interacting with DirectAdmin API
- `CloudflareAPIClient` - For managing DNS via Cloudflare

These can be used to implement the sync functionality for external platforms.

## Future Enhancements

1. Implement actual API integration logic in sync methods
2. Add Celery for background tasks (syncing, notifications)
3. Add email notifications
4. Add audit logging
5. Add rate limiting
6. Add API documentation with Swagger/OpenAPI
7. Add WebSocket support for real-time updates
8. Add payment gateway integration
9. Add invoice generation
10. Add support tickets system
