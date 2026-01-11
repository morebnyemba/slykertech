# API Testing Guide

This guide provides examples for testing the Slyker Tech API using curl, Postman, or any HTTP client.

## Base URL

Development: `http://localhost:8000/api`
Production: `https://api.slykertech.co.zw/api`

## Authentication

Most endpoints require JWT authentication. Include the token in the Authorization header:

```
Authorization: Bearer {access_token}
```

## Test Endpoints

### 1. Health Check (No Auth Required)

```bash
curl http://localhost:8000/api/services/services/
```

### 2. User Registration

```bash
curl -X POST http://localhost:8000/api/accounts/register/ \
  -H "Content-Type: application/json" \
  -d '{
    "email": "client@example.com",
    "password": "SecurePass123!",
    "password2": "SecurePass123!",
    "first_name": "John",
    "last_name": "Doe",
    "phone": "+263771234567",
    "company_name": "Example Corp",
    "user_type": "client"
  }'
```

### 3. Login (Get JWT Tokens)

```bash
curl -X POST http://localhost:8000/api/token/ \
  -H "Content-Type: application/json" \
  -d '{
    "email": "client@example.com",
    "password": "SecurePass123!"
  }'
```

Response:
```json
{
  "access": "eyJ0eXAiOiJKV1QiLCJhbG...",
  "refresh": "eyJ0eXAiOiJKV1QiLCJhbG..."
}
```

Save the `access` token for subsequent requests.

### 4. Get Current User Profile

```bash
curl http://localhost:8000/api/accounts/users/me/ \
  -H "Authorization: Bearer {access_token}"
```

### 5. Refresh Access Token

```bash
curl -X POST http://localhost:8000/api/token/refresh/ \
  -H "Content-Type: application/json" \
  -d '{
    "refresh": "{refresh_token}"
  }'
```

### 6. List All Services

```bash
curl http://localhost:8000/api/services/services/ \
  -H "Authorization: Bearer {access_token}"
```

### 7. Get Service Details

```bash
curl http://localhost:8000/api/services/services/1/ \
  -H "Authorization: Bearer {access_token}"
```

### 8. List User Subscriptions

```bash
curl http://localhost:8000/api/services/subscriptions/ \
  -H "Authorization: Bearer {access_token}"
```

### 9. Create DNS Record

```bash
curl -X POST http://localhost:8000/api/services/dns-records/ \
  -H "Authorization: Bearer {access_token}" \
  -H "Content-Type: application/json" \
  -d '{
    "subscription": 1,
    "domain": "example.com",
    "record_type": "A",
    "name": "www",
    "content": "192.0.2.1",
    "ttl": 3600
  }'
```

### 10. Update DNS Record

```bash
curl -X PUT http://localhost:8000/api/services/dns-records/1/ \
  -H "Authorization: Bearer {access_token}" \
  -H "Content-Type: application/json" \
  -d '{
    "subscription": 1,
    "domain": "example.com",
    "record_type": "A",
    "name": "www",
    "content": "192.0.2.2",
    "ttl": 1800
  }'
```

### 11. Delete DNS Record

```bash
curl -X DELETE http://localhost:8000/api/services/dns-records/1/ \
  -H "Authorization: Bearer {access_token}"
```

### 12. List Integration Credentials

```bash
curl http://localhost:8000/api/integrations/credentials/ \
  -H "Authorization: Bearer {access_token}"
```

### 13. Add Integration Credential

```bash
curl -X POST http://localhost:8000/api/integrations/credentials/ \
  -H "Authorization: Bearer {access_token}" \
  -H "Content-Type: application/json" \
  -d '{
    "client": 1,
    "provider": "cpanel",
    "name": "Main cPanel Server",
    "host": "server.example.com",
    "username": "cpanel_user",
    "api_token": "YOUR_API_TOKEN",
    "port": 2083
  }'
```

### 14. Verify Credentials

```bash
curl -X POST http://localhost:8000/api/integrations/credentials/1/verify/ \
  -H "Authorization: Bearer {access_token}"
```

### 15. List cPanel Accounts

```bash
curl http://localhost:8000/api/integrations/cpanel/ \
  -H "Authorization: Bearer {access_token}"
```

### 16. Sync cPanel Account

```bash
curl -X POST http://localhost:8000/api/integrations/cpanel/1/sync/ \
  -H "Authorization: Bearer {access_token}"
```

### 17. Suspend Subscription

```bash
curl -X POST http://localhost:8000/api/services/subscriptions/1/suspend/ \
  -H "Authorization: Bearer {access_token}"
```

### 18. Activate Subscription

```bash
curl -X POST http://localhost:8000/api/services/subscriptions/1/activate/ \
  -H "Authorization: Bearer {access_token}"
```

### 19. Change Password

```bash
curl -X POST http://localhost:8000/api/accounts/users/change_password/ \
  -H "Authorization: Bearer {access_token}" \
  -H "Content-Type: application/json" \
  -d '{
    "old_password": "OldPass123!",
    "new_password": "NewPass123!",
    "new_password2": "NewPass123!"
  }'
```

### 20. Get Client Profile

```bash
curl http://localhost:8000/api/clients/clients/1/ \
  -H "Authorization: Bearer {access_token}"
```

## Postman Collection

To import into Postman:

1. Create a new collection
2. Add a variable `base_url` with value `http://localhost:8000/api`
3. Add a variable `access_token` for authentication
4. Create requests using the examples above, replacing hardcoded values with `{{base_url}}` and `{{access_token}}`

## Common Response Codes

- `200 OK` - Success
- `201 Created` - Resource created successfully
- `204 No Content` - Success with no response body
- `400 Bad Request` - Invalid request data
- `401 Unauthorized` - Missing or invalid authentication
- `403 Forbidden` - Insufficient permissions
- `404 Not Found` - Resource not found
- `500 Internal Server Error` - Server error

## Error Response Format

```json
{
  "error": "Error message",
  "field_name": ["Field-specific error"]
}
```

## Pagination

List endpoints support pagination:

```bash
curl "http://localhost:8000/api/services/subscriptions/?page=2&page_size=20" \
  -H "Authorization: Bearer {access_token}"
```

Response includes:
```json
{
  "count": 100,
  "next": "http://localhost:8000/api/services/subscriptions/?page=3",
  "previous": "http://localhost:8000/api/services/subscriptions/?page=1",
  "results": [...]
}
```

## Filtering

Some endpoints support filtering:

```bash
# Filter subscriptions by status
curl "http://localhost:8000/api/services/subscriptions/?status=active" \
  -H "Authorization: Bearer {access_token}"

# Filter DNS records by domain
curl "http://localhost:8000/api/services/dns-records/?domain=example.com" \
  -H "Authorization: Bearer {access_token}"
```

## Testing Tips

1. **Use environment variables** for base URL and tokens
2. **Save tokens** after login to reuse in subsequent requests
3. **Test error cases** with invalid data
4. **Check permissions** by testing as different user types
5. **Test pagination** with large datasets
6. **Verify data** after create/update operations
7. **Clean up** test data after testing

## Automated Testing with Python

```python
import requests

BASE_URL = "http://localhost:8000/api"

# Login
response = requests.post(f"{BASE_URL}/token/", json={
    "email": "client@example.com",
    "password": "SecurePass123!"
})
access_token = response.json()["access"]

# Get subscriptions
headers = {"Authorization": f"Bearer {access_token}"}
response = requests.get(f"{BASE_URL}/services/subscriptions/", headers=headers)
subscriptions = response.json()

print(f"Found {len(subscriptions)} subscriptions")
```

## Automated Testing with JavaScript/Node.js

```javascript
const axios = require('axios');

const BASE_URL = 'http://localhost:8000/api';

async function test() {
  // Login
  const loginResponse = await axios.post(`${BASE_URL}/token/`, {
    email: 'client@example.com',
    password: 'SecurePass123!'
  });
  
  const token = loginResponse.data.access;
  
  // Get subscriptions
  const subsResponse = await axios.get(`${BASE_URL}/services/subscriptions/`, {
    headers: { Authorization: `Bearer ${token}` }
  });
  
  console.log(`Found ${subsResponse.data.length} subscriptions`);
}

test();
```

---

For more information, see the [Backend README](backend/README.md) for complete API documentation.
