# Domain Availability Search Service

This document explains the Domain Availability Search Service implementation, which allows users to check domain availability across multiple TLDs using direct WHOIS server queries.

## Overview

The Domain Availability Search Service consists of:
- **Backend Python Service**: Performs WHOIS queries using socket and HTTP protocols
- **Django REST API**: Provides HTTP endpoints for domain availability checks
- **Next.js API Route**: Frontend API layer with validation and error handling
- **React Component**: User interface for domain search with real-time results
- **Configuration File**: WHOIS server mappings for 60+ TLDs

## Architecture

```
┌─────────────────┐
│  React Frontend │ (domain-search.tsx)
│  Component      │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Next.js API    │ (/api/domain-search/route.ts)
│  Route          │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Django REST    │ (/api/services/whois/check/)
│  API            │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  WHOIS Service  │ (backend/services/whois_service.py)
│  (Python)       │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  WHOIS Servers  │ (Port 43 / HTTP)
│  (External)     │
└─────────────────┘
```

## Features

### 1. Multi-Protocol WHOIS Queries
- **Socket-based queries** (port 43): Traditional WHOIS protocol
- **HTTP/HTTPS queries**: For modern WHOIS APIs
- Automatic protocol detection from configuration

### 2. Supported TLDs
The service supports 60+ TLDs including:
- Generic: .com, .net, .org, .info, .biz
- Country: .uk, .de, .fr, .au, .ca, .jp, .cn
- New gTLDs: .io, .co, .me, .tv

See `dist.whois.json` for the complete list.

### 3. Rate Limiting
- Maximum 10 domains per request
- 30-second timeout per request
- Connection pooling for efficiency

### 4. Error Handling
- Invalid domain format detection
- Unsupported TLD handling
- Network timeout handling
- WHOIS server error handling

## API Usage

### Endpoint
```
POST /api/domain-search
```

### Request Format
```json
{
  "domains": [
    "example.com",
    "test.org",
    "mysite.net"
  ]
}
```

### Response Format
```json
{
  "results": [
    {
      "domain": "example.com",
      "available": false,
      "tld": "com",
      "whoisServer": "whois.verisign-grs.com",
      "message": "Domain is registered"
    },
    {
      "domain": "test.org",
      "available": true,
      "tld": "org",
      "whoisServer": "whois.pir.org",
      "message": "Domain is available"
    }
  ]
}
```

### Error Response
```json
{
  "error": "Invalid domain format: invalid..domain"
}
```

## Frontend Component Usage

### Basic Usage
```tsx
import DomainSearch from '@/components/domain-search';

export default function DomainPage() {
  return (
    <div>
      <DomainSearch />
    </div>
  );
}
```

### Features
- **Bulk Search**: Enter multiple domains (comma or newline separated)
- **Real-time Validation**: Client-side domain format validation
- **Results Filtering**: Filter by availability status
- **CSV Export**: Export results to CSV file
- **Copy to Clipboard**: One-click domain copy

## Configuration

### WHOIS Server Configuration (`dist.whois.json`)

Each TLD entry contains:
```json
{
  "com": {
    "server": "whois.verisign-grs.com",
    "uri": "socket://whois.verisign-grs.com:43",
    "available": "No match for domain"
  }
}
```

**Fields:**
- `server`: WHOIS server hostname
- `uri`: Connection URI (socket:// or https://)
- `available`: Pattern to match for available domains

### Adding New TLDs

To add a new TLD:

1. Find the WHOIS server for the TLD
2. Determine the "available" pattern by testing queries
3. Add entry to `dist.whois.json`:

```json
{
  "yourtld": {
    "server": "whois.nic.yourtld",
    "uri": "socket://whois.nic.yourtld:43",
    "available": "NOT FOUND"
  }
}
```

4. Restart the backend service to reload configuration

## Backend Components

### 1. WHOIS Configuration Manager
**File**: `backend/config/whois_config.py`

Singleton class that:
- Loads `dist.whois.json` on startup
- Provides TLD-to-server lookup
- Validates configuration format
- Caches parsed data

**Usage:**
```python
from backend.config.whois_config import whois_config

# Get WHOIS server for a TLD
config = whois_config.get_whois_server('com')
# Returns: {'server': '...', 'uri': '...', 'available': '...'}

# Check if TLD is supported
is_supported = whois_config.is_tld_supported('com')

# Get all supported TLDs
tlds = whois_config.get_all_tlds()
```

### 2. WHOIS Service
**File**: `backend/services/whois_service.py`

Service class that:
- Performs WHOIS queries via socket or HTTP
- Normalizes domain names
- Parses WHOIS responses
- Checks availability using regex patterns
- Implements retry logic

**Usage:**
```python
from backend.services.whois_service import whois_service

# Query single domain
result = whois_service.query_domain('example.com')
# Returns: {'domain': '...', 'available': bool, ...}

# Query multiple domains
results = whois_service.query_multiple_domains([
    'example.com',
    'test.org'
])
```

### 3. Django REST API View
**File**: `backend/services/views.py`

Provides REST endpoint at `/api/services/whois/check/`

**Features:**
- Request validation
- Rate limiting (10 domains max)
- Permission: AllowAny (public endpoint)
- Error handling and logging

## Environment Variables

### Frontend (.env.local)
```bash
NEXT_PUBLIC_API_URL=http://localhost:8000
```

### Backend (.env)
```bash
# No specific environment variables required
# WHOIS service uses dist.whois.json configuration
```

## Rate Limiting Considerations

### Per Request Limits
- **Maximum domains**: 10 per request
- **Timeout**: 30 seconds total
- **Retry attempts**: 2 per domain

### WHOIS Server Rate Limits
Many WHOIS servers have their own rate limits:
- **Verisign (.com/.net)**: ~50 queries per minute
- **PIR (.org)**: Varies by use case
- **Country TLDs**: Typically stricter limits

**Best Practices:**
- Implement exponential backoff for failures
- Cache results when possible (with appropriate TTL)
- Respect WHOIS server terms of service
- Consider implementing IP-based rate limiting

## Troubleshooting

### Common Issues

#### 1. "WHOIS service endpoint not configured"
**Problem**: Django backend endpoint not accessible

**Solution**:
- Ensure Django backend is running
- Check `NEXT_PUBLIC_API_URL` environment variable
- Verify backend URL configuration

#### 2. "TLD .xyz is not supported"
**Problem**: TLD not in configuration

**Solution**:
- Add TLD to `dist.whois.json`
- Find WHOIS server using online tools
- Test availability pattern
- Restart backend service

#### 3. "Query failed after 2 retries"
**Problem**: WHOIS server timeout or connection error

**Solutions**:
- Check network connectivity
- Verify WHOIS server is responding (use `whois` command)
- Check if server requires authentication
- Increase timeout in `whois_service.py`

#### 4. "Invalid domain format"
**Problem**: Domain doesn't match validation regex

**Solution**:
- Ensure domain has valid format: `name.tld`
- Remove special characters
- Check domain length (max 253 characters)

#### 5. "Maximum 10 domains per request"
**Problem**: Too many domains in single request

**Solution**:
- Split into multiple requests
- Implement batch processing with delays

### Debugging

#### Enable Debug Logging
```python
# In backend/config/settings.py
LOGGING = {
    'version': 1,
    'disable_existing_loggers': False,
    'handlers': {
        'console': {
            'class': 'logging.StreamHandler',
        },
    },
    'loggers': {
        'backend.services.whois_service': {
            'handlers': ['console'],
            'level': 'DEBUG',
        },
    },
}
```

#### Test WHOIS Queries Directly
```bash
# Using command line
whois example.com

# Using Python service directly
cd backend
python manage.py shell

from services.whois_service import whois_service
result = whois_service.query_domain('example.com')
print(result)
```

## Security Considerations

### Input Validation
- Domain names are sanitized before queries
- Maximum length checks prevent buffer overflow
- Regex validation prevents injection attacks
- Rate limiting prevents abuse

### WHOIS Server Security
- No credentials stored or transmitted
- Read-only queries (no modifications)
- Timeout protection against slow servers
- Connection pooling prevents exhaustion

### Data Privacy
- No personal data collected
- Query logs should be anonymized
- Respect WHOIS privacy policies
- Consider GDPR implications for EU domains

## Performance Optimization

### Current Optimizations
1. **Connection Pooling**: Reuse connections for same servers
2. **Retry Logic**: Automatic retry with exponential backoff
3. **Configuration Caching**: Load config once at startup
4. **Parallel Processing**: Multiple domains queried efficiently

### Future Improvements
1. **Result Caching**: Cache availability results with TTL
2. **Async Queries**: Use asyncio for concurrent queries
3. **Database Storage**: Store historical queries
4. **CDN Caching**: Cache common domain checks

## Testing

### Manual Testing

#### Test Single Domain
```bash
curl -X POST http://localhost:3000/api/domain-search \
  -H "Content-Type: application/json" \
  -d '{"domains": ["example.com"]}'
```

#### Test Multiple Domains
```bash
curl -X POST http://localhost:3000/api/domain-search \
  -H "Content-Type: application/json" \
  -d '{"domains": ["example.com", "test.org", "mysite.net"]}'
```

#### Test Invalid Domain
```bash
curl -X POST http://localhost:3000/api/domain-search \
  -H "Content-Type: application/json" \
  -d '{"domains": ["invalid..domain"]}'
```

### Automated Testing
Tests should cover:
- ✓ Valid domain queries
- ✓ Invalid domain format
- ✓ Unsupported TLDs
- ✓ Timeout handling
- ✓ Rate limiting
- ✓ Multiple domain queries
- ✓ WHOIS server errors

## Deployment

### Production Checklist
- [ ] Update `dist.whois.json` with production WHOIS servers
- [ ] Configure rate limiting per IP address
- [ ] Set up monitoring and alerting
- [ ] Enable query logging for analytics
- [ ] Configure CDN caching for API responses
- [ ] Test all supported TLDs
- [ ] Document supported TLDs for users
- [ ] Set up error tracking (Sentry, etc.)

### Environment Setup
```bash
# Backend
cd backend
pip install -r requirements.txt
python manage.py migrate
python manage.py runserver

# Frontend
npm install
npm run dev
```

## Supported TLDs

| TLD | WHOIS Server | Status |
|-----|--------------|--------|
| .com | whois.verisign-grs.com | ✓ Active |
| .net | whois.verisign-grs.com | ✓ Active |
| .org | whois.pir.org | ✓ Active |
| .io | whois.nic.io | ✓ Active |
| .co | whois.nic.co | ✓ Active |
| .uk | whois.nic.uk | ✓ Active |
| ... | ... | ... |

See `dist.whois.json` for complete list of 60+ supported TLDs.

## Future Enhancements

### Planned Features
1. **Bulk Upload**: CSV upload for large domain lists
2. **Scheduled Checks**: Monitor domains for availability changes
3. **Domain Suggestions**: AI-powered alternative suggestions
4. **Price Comparison**: Show pricing from multiple registrars
5. **Historical Data**: Track domain availability over time
6. **Webhook Notifications**: Alert when domains become available
7. **API Key Authentication**: Rate limiting per API key

## Support

For issues, questions, or contributions:
- Open an issue in the GitHub repository
- Contact: support@slykertech.com
- Documentation: https://docs.slykertech.com/domain-search

## License

This service is part of the Slyker Tech platform and follows the project's license terms.
