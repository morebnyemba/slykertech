# Domain Availability Search - Quick Start Guide

## Overview
The Domain Availability Search Service allows users to check domain availability across 57+ TLDs using direct WHOIS server queries.

## Accessing the Feature

### Frontend URL
```
http://localhost:3000/services/domains/check
```

### API Endpoint
```
POST http://localhost:3000/api/domain-search
Content-Type: application/json

{
  "domains": ["example.com", "test.org"]
}
```

## Usage Examples

### 1. Check Single Domain
```bash
curl -X POST http://localhost:3000/api/domain-search \
  -H "Content-Type: application/json" \
  -d '{"domains": ["example.com"]}'
```

### 2. Check Multiple Domains
```bash
curl -X POST http://localhost:3000/api/domain-search \
  -H "Content-Type: application/json" \
  -d '{"domains": ["example.com", "test.org", "mysite.net"]}'
```

### 3. Using the Web Interface
1. Navigate to `/services/domains/check`
2. Enter domain names (one per line or comma-separated)
3. Click "Search Domains"
4. View results with availability status
5. Filter by available/registered
6. Export results to CSV

## Features

✅ 57+ TLDs supported (.com, .net, .org, .io, etc.)
✅ Bulk domain checking (up to 10 per request)
✅ Real-time WHOIS queries
✅ Results filtering and export
✅ Comprehensive error handling
✅ Rate limiting protection

## Supported TLDs

### Popular TLDs
- .com, .net, .org
- .io, .co, .me
- .info, .biz, .tv

### Country TLDs
- .uk, .de, .fr, .it, .nl
- .au, .ca, .jp, .cn, .ru
- .br, .za, .se, .no, .dk

### And 40+ more! See `dist.whois.json` for complete list.

## Configuration

### Environment Variables

**Frontend (.env.local)**
```bash
NEXT_PUBLIC_API_URL=http://localhost:8000
```

**Backend (.env)**
```bash
# No specific variables required
# WHOIS configuration is in dist.whois.json
```

## Development

### Install Dependencies

**Frontend**
```bash
npm install
```

**Backend**
```bash
cd backend
pip install -r requirements.txt
```

### Run Development Servers

**Frontend**
```bash
npm run dev
# Runs on http://localhost:3000
```

**Backend**
```bash
cd backend
python manage.py runserver
# Runs on http://localhost:8000
```

### Test the API

```bash
# Test Next.js API route
curl http://localhost:3000/api/domain-search

# Test Django backend directly
curl http://localhost:8000/api/services/whois/check/
```

## Troubleshooting

### "WHOIS service endpoint not configured"
**Solution**: Ensure Django backend is running on port 8000

### "TLD not supported"
**Solution**: Add TLD to `dist.whois.json` and restart backend

### "Request timeout"
**Solution**: WHOIS servers can be slow. Default timeout is 10 seconds per domain.

## Documentation

For comprehensive documentation, see:
- **[DOMAIN_SEARCH.md](./DOMAIN_SEARCH.md)** - Full technical documentation
- **API Examples** - See documentation for detailed API usage
- **Adding TLDs** - Guide for adding new TLD support

## Architecture

```
User → Next.js Frontend (/services/domains/check)
     → Next.js API (/api/domain-search)
     → Django REST API (/api/services/whois/check/)
     → Python WHOIS Service
     → WHOIS Servers (Port 43 / HTTP)
```

## Rate Limits

- **Max domains per request**: 10
- **Request timeout**: 30 seconds
- **Per-domain timeout**: 10 seconds

## Security

✓ Input sanitization
✓ Domain validation
✓ Rate limiting
✓ Timeout protection
✓ No security vulnerabilities (CodeQL verified)

## Support

For issues or questions:
- Check [DOMAIN_SEARCH.md](./DOMAIN_SEARCH.md) for detailed troubleshooting
- Review API documentation
- Check backend logs for errors

## License

Part of the Slyker Tech platform. See project license.
