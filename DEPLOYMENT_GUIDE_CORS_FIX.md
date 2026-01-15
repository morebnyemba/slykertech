# CORS Fix - Quick Deployment Guide

## Pre-Deployment Checklist

### Backend Environment Variables
Ensure these are set in your production `.env` file:

```env
# Core Settings
DEBUG=False
SECRET_KEY=your-production-secret-key

# CORS Configuration (CRITICAL)
CORS_ALLOWED_ORIGINS=https://slykertech.co.zw,https://www.slykertech.co.zw

# CSRF Configuration
CSRF_TRUSTED_ORIGINS=https://slykertech.co.zw,https://www.slykertech.co.zw,https://api.slykertech.co.zw

# Allowed Hosts
ALLOWED_HOSTS=api.slykertech.co.zw,slykertech.co.zw,www.slykertech.co.zw
```

### Frontend Environment Variables
Ensure these are set in your production `.env` file:

```env
# API Configuration
NEXT_PUBLIC_API_URL=https://api.slykertech.co.zw/api
NEXT_PUBLIC_SITE_URL=https://slykertech.co.zw
NEXT_PUBLIC_WS_URL=wss://api.slykertech.co.zw/ws
```

## Deployment Steps

1. **Backend Deployment**
   ```bash
   cd backend
   # Pull latest changes
   git pull origin main
   
   # Verify .env file has correct CORS settings
   grep CORS_ALLOWED_ORIGINS .env
   
   # Restart Django/Gunicorn
   sudo systemctl restart gunicorn
   # OR if using Docker
   docker-compose restart backend
   ```

2. **Frontend Deployment**
   ```bash
   cd frontend
   # Pull latest changes
   git pull origin main
   
   # Rebuild Next.js
   npm run build
   
   # Restart Next.js
   pm2 restart frontend
   # OR if using Docker
   docker-compose restart frontend
   ```

## Post-Deployment Testing

### 1. Test CORS Headers
Open browser console on https://slykertech.co.zw and run:

```javascript
fetch('https://api.slykertech.co.zw/api/token/', {
  method: 'OPTIONS',
  headers: { 'Content-Type': 'application/json' }
})
  .then(r => {
    console.log('CORS Headers:', {
      'Access-Control-Allow-Origin': r.headers.get('Access-Control-Allow-Origin'),
      'Access-Control-Allow-Credentials': r.headers.get('Access-Control-Allow-Credentials'),
      'Access-Control-Allow-Methods': r.headers.get('Access-Control-Allow-Methods')
    });
  });
```

**Expected Output:**
```
CORS Headers: {
  Access-Control-Allow-Origin: "https://slykertech.co.zw",
  Access-Control-Allow-Credentials: "true",
  Access-Control-Allow-Methods: "DELETE, GET, OPTIONS, PATCH, POST, PUT"
}
```

### 2. Test Login Endpoint
Navigate to https://slykertech.co.zw/login and attempt to log in. Check browser console:
- ✅ No CORS errors
- ✅ POST request to /api/token/ succeeds
- ✅ User is authenticated

### 3. Test Cart Endpoint
Navigate to https://slykertech.co.zw and check browser console:
- ✅ No CORS errors for /api/billing/carts/current/
- ✅ Cart data loads successfully

### 4. Check for Errors
Monitor browser console for any of these errors:
- ❌ "blocked by CORS policy"
- ❌ "No 'Access-Control-Allow-Origin' header"
- ❌ "net::ERR_FAILED"

If any of these appear, verify:
1. Environment variables are correctly set
2. Services have been restarted
3. HTTPS is enabled (required for SameSite=None cookies)

## Troubleshooting

### Issue: Still seeing CORS errors
**Check:**
1. Did you restart the backend service?
   ```bash
   sudo systemctl status gunicorn
   # or
   docker-compose ps backend
   ```

2. Are environment variables loaded?
   ```bash
   python manage.py shell
   >>> from django.conf import settings
   >>> print(settings.CORS_ALLOWED_ORIGINS)
   >>> print(settings.DEBUG)
   ```

3. Is HTTPS working?
   ```bash
   curl -I https://api.slykertech.co.zw/health/
   ```

### Issue: Cookies not being set
**Check:**
1. HTTPS is required for SameSite=None cookies
2. SESSION_COOKIE_SECURE and CSRF_COOKIE_SECURE must be True in production
3. Browser developer tools > Application > Cookies

### Issue: 404 on forgot-password
This is a separate issue not related to CORS. Check:
1. Next.js routing configuration
2. Page exists at `src/app/forgot-password/page.tsx`

## Rollback Plan

If issues persist after deployment:

```bash
# Backend
cd backend
git checkout <previous-commit>
sudo systemctl restart gunicorn

# Frontend
cd frontend
git checkout <previous-commit>
npm run build
pm2 restart frontend
```

## Support

For additional help, see:
- Full documentation: `CORS_FIX_DOCUMENTATION.md`
- Django CORS headers docs: https://github.com/adamchainz/django-cors-headers
- MDN CORS guide: https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS
