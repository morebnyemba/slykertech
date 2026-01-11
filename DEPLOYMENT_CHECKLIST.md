# Deployment Checklist for Slyker Tech

This checklist ensures all critical components are ready for production deployment.

## Pre-Deployment Checklist

### 1. Environment Configuration ✅

#### Backend (.env)
- [ ] `SECRET_KEY` - Generate a strong, unique secret key
- [ ] `DEBUG=False` - **CRITICAL**: Never deploy with DEBUG=True
- [ ] `ALLOWED_HOSTS` - Set to your domain(s)
- [ ] `ENCRYPTION_KEY` - Generate using: `python -c "from cryptography.fernet import Fernet; print(Fernet.generate_key().decode())"`
- [ ] Database credentials configured (PostgreSQL recommended)
- [ ] `CORS_ALLOWED_ORIGINS` - Set to frontend domain(s)
- [ ] JWT token lifetimes configured
- [ ] All API keys configured (Paynow, Namecheap, WhatsApp, etc.)

#### Frontend (.env.local)
- [ ] `NEXT_PUBLIC_API_URL` - Set to production API URL
- [ ] `NEXT_PUBLIC_SITE_URL` - Set to production frontend URL

### 2. Database Setup ✅

- [ ] PostgreSQL database created
- [ ] Database user created with proper permissions
- [ ] Database connection tested
- [ ] Migrations run: `python manage.py migrate`
- [ ] Superuser created: `python manage.py createsuperuser`
- [ ] Initial data populated: `python manage.py populate_services`

### 3. Security Configuration ✅

- [ ] SSL/TLS certificate installed (Let's Encrypt recommended)
- [ ] HTTPS enforced
- [ ] Secure cookies enabled (`SESSION_COOKIE_SECURE=True`, `CSRF_COOKIE_SECURE=True`)
- [ ] Security headers configured
- [ ] Firewall configured (UFW)
- [ ] Fail2ban installed and configured
- [ ] Strong passwords for all accounts
- [ ] API rate limiting configured

### 4. Application Testing ✅

#### Backend
- [ ] Health check endpoint working: `/health/`
- [ ] Readiness check endpoint working: `/ready/`
- [ ] Django admin accessible and working
- [ ] JWT authentication working
- [ ] All API endpoints tested
- [ ] Run: `python manage.py check --deploy`

#### Frontend
- [ ] Build successful: `npm run build`
- [ ] No console errors in production build
- [ ] All pages rendering correctly
- [ ] API integration working
- [ ] Forms submitting properly

### 5. Infrastructure Setup ✅

#### Backend (Django)
- [ ] Gunicorn installed and configured
- [ ] Systemd service created for Gunicorn
- [ ] Static files collected: `python manage.py collectstatic`
- [ ] Media directory configured with proper permissions
- [ ] Nginx reverse proxy configured
- [ ] Log directory created with proper permissions

#### Frontend (Next.js)
- [ ] Build artifacts generated
- [ ] PM2 or similar process manager configured (if self-hosting)
- [ ] OR Vercel deployment configured
- [ ] Nginx configuration for frontend (if self-hosting)

#### Redis & Celery
- [ ] Redis server installed and running
- [ ] Celery worker configured and running
- [ ] Celery beat configured for scheduled tasks
- [ ] Task monitoring setup

### 6. Monitoring & Logging ✅

- [ ] Application logs configured
- [ ] Log rotation configured (logrotate)
- [ ] Error tracking configured (Sentry or similar)
- [ ] Uptime monitoring configured
- [ ] Database backup script configured
- [ ] Backup testing completed

### 7. Performance Optimization ✅

- [ ] Database indexes optimized
- [ ] Static files served via CDN (optional)
- [ ] Image optimization enabled
- [ ] Caching configured (Redis)
- [ ] Database connection pooling configured
- [ ] Gzip compression enabled

### 8. Documentation ✅

- [ ] API documentation accessible
- [ ] Admin user guide created
- [ ] Deployment runbook updated
- [ ] Rollback procedure documented
- [ ] Emergency contacts documented

## Deployment Commands

### Backend Deployment

```bash
# Pull latest code
cd /var/www/slykertech/backend
git pull origin main

# Activate virtual environment
source venv/bin/activate

# Install/update dependencies
pip install -r requirements.txt

# Run migrations
python manage.py migrate

# Collect static files
python manage.py collectstatic --noinput

# Restart services
sudo systemctl restart gunicorn
sudo systemctl restart celery
sudo systemctl restart celery-beat
```

### Frontend Deployment

```bash
# Pull latest code
cd /var/www/slykertech
git pull origin main

# Install dependencies
npm install

# Build
npm run build

# Restart service
pm2 restart slykertech-frontend
# OR if using Vercel, deployment is automatic
```

## Post-Deployment Verification

- [ ] Health check returns 200: `curl https://api.slykertech.co.zw/health/`
- [ ] Readiness check returns 200: `curl https://api.slykertech.co.zw/ready/`
- [ ] Frontend loads: `curl https://slykertech.co.zw`
- [ ] Admin panel accessible
- [ ] Test user registration
- [ ] Test user login
- [ ] Test creating a service subscription
- [ ] Test payment flow
- [ ] Check logs for errors
- [ ] Monitor resource usage (CPU, memory, disk)

## Rollback Procedure

If issues are detected:

```bash
# Backend rollback
cd /var/www/slykertech/backend
git checkout <previous-commit-hash>
python manage.py migrate
sudo systemctl restart gunicorn

# Frontend rollback
cd /var/www/slykertech
git checkout <previous-commit-hash>
npm install
npm run build
pm2 restart slykertech-frontend
```

## Emergency Contacts

- **Technical Lead**: [Your Name]
- **DevOps**: [Contact]
- **On-Call Support**: [Contact]

## Production URLs

- **Frontend**: https://slykertech.co.zw
- **Backend API**: https://api.slykertech.co.zw
- **Admin Panel**: https://api.slykertech.co.zw/admin
- **Health Check**: https://api.slykertech.co.zw/health/
- **Readiness Check**: https://api.slykertech.co.zw/ready/

## Notes

- Always test in staging environment before production
- Schedule deployments during low-traffic periods
- Keep stakeholders informed of deployment schedule
- Monitor application for at least 1 hour after deployment
- Have rollback plan ready before deploying

---

**Last Updated**: [Date]
**Reviewed By**: [Name]
