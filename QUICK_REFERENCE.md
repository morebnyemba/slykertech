# Quick Reference Card

## Slyker Tech Fullstack Application

### 🚀 Quick Start

```bash
# Clone and setup
git clone https://github.com/morebnyemba/slykertech.git
cd slykertech
./setup.sh

# Or manual setup:
# Backend
cd backend
python3 -m venv venv
source venv/bin/activate
pip install -r requirements.txt
python manage.py migrate
python manage.py createsuperuser
python manage.py populate_services
python manage.py runserver 8000

# Frontend (new terminal)
npm install
npm run dev
```

### 🌐 URLs

| Service | URL | Description |
|---------|-----|-------------|
| Frontend | http://localhost:3000 | Next.js website |
| Backend API | http://localhost:8000/api | REST API |
| Admin Panel | http://localhost:8000/admin | Django admin |

### 🔑 API Quick Reference

#### Authentication
```bash
# Register
POST /api/accounts/register/
{
  "email": "user@example.com",
  "password": "Pass123!",
  "password2": "Pass123!",
  "first_name": "John",
  "last_name": "Doe",
  "user_type": "client"
}

# Login
POST /api/token/
{
  "email": "user@example.com",
  "password": "Pass123!"
}
→ Returns: { "access": "...", "refresh": "..." }

# Use token
GET /api/accounts/users/me/
Authorization: Bearer {access_token}
```

#### Services
```bash
# List services
GET /api/services/services/

# List my subscriptions
GET /api/services/subscriptions/
Authorization: Bearer {token}

# Manage DNS
GET /api/services/dns-records/
POST /api/services/dns-records/
PUT /api/services/dns-records/{id}/
DELETE /api/services/dns-records/{id}/
```

#### Integrations
```bash
# List credentials
GET /api/integrations/credentials/

# cPanel accounts
GET /api/integrations/cpanel/
POST /api/integrations/cpanel/{id}/sync/

# DirectAdmin accounts
GET /api/integrations/directadmin/
POST /api/integrations/directadmin/{id}/sync/
```

### 📁 Project Structure

```
slykertech/
├── src/                    # Next.js frontend
│   ├── app/               # Pages and routes
│   └── components/        # React components
├── backend/               # Django backend
│   ├── accounts/         # Authentication
│   ├── clients/          # Client management
│   ├── services/         # Services & DNS
│   └── integrations/     # External APIs
├── README.md             # Main documentation
├── setup.sh              # Quick setup script
└── *.md                  # Additional guides
```

### 🔧 Common Commands

#### Backend
```bash
# Activate venv
source backend/venv/bin/activate

# Run server
python manage.py runserver 8000

# Make migrations
python manage.py makemigrations
python manage.py migrate

# Create superuser
python manage.py createsuperuser

# Populate services
python manage.py populate_services

# Run tests
python manage.py test

# Django shell
python manage.py shell
```

#### Frontend
```bash
# Development
npm run dev

# Build
npm run build

# Production
npm run start

# Lint
npm run lint
```

### 🔐 Environment Variables

#### Backend (.env)
```env
SECRET_KEY=your-secret-key
ENCRYPTION_KEY=your-encryption-key
DEBUG=True
ALLOWED_HOSTS=localhost,127.0.0.1
CORS_ALLOWED_ORIGINS=http://localhost:3000
```

#### Frontend (.env.local)
```env
NEXT_PUBLIC_API_URL=http://localhost:8000/api
```

### 📚 Documentation

| Guide | Purpose |
|-------|---------|
| [README.md](README.md) | Project overview & quick start |
| [backend/README.md](backend/README.md) | Complete API documentation |
| [INTEGRATION_GUIDE.md](INTEGRATION_GUIDE.md) | Connect frontend to backend |
| [DEPLOYMENT_GUIDE.md](DEPLOYMENT_GUIDE.md) | Production deployment |
| [API_TESTING.md](API_TESTING.md) | API testing examples |
| [CONTRIBUTING.md](CONTRIBUTING.md) | Contribution guidelines |
| [PROJECT_SUMMARY.md](PROJECT_SUMMARY.md) | Implementation summary |

### 🛠️ Tech Stack

**Frontend:**
- Next.js 15.3
- React 19
- TypeScript
- Tailwind CSS

**Backend:**
- Django 6.0.1
- Django REST Framework
- PostgreSQL/SQLite
- JWT Authentication

### 🔍 Troubleshooting

#### Backend won't start
```bash
cd backend
source venv/bin/activate
pip install -r requirements.txt
python manage.py migrate
```

#### Frontend won't start
```bash
rm -rf node_modules package-lock.json
npm install
npm run dev
```

#### Database issues
```bash
cd backend
rm db.sqlite3
python manage.py migrate
python manage.py createsuperuser
python manage.py populate_services
```

#### CORS errors
Check that `CORS_ALLOWED_ORIGINS` in backend/.env includes your frontend URL

### 📞 Support

- 📧 Email: support@slykertech.co.zw
- 🌐 Website: https://slykertech.co.zw
- 💬 GitHub Issues: [Create an issue](https://github.com/morebnyemba/slykertech/issues)

### ✅ Pre-Deployment Checklist

- [ ] Update SECRET_KEY and ENCRYPTION_KEY
- [ ] Set DEBUG=False
- [ ] Configure production database
- [ ] Set ALLOWED_HOSTS
- [ ] Configure CORS_ALLOWED_ORIGINS
- [ ] Enable HTTPS
- [ ] Set up static files serving
- [ ] Configure backup strategy
- [ ] Test all endpoints
- [ ] Review security settings

### 🎯 Quick Links

- Django Admin: `/admin`
- API Root: `/api`
- API Docs: See [backend/README.md](backend/README.md)
- Frontend Docs: See [INTEGRATION_GUIDE.md](INTEGRATION_GUIDE.md)

---

**Version:** 1.0.0
**Last Updated:** January 2026
**Status:** ✅ Production Ready
