# Slyker Tech - Fullstack Web Services Platform

This is a fullstack web application for Slyker Tech, combining a modern Next.js frontend with a powerful Django backend to provide comprehensive digital services and client portal functionality.

## 🚀 Features

### Frontend (Next.js)
- Modern, responsive design with Tailwind CSS
- Server-side rendering for optimal SEO
- Dark/light theme support
- Service catalog and portfolio showcase
- Contact forms and client interaction

### Backend (Django REST API)
- RESTful API with JWT authentication
- Client portal for service management
- DNS records management
- cPanel and DirectAdmin integration
- Cloudflare API integration
- Secure credential storage with encryption
- Comprehensive admin interface

## 📁 Project Structure

```
slykertech/
├── src/                      # Next.js frontend
│   ├── app/                  # Next.js app directory
│   ├── components/           # React components
│   └── lib/                  # Utility functions
├── backend/                  # Django backend
│   ├── accounts/             # User authentication
│   ├── clients/              # Client management
│   ├── services/             # Services and subscriptions
│   ├── integrations/         # External API integrations
│   └── config/               # Django settings
├── INTEGRATION_GUIDE.md      # Frontend-backend integration guide
├── DEPLOYMENT_GUIDE.md       # Production deployment guide
└── README.md                 # This file
```

## 🛠️ Getting Started

### Prerequisites
- Node.js 18+ and npm
- Python 3.8+
- PostgreSQL (optional, SQLite is default)

### Frontend Setup

```bash
# Install dependencies
npm install

# Run development server
npm run dev
```

Visit [http://localhost:3000](http://localhost:3000)

### Backend Setup

```bash
# Navigate to backend
cd backend

# Create virtual environment
python3 -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate

# Install dependencies
pip install -r requirements.txt

# Copy environment file
cp .env.example .env

# Run migrations
python manage.py migrate

# Create superuser
python manage.py createsuperuser

# Populate initial services
python manage.py populate_services

# Run development server
python manage.py runserver 8000
```

Visit:
- API: [http://localhost:8000/api](http://localhost:8000/api)
- Admin: [http://localhost:8000/admin](http://localhost:8000/admin)

## 📚 Documentation

- **[Backend README](backend/README.md)** - Detailed API documentation
- **[Integration Guide](INTEGRATION_GUIDE.md)** - Connect frontend to backend
- **[Deployment Guide](DEPLOYMENT_GUIDE.md)** - Production deployment instructions

## 🔑 Key API Endpoints

### Authentication
- `POST /api/token/` - Login and obtain JWT tokens
- `POST /api/token/refresh/` - Refresh access token
- `POST /api/accounts/register/` - Register new user

### Services
- `GET /api/services/services/` - List all services
- `GET /api/services/subscriptions/` - List user subscriptions
- `POST /api/services/dns-records/` - Manage DNS records

### Integrations
- `GET /api/integrations/cpanel/` - Manage cPanel accounts
- `GET /api/integrations/directadmin/` - Manage DirectAdmin accounts
- `POST /api/integrations/credentials/` - Add integration credentials

## 🔐 Security Features

- JWT token-based authentication
- Encrypted credential storage (Fernet encryption)
- CORS protection
- Input validation and sanitization
- HTTPS enforcement in production
- Password hashing with Django's PBKDF2
- Rate limiting ready

## 🧪 Testing

### Frontend
```bash
npm run lint
npm run build
```

### Backend
```bash
cd backend
python manage.py test
python manage.py check
```

## 🚢 Deployment

See [DEPLOYMENT_GUIDE.md](DEPLOYMENT_GUIDE.md) for detailed production deployment instructions.

### Quick Deploy

**Backend (with Gunicorn + Nginx)**
```bash
cd backend
gunicorn config.wsgi:application --bind 0.0.0.0:8000
```

**Frontend (with Vercel)**
- Push to GitHub
- Import to Vercel
- Configure environment variables
- Deploy

## 🤝 Contributing

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/AmazingFeature`)
3. Commit your changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

## 📝 Environment Variables

### Frontend (.env.local)
```
NEXT_PUBLIC_API_URL=http://localhost:8000/api
```

### Backend (.env)
```
SECRET_KEY=your-secret-key
ENCRYPTION_KEY=your-encryption-key
DEBUG=False
ALLOWED_HOSTS=yourdomain.com
CORS_ALLOWED_ORIGINS=https://yourdomain.com
DB_ENGINE=django.db.backends.postgresql
DB_NAME=slykertech
DB_USER=dbuser
DB_PASSWORD=dbpassword
```

## 🏗️ Tech Stack

### Frontend
- Next.js 15.3
- React 19
- TypeScript
- Tailwind CSS
- Radix UI
- Lucide Icons

### Backend
- Django 6.0.1
- Django REST Framework 3.16
- PostgreSQL / SQLite
- JWT Authentication
- Gunicorn (production)

### Integrations
- cPanel UAPI
- DirectAdmin API
- Cloudflare API
- Various hosting platforms

## 📄 License

This project is proprietary software owned by Slyker Tech.

## 📞 Support

For support, email support@slykertech.co.zw or visit our website at [https://slykertech.co.zw](https://slykertech.co.zw)

## 🎯 Future Enhancements

- [ ] Real-time notifications with WebSockets
- [ ] Payment gateway integration
- [ ] Invoice generation and billing
- [ ] Support ticket system
- [ ] Email notifications
- [ ] Advanced analytics dashboard
- [ ] Mobile app (React Native)
- [ ] Multi-language support
- [ ] Automated backups
- [ ] CDN integration

---

Built with ❤️ by Slyker Tech
