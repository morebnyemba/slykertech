# Quick Start Guide - Slyker Tech

Get the Slyker Tech application up and running in minutes!

## Prerequisites

- **Python 3.8+** - [Download](https://www.python.org/downloads/)
- **Node.js 18+** - [Download](https://nodejs.org/)
- **PostgreSQL** (optional, SQLite works for development)

## 🚀 Quick Setup (5 minutes)

### Option 1: Automated Setup (Recommended)

```bash
# Clone the repository
git clone https://github.com/morebnyemba/slykertech.git
cd slykertech

# Run the setup script
chmod +x setup.sh
./setup.sh
```

The script will:
- ✅ Check prerequisites
- ✅ Set up backend (Django)
- ✅ Set up frontend (Next.js)
- ✅ Run database migrations
- ✅ Prompt to create superuser

### Option 2: Manual Setup

#### Backend Setup

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

# Create superuser (admin account)
python manage.py createsuperuser

# Populate initial services
python manage.py populate_services

# Start backend server
python manage.py runserver 8000
```

#### Frontend Setup

```bash
# In a new terminal, navigate to project root
cd slykertech

# Install dependencies
npm install

# Start frontend dev server
npm run dev
```

## 🌐 Access the Application

- **Frontend**: http://localhost:3000
- **Backend API**: http://localhost:8000/api
- **Admin Panel**: http://localhost:8000/admin
- **Health Check**: http://localhost:8000/health/

## 🔑 First Steps

1. **Login to Admin Panel**
   - Go to http://localhost:8000/admin
   - Use the superuser credentials you created

2. **Explore the API**
   - Visit http://localhost:8000/api
   - Check available endpoints

3. **Test the Frontend**
   - Browse to http://localhost:3000
   - Explore services, portfolio, and contact pages

## 🐳 Docker Setup (Alternative)

```bash
# Start all services
docker-compose up -d

# Run migrations
docker-compose exec backend python manage.py migrate

# Create superuser
docker-compose exec backend python manage.py createsuperuser

# Access services
Frontend: http://localhost:3000
Backend: http://localhost:8000
```

## 📚 API Endpoints

### Authentication
```bash
# Get JWT token
curl -X POST http://localhost:8000/api/token/ \
  -H "Content-Type: application/json" \
  -d '{"email":"user@example.com","password":"yourpassword"}'

# Use token in requests
curl http://localhost:8000/api/services/subscriptions/ \
  -H "Authorization: Bearer YOUR_ACCESS_TOKEN"
```

### Key Endpoints
- `POST /api/accounts/register/` - Register new user
- `POST /api/token/` - Login (get JWT tokens)
- `GET /api/services/services/` - List all services
- `GET /api/reseller/profiles/` - Reseller profiles (requires auth)
- `GET /api/wallet/wallets/` - Wallet management (requires auth)

## 🔧 Common Issues

### Backend won't start
```bash
# Make sure virtual environment is activated
source backend/venv/bin/activate

# Check if dependencies are installed
pip list | grep Django

# If not, reinstall
pip install -r backend/requirements.txt
```

### Frontend build errors
```bash
# Clear node_modules and reinstall
rm -rf node_modules package-lock.json
npm install
```

### Database migration errors
```bash
cd backend
python manage.py migrate --run-syncdb
```

## 📖 Next Steps

- Read [INTEGRATION_GUIDE.md](INTEGRATION_GUIDE.md) for API integration details
- Read [DEPLOYMENT_GUIDE.md](DEPLOYMENT_GUIDE.md) for production deployment
- Read [DEPLOYMENT_CHECKLIST.md](DEPLOYMENT_CHECKLIST.md) before going live

## 🆘 Need Help?

- Check [README.md](README.md) for detailed documentation
- Review [backend/README.md](backend/README.md) for API details
- Create an issue on GitHub

---

Happy coding! 🎉
