#!/bin/bash
# Create environment files for local development

set -e

echo "========================================="
echo "Creating Environment Files"
echo "========================================="
echo ""

# Create backend .env
echo "Creating backend/.env..."
cat > backend/.env << 'EOF'
# ===========================================
# SLYKERTECH BACKEND ENVIRONMENT (DEVELOPMENT)
# ===========================================

# Django Settings
SECRET_KEY=dev-secret-key-change-me-in-production-min-50-characters
DEBUG=True
ALLOWED_HOSTS=localhost,127.0.0.1

# SQLite Database (Development)
DB_ENGINE=django.db.backends.sqlite3
DB_NAME=db.sqlite3

# CORS Settings
CORS_ALLOWED_ORIGINS=http://localhost:3000,http://127.0.0.1:3000

# CSRF Settings
CSRF_TRUSTED_ORIGINS=http://localhost:3000,http://127.0.0.1:3000

# JWT Settings
JWT_ACCESS_TOKEN_LIFETIME=60
JWT_REFRESH_TOKEN_LIFETIME=1440

# Celery & Redis (Optional - comment out if not using)
# CELERY_BROKER_URL=redis://localhost:6379/0
# CELERY_RESULT_BACKEND=redis://localhost:6379/0
EOF

echo "✅ Backend .env created at backend/.env"

# Create frontend .env.local
echo "Creating .env.local..."
cat > .env.local << 'EOF'
# ===========================================
# SLYKERTECH FRONTEND ENVIRONMENT (DEVELOPMENT)
# ===========================================

# Backend API URL
NEXT_PUBLIC_API_URL=http://localhost:8000/api

# Site URL
NEXT_PUBLIC_SITE_URL=http://localhost:3000

# Environment Mode
NODE_ENV=development
EOF

echo "✅ Frontend .env.local created at .env.local"

echo ""
echo "========================================="
echo "Environment Files Created! 🎉"
echo "========================================="
echo ""
echo "Next steps:"
echo "1. Review and customize the environment files if needed"
echo "2. Run the setup script: ./setup.sh"
echo "3. Start the development servers"
echo ""
echo "Files created:"
echo "  - backend/.env (Django configuration)"
echo "  - .env.local (Next.js configuration)"
echo ""
