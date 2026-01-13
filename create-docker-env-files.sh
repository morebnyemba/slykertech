#!/bin/bash
# Create environment files for Docker deployment

set -e

echo "========================================="
echo "Creating Docker Environment Files"
echo "========================================="
echo ""

# Prompt for database password
echo "Enter database password (or press Enter for default 'devpassword'):"
read -r db_password
db_password=${db_password:-devpassword}

# Create root .env for docker-compose
echo "Creating .env for docker-compose..."
cat > .env << EOF
# Database password for PostgreSQL
DB_PASSWORD=${db_password}

# Environment mode
NODE_ENV=production
DEBUG=False
EOF

echo "✅ Docker .env created at .env"

# Create backend directory if it doesn't exist
mkdir -p backend

# Create backend .env
echo "Creating backend/.env..."
cat > backend/.env << EOF
# ===========================================
# SLYKERTECH BACKEND ENVIRONMENT (DOCKER)
# ===========================================

# Django Settings
SECRET_KEY=docker-dev-secret-key-change-in-production
DEBUG=False
ALLOWED_HOSTS=localhost,127.0.0.1,backend

# PostgreSQL via DATABASE_URL (Docker)
DATABASE_URL=postgresql://slykertech:${db_password}@db:5432/slykertech

# Redis
CELERY_BROKER_URL=redis://redis:6379/0
CELERY_RESULT_BACKEND=redis://redis:6379/0

# CORS Settings
CORS_ALLOWED_ORIGINS=http://localhost:3000,http://frontend:3000,http://localhost:80

# CSRF Settings
CSRF_TRUSTED_ORIGINS=http://localhost:3000,http://localhost:80

# JWT Settings
JWT_ACCESS_TOKEN_LIFETIME=60
JWT_REFRESH_TOKEN_LIFETIME=1440
EOF

echo "✅ Backend .env created at backend/.env"

# Create frontend .env.local
echo "Creating .env.local..."
cat > .env.local << 'EOF'
# ===========================================
# SLYKERTECH FRONTEND ENVIRONMENT (DOCKER)
# ===========================================

# Backend API URL (use container name for internal communication)
NEXT_PUBLIC_API_URL=http://backend:8000/api

# Site URL
NEXT_PUBLIC_SITE_URL=http://localhost:3000

# Environment Mode
NODE_ENV=production
EOF

echo "✅ Frontend .env.local created at .env.local"

echo ""
echo "========================================="
echo "Docker Environment Files Created! 🎉"
echo "========================================="
echo ""
echo "Next steps:"
echo "1. Review the environment files if needed"
echo "2. Start Docker services: docker-compose up -d"
echo "3. Run migrations: docker-compose exec backend python manage.py migrate"
echo "4. Create superuser: docker-compose exec backend python manage.py createsuperuser"
echo ""
echo "Files created:"
echo "  - .env (Docker Compose configuration)"
echo "  - backend/.env (Django configuration)"
echo "  - .env.local (Next.js configuration)"
echo ""
echo "Database password: ${db_password}"
echo "⚠️  Keep this password safe!"
echo ""
