# Environment Files Setup Guide

This guide provides step-by-step instructions for creating and configuring environment files for the Slykertech application.

## Quick Start Commands

### 1. Create Backend Environment File

```bash
# Navigate to backend directory
cd backend

# Create .env file from template
cp .env.example .env

# Edit the file with your settings
nano .env
# or
vim .env
```

### 2. Create Frontend Environment File

```bash
# Navigate to root directory (if in backend)
cd ..

# Create .env file from template
cp .env.example .env.local

# Edit the file with your settings
nano .env.local
# or
vim .env.local
```

### 3. Create Docker Environment File (Optional)

```bash
# In root directory, create .env for docker-compose
cat > .env << 'EOF'
# Database password for PostgreSQL
DB_PASSWORD=your_secure_password_here

# Environment mode
NODE_ENV=production
DEBUG=False
EOF
```

## Configuration Options

### Backend Environment Variables

#### For Development (SQLite)

```bash
# backend/.env
SECRET_KEY=your-secret-key-change-in-production
DEBUG=True
ALLOWED_HOSTS=localhost,127.0.0.1

# SQLite Database (Default)
DB_ENGINE=django.db.backends.sqlite3
DB_NAME=db.sqlite3

# CORS (for local development)
CORS_ALLOWED_ORIGINS=http://localhost:3000,http://127.0.0.1:3000

# CSRF
CSRF_TRUSTED_ORIGINS=http://localhost:3000,http://127.0.0.1:3000

# JWT
JWT_ACCESS_TOKEN_LIFETIME=60
JWT_REFRESH_TOKEN_LIFETIME=1440
```

#### For Docker (PostgreSQL)

```bash
# backend/.env
SECRET_KEY=your-secret-key-change-in-production
DEBUG=False
ALLOWED_HOSTS=localhost,127.0.0.1,backend

# PostgreSQL via DATABASE_URL (recommended for Docker)
DATABASE_URL=postgresql://slykertech:devpassword@db:5432/slykertech

# Redis
CELERY_BROKER_URL=redis://redis:6379/0
CELERY_RESULT_BACKEND=redis://redis:6379/0

# CORS
CORS_ALLOWED_ORIGINS=http://localhost:3000,http://frontend:3000

# CSRF
CSRF_TRUSTED_ORIGINS=http://localhost:3000
```

#### For Production (PostgreSQL)

```bash
# backend/.env
SECRET_KEY=your-super-secret-production-key-min-50-chars
DEBUG=False
ALLOWED_HOSTS=api.slykertech.co.zw

# PostgreSQL
DATABASE_URL=postgresql://slykertech_user:secure_password@localhost:5432/slykertech

# Or use individual settings:
# DB_ENGINE=django.db.backends.postgresql
# DB_NAME=slykertech
# DB_USER=slykertech_user
# DB_PASSWORD=secure_password
# DB_HOST=localhost
# DB_PORT=5432

# Redis
CELERY_BROKER_URL=redis://localhost:6379/0
CELERY_RESULT_BACKEND=redis://localhost:6379/0

# CORS
CORS_ALLOWED_ORIGINS=https://slykertech.co.zw,https://www.slykertech.co.zw

# CSRF
CSRF_TRUSTED_ORIGINS=https://slykertech.co.zw,https://www.slykertech.co.zw,https://api.slykertech.co.zw

# Encryption Key (generate with: python -c "from cryptography.fernet import Fernet; print(Fernet.generate_key().decode())")
ENCRYPTION_KEY=your-32-character-encryption-key

# Email Settings
EMAIL_HOST=smtp.gmail.com
EMAIL_PORT=587
EMAIL_USE_TLS=True
EMAIL_HOST_USER=your-email@gmail.com
EMAIL_HOST_PASSWORD=your-app-password
DEFAULT_FROM_EMAIL=noreply@slykertech.co.zw
```

### Frontend Environment Variables

#### For Development

```bash
# .env.local (in root directory)
NEXT_PUBLIC_API_URL=http://localhost:8000/api
NEXT_PUBLIC_SITE_URL=http://localhost:3000
NODE_ENV=development
```

#### For Docker

```bash
# .env.local (in root directory)
NEXT_PUBLIC_API_URL=http://backend:8000/api
NEXT_PUBLIC_SITE_URL=http://localhost:3000
NODE_ENV=production
```

#### For Production

```bash
# .env.local or .env.production (in root directory)
NEXT_PUBLIC_API_URL=https://api.slykertech.co.zw/api
NEXT_PUBLIC_SITE_URL=https://slykertech.co.zw
NODE_ENV=production
```

## Complete Setup Script

### All-in-One Setup for Development

```bash
#!/bin/bash

# Create backend .env
cat > backend/.env << 'EOF'
SECRET_KEY=dev-secret-key-change-me-in-production
DEBUG=True
ALLOWED_HOSTS=localhost,127.0.0.1

# SQLite for development
DB_ENGINE=django.db.backends.sqlite3
DB_NAME=db.sqlite3

# CORS
CORS_ALLOWED_ORIGINS=http://localhost:3000,http://127.0.0.1:3000

# CSRF
CSRF_TRUSTED_ORIGINS=http://localhost:3000,http://127.0.0.1:3000

# JWT
JWT_ACCESS_TOKEN_LIFETIME=60
JWT_REFRESH_TOKEN_LIFETIME=1440
EOF

echo "✅ Backend .env created"

# Create frontend .env.local
cat > .env.local << 'EOF'
NEXT_PUBLIC_API_URL=http://localhost:8000/api
NEXT_PUBLIC_SITE_URL=http://localhost:3000
NODE_ENV=development
EOF

echo "✅ Frontend .env.local created"
echo ""
echo "Environment files created successfully!"
echo "You can now run: ./setup.sh"
```

Save this as `create-env-files.sh` and run:

```bash
chmod +x create-env-files.sh
./create-env-files.sh
```

### All-in-One Setup for Docker

```bash
#!/bin/bash

# Create root .env for docker-compose
cat > .env << 'EOF'
DB_PASSWORD=devpassword
NODE_ENV=production
DEBUG=False
EOF

echo "✅ Docker .env created"

# Create backend .env
cat > backend/.env << 'EOF'
SECRET_KEY=docker-dev-secret-key-change-me
DEBUG=False
ALLOWED_HOSTS=localhost,127.0.0.1,backend

# PostgreSQL via DATABASE_URL
DATABASE_URL=postgresql://slykertech:devpassword@db:5432/slykertech

# Redis
CELERY_BROKER_URL=redis://redis:6379/0
CELERY_RESULT_BACKEND=redis://redis:6379/0

# CORS
CORS_ALLOWED_ORIGINS=http://localhost:3000,http://frontend:3000

# CSRF
CSRF_TRUSTED_ORIGINS=http://localhost:3000
EOF

echo "✅ Backend .env created"
echo ""
echo "Environment files created successfully!"
echo "You can now run: docker-compose up -d"
```

Save this as `create-docker-env-files.sh` and run:

```bash
chmod +x create-docker-env-files.sh
./create-docker-env-files.sh
```

## Testing Database Connection

### Test PostgreSQL Connection (Docker)

```bash
# Start only the database
docker-compose up -d db

# Wait a few seconds, then test connection
docker-compose exec db psql -U slykertech -d slykertech -c "SELECT version();"

# You should see PostgreSQL version information
```

### Test Django Database Connection

```bash
# With docker-compose
docker-compose exec backend python manage.py check --database default

# Or without docker (activate virtual environment first)
cd backend
source venv/bin/activate
python manage.py check --database default
```

### Test Next.js Build with API URL

```bash
# Build Next.js to verify API URL is embedded
npm run build

# Check if API URL is properly configured
grep -r "NEXT_PUBLIC_API_URL" .next/

# Start production build
npm start
```

## Troubleshooting

### Backend Cannot Connect to PostgreSQL

**Problem**: Django shows "could not connect to server" error

**Solution**:
```bash
# Check if PostgreSQL is running
docker-compose ps db

# Check database logs
docker-compose logs db

# Verify connection settings in .env
cat backend/.env | grep DATABASE

# Test direct connection
docker-compose exec db psql -U slykertech -d slykertech
```

### Frontend Cannot Reach Backend API

**Problem**: Next.js shows network errors when calling API

**Solution**:
```bash
# Check NEXT_PUBLIC_API_URL in .env.local
cat .env.local | grep NEXT_PUBLIC_API_URL

# For Docker, use container name:
# NEXT_PUBLIC_API_URL=http://backend:8000/api

# For local development, use localhost:
# NEXT_PUBLIC_API_URL=http://localhost:8000/api

# Rebuild Next.js after changing env
npm run build
```

### Environment Variables Not Loading

**Problem**: Settings not being applied

**Solution**:
```bash
# Verify .env file exists
ls -la backend/.env
ls -la .env.local

# Check file permissions
chmod 600 backend/.env
chmod 600 .env.local

# Restart services
docker-compose restart
# or for local development
# Restart both backend and frontend servers
```

## Security Best Practices

### For Production

1. **Generate Strong Secret Key**:
```bash
python -c "from django.core.management.utils import get_random_secret_key; print(get_random_secret_key())"
```

2. **Generate Encryption Key**:
```bash
python -c "from cryptography.fernet import Fernet; print(Fernet.generate_key().decode())"
```

3. **Use Strong Database Password**:
```bash
# Generate random password
openssl rand -base64 32
```

4. **Never Commit .env Files**:
```bash
# Verify .env is in .gitignore
cat .gitignore | grep "\.env"

# Should show:
# .env
# .env.local
# .env.*.local
```

## Environment Variables Reference

### Backend Required Variables

| Variable | Description | Example |
|----------|-------------|---------|
| `SECRET_KEY` | Django secret key | `django-insecure-...` |
| `DEBUG` | Debug mode | `True` or `False` |
| `ALLOWED_HOSTS` | Allowed host names | `localhost,api.example.com` |
| `DATABASE_URL` | Database connection string | `postgresql://user:pass@host:5432/db` |
| `CORS_ALLOWED_ORIGINS` | CORS origins | `https://example.com` |
| `CSRF_TRUSTED_ORIGINS` | CSRF trusted origins | `https://example.com` |

### Frontend Required Variables

| Variable | Description | Example |
|----------|-------------|---------|
| `NEXT_PUBLIC_API_URL` | Backend API URL | `https://api.example.com/api` |
| `NEXT_PUBLIC_SITE_URL` | Frontend site URL | `https://example.com` |
| `NODE_ENV` | Environment mode | `development` or `production` |

## Quick Reference Commands

```bash
# View current environment variables
cat backend/.env
cat .env.local

# Edit environment files
nano backend/.env
nano .env.local

# Validate Django settings
cd backend && python manage.py check

# Test database connection
cd backend && python manage.py dbshell

# Run migrations
cd backend && python manage.py migrate

# Build Next.js with env vars
npm run build

# Start services with docker-compose
docker-compose up -d

# View service logs
docker-compose logs -f backend
docker-compose logs -f frontend

# Restart services
docker-compose restart backend
docker-compose restart frontend
```

## Additional Resources

- [Django Database Documentation](https://docs.djangoproject.com/en/stable/ref/databases/)
- [Next.js Environment Variables](https://nextjs.org/docs/basic-features/environment-variables)
- [Docker Compose Environment Variables](https://docs.docker.com/compose/environment-variables/)
- [PostgreSQL Connection Strings](https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING)

## Support

For issues or questions:
- Review `DEPLOYMENT_GUIDE.md` for production setup
- Check `QUICK_START.md` for development setup
- Review `CONFIGURATION_CHANGES_SUMMARY.md` for configuration details
- Contact: support@slykertech.co.zw
