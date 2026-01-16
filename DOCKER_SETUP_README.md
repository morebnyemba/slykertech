# Docker Setup Guide for Slyker Tech

This guide will help you set up and run Slyker Tech using Docker and Docker Compose.

## Prerequisites

- Docker (version 20.10 or higher)
- Docker Compose (version 2.0 or higher)

## Quick Start

The easiest way to get started is to use our automated setup script:

```bash
./docker-setup.sh
```

This script will:
1. Check for required dependencies (Docker, Docker Compose)
2. Create or update the `.env` file with proper configuration
3. Handle database initialization and password setup
4. Start all services (backend, frontend, database, redis)
5. Run database migrations
6. Optionally create a superuser account

## Manual Setup

If you prefer to set things up manually, follow these steps:

### 1. Create Environment File

Create a `.env` file in the root directory:

```bash
cp .env.example .env
```

Edit the `.env` file and set your database password:

```env
DB_PASSWORD=your_secure_password_here
DEBUG=False
```

**Important:** Make sure the `DB_PASSWORD` is consistent across all configurations.

### 2. Start Services

```bash
docker-compose up -d --build
```

### 3. Run Migrations

```bash
docker-compose exec backend python manage.py migrate
```

### 4. Create Superuser

```bash
docker-compose exec backend python manage.py createsuperuser
```

### 5. Populate Initial Data

```bash
docker-compose exec backend python manage.py populate_services
```

## Common Issues and Solutions

### Database Authentication Error

**Error Message:**
```
FATAL: password authentication failed for user "slykertech"
```

**Cause:**
This error occurs when the password in your `.env` file doesn't match the password the PostgreSQL database was initialized with.

**Solution:**

1. **Stop all containers:**
   ```bash
   docker-compose down
   ```

2. **Remove the database volume:**
   ```bash
   docker volume rm slykertech_postgres_data
   ```

3. **Verify your `.env` file has the correct `DB_PASSWORD`:**
   ```bash
   cat .env | grep DB_PASSWORD
   ```

4. **Start services again:**
   ```bash
   docker-compose up -d --build
   ```

5. **Run migrations:**
   ```bash
   docker-compose exec backend python manage.py migrate
   ```

**Alternatively**, use the automated setup script which handles this for you:
```bash
./docker-setup.sh
```

### Missing .env File

If you see errors about missing environment variables, make sure you have created the `.env` file:

```bash
# Check if .env exists
ls -la .env

# If not, create it from the example
cp .env.example .env
# Edit the file and set your configuration
```

### Services Not Starting

Check the logs to diagnose issues:

```bash
# View all logs
docker-compose logs

# View specific service logs
docker-compose logs backend
docker-compose logs db
```

## Environment Variables

The following environment variables can be configured in your `.env` file:

### Required Variables

- `DB_PASSWORD`: PostgreSQL database password (must be consistent)
- `DEBUG`: Set to `False` for production, `True` for development
- `ALLOWED_HOSTS`: Comma-separated list of allowed hosts
- `CORS_ALLOWED_ORIGINS`: Comma-separated list of allowed CORS origins
- `CSRF_TRUSTED_ORIGINS`: Comma-separated list of trusted CSRF origins

### Optional Variables

- `NODE_ENV`: Node.js environment (development/production)
- `NEXT_PUBLIC_API_URL`: Frontend API URL
- `NEXT_PUBLIC_SITE_URL`: Site URL
- `NEXT_PUBLIC_WS_URL`: WebSocket URL

## Useful Commands

### View logs
```bash
# All services
docker-compose logs -f

# Specific service
docker-compose logs -f backend
```

### Restart services
```bash
# All services
docker-compose restart

# Specific service
docker-compose restart backend
```

### Stop services
```bash
docker-compose down
```

### Access Django shell
```bash
docker-compose exec backend python manage.py shell
```

### Access PostgreSQL database
```bash
docker-compose exec db psql -U slykertech -d slykertech
```

### Run Django management commands
```bash
docker-compose exec backend python manage.py <command>
```

## Production Deployment

For production deployment:

1. Set `DEBUG=False` in your `.env` file
2. Use a strong, secure `DB_PASSWORD`
3. Update `ALLOWED_HOSTS` with your domain names
4. Update `CORS_ALLOWED_ORIGINS` and `CSRF_TRUSTED_ORIGINS` with your frontend URLs
5. Configure SSL/TLS certificates (see nginx.conf)
6. Set up proper backup procedures for your database

## Troubleshooting

### Database Connection Timeout

If the backend fails to connect to the database on startup:

1. Check that the database service is healthy:
   ```bash
   docker-compose ps
   ```

2. Verify database credentials:
   ```bash
   docker-compose exec backend python -c "import os; print(os.environ.get('DATABASE_URL'))"
   ```

3. Test database connection:
   ```bash
   docker-compose exec db pg_isready -U slykertech
   ```

### Redis Connection Issues

If you see Redis connection errors:

1. Check Redis service status:
   ```bash
   docker-compose ps redis
   ```

2. Test Redis connection:
   ```bash
   docker-compose exec redis redis-cli ping
   ```

### Frontend Build Errors

If the frontend fails to build:

1. Check Node.js version in Dockerfile.frontend
2. Clear node_modules volume:
   ```bash
   docker-compose down
   docker volume rm slykertech_node_modules
   docker-compose up -d --build
   ```

## Getting Help

If you encounter issues not covered in this guide:

1. Check the main README.md for general project information
2. Review logs for error messages: `docker-compose logs`
3. Consult the Django documentation for backend issues
4. Consult the Next.js documentation for frontend issues

## Security Notes

- Never commit your `.env` file to version control (it's in .gitignore)
- Use strong passwords for production deployments
- Keep your Docker images updated
- Regularly backup your database
- Follow Django security best practices
