#!/bin/bash
# Docker entrypoint script for Slyker Tech backend
# This script runs before the main application starts

set -e

echo "========================================"
echo "Slyker Tech Backend - Starting"
echo "========================================"
echo ""

# Check environment configuration
echo "Validating environment configuration..."
python /app/check_env.py

if [ $? -ne 0 ]; then
    echo ""
    echo "========================================"
    echo "ERROR: Environment validation failed!"
    echo "========================================"
    echo ""
    echo "Please fix the configuration errors above."
    echo ""
    echo "Common fixes:"
    echo "  1. Ensure .env file exists: cp .env.example .env"
    echo "  2. Set DB_PASSWORD in .env file"
    echo "  3. Reset database: ./fix-db-auth.sh"
    echo ""
    exit 1
fi

echo ""
echo "✅ Environment configuration validated"
echo ""

# Wait for database to be ready
echo "Waiting for PostgreSQL to be ready..."
max_attempts=30
attempt=0

while [ $attempt -lt $max_attempts ]; do
    if pg_isready -h "${DB_HOST:-db}" -U "${DB_USER:-slykertech}" > /dev/null 2>&1; then
        echo "✅ PostgreSQL is ready"
        break
    fi
    attempt=$((attempt + 1))
    echo "Waiting for PostgreSQL... (${attempt}/${max_attempts})"
    sleep 2
done

if [ $attempt -eq $max_attempts ]; then
    echo "❌ ERROR: PostgreSQL did not become ready in time"
    echo "Check database logs with: docker-compose logs db"
    exit 1
fi

echo ""
echo "Cleaning up stale database types..."
python /app/cleanup_db_types.py || echo "⚠️  Cleanup script failed or database not ready yet"

echo ""
echo "Running database migrations..."
python manage.py migrate --noinput

if [ $? -eq 0 ]; then
    echo "✅ Migrations completed"
else
    echo "❌ WARNING: Migrations failed or partially completed"
    echo "The application will still start, but database may not be up to date"
    echo ""
    echo "If you see 'duplicate key' or 'pg_type' errors:"
    echo "  1. See MIGRATION_ERROR_FIX.md for automatic fix details"
    echo "  2. Or run: docker-compose exec backend python /app/cleanup_db_types.py"
    echo "  3. Or reset database: ./fix-db-auth.sh"
fi

echo ""
echo "Collecting static files..."
python manage.py collectstatic --noinput || true

echo ""
echo "========================================"
echo "Starting application server..."
echo "========================================"
echo ""

# Execute the main command
exec "$@"
