#!/bin/bash
# Docker Setup Script for Slyker Tech
# This script ensures proper environment configuration and database initialization

set -e

echo "========================================="
echo "Slyker Tech - Docker Setup"
echo "========================================="
echo ""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration
POSTGRES_VOLUME="slykertech_postgres_data"
MAX_WAIT_TIME=60
CHECK_INTERVAL=3

# Check if Docker is installed
if ! command -v docker &> /dev/null; then
    echo -e "${RED}❌ Docker is not installed. Please install Docker first.${NC}"
    exit 1
fi

if ! command -v docker-compose &> /dev/null && ! docker compose version &> /dev/null; then
    echo -e "${RED}❌ Docker Compose is not installed. Please install Docker Compose first.${NC}"
    exit 1
fi

echo -e "${GREEN}✅ Docker and Docker Compose are installed${NC}"
echo ""

# Check if .env file exists
if [ -f ".env" ]; then
    echo -e "${YELLOW}⚠️  .env file already exists${NC}"
    echo "Do you want to:"
    echo "  1) Keep existing .env file"
    echo "  2) Recreate .env file (will backup existing)"
    echo "  3) Exit"
    read -p "Enter choice (1-3): " choice
    
    case $choice in
        1)
            echo "Keeping existing .env file"
            ;;
        2)
            echo "Backing up existing .env to .env.backup"
            cp .env .env.backup
            rm .env
            ;;
        3)
            echo "Exiting..."
            exit 0
            ;;
        *)
            echo "Invalid choice. Exiting..."
            exit 1
            ;;
    esac
fi

# Create .env file if it doesn't exist
if [ ! -f ".env" ]; then
    echo "Creating .env file from template..."
    
    # Prompt for database password
    echo ""
    echo "Enter database password (minimum 8 characters, required for security):"
    read -s -r db_password
    echo ""
    
    # Validate password
    if [ -z "$db_password" ]; then
        echo -e "${RED}❌ Password cannot be empty${NC}"
        exit 1
    fi
    
    if [ ${#db_password} -lt 8 ]; then
        echo -e "${RED}❌ Password must be at least 8 characters${NC}"
        exit 1
    fi
    
    # Prompt for debug mode
    echo "Enable debug mode? (y/n, default: n for production):"
    read -r debug_choice
    if [ "$debug_choice" = "y" ]; then
        debug_mode="True"
    else
        debug_mode="False"
    fi
    # Create .env file
    cat > .env << EOF
# ===========================================
# SLYKERTECH ENVIRONMENT CONFIGURATION
# ===========================================

# ===========================================
# DOCKER COMPOSE CONFIGURATION
# ===========================================
# Database password for PostgreSQL
DB_PASSWORD=${db_password}

# Node environment (development/production)
NODE_ENV=production

# Django allowed hosts (comma-separated list)
ALLOWED_HOSTS=localhost,127.0.0.1,api.slykertech.co.zw,slykertech.co.zw,www.slykertech.co.zw

# ===========================================
# DJANGO BACKEND CONFIGURATION
# ===========================================
# Django Debug mode (set to False in production)
DEBUG=${debug_mode}

# CORS Configuration - Origins allowed to make cross-origin requests
CORS_ALLOWED_ORIGINS=http://localhost:3000,https://slykertech.co.zw,https://www.slykertech.co.zw

# CSRF Trusted Origins - Origins allowed to submit forms
CSRF_TRUSTED_ORIGINS=http://localhost:3000,https://slykertech.co.zw,https://www.slykertech.co.zw,https://api.slykertech.co.zw

# ===========================================
# FRONTEND API CONFIGURATION
# ===========================================
# Backend API URL (used for both build and runtime)
NEXT_PUBLIC_API_URL=http://localhost:8000/api
NEXT_PUBLIC_WS_URL=ws://localhost:8000/ws

# Site URL
NEXT_PUBLIC_SITE_URL=http://localhost:3000

# For production deployment, uncomment and update these:
# NEXT_PUBLIC_API_URL=https://api.slykertech.co.zw/api
# NEXT_PUBLIC_WS_URL=wss://api.slykertech.co.zw/ws
# NEXT_PUBLIC_SITE_URL=https://slykertech.co.zw
EOF
    
    echo -e "${GREEN}✅ .env file created${NC}"
fi

# Check if volumes exist and if we need to reset the database
echo ""
echo "Checking Docker volumes..."
if docker volume ls | grep -q "$POSTGRES_VOLUME"; then
    echo -e "${YELLOW}⚠️  Existing PostgreSQL volume found${NC}"
    echo "If you're experiencing database authentication errors, you may need to reset the database."
    echo "Do you want to remove existing database volume and start fresh? (y/n)"
    read -r reset_db
    
    if [ "$reset_db" = "y" ]; then
        echo "Stopping and removing containers..."
        docker-compose down
        
        echo "Removing PostgreSQL volume..."
        docker volume rm "$POSTGRES_VOLUME" 2>/dev/null || true
        
        echo -e "${GREEN}✅ Database volume removed${NC}"
    fi
fi

echo ""
echo "========================================="
echo "Starting Docker Services"
echo "========================================="
echo ""

# Build and start services
echo "Building and starting services..."
docker-compose up -d --build

echo ""
echo "Waiting for database to be ready..."
elapsed=0
while [ $elapsed -lt $MAX_WAIT_TIME ]; do
    if docker-compose exec -T db pg_isready -U slykertech >/dev/null 2>&1; then
        echo -e "${GREEN}✅ Database is ready${NC}"
        break
    fi
    echo "Waiting for database... (${elapsed}s / ${MAX_WAIT_TIME}s)"
    sleep $CHECK_INTERVAL
    elapsed=$((elapsed + CHECK_INTERVAL))
done

if [ $elapsed -ge $MAX_WAIT_TIME ]; then
    echo -e "${YELLOW}⚠️  Database did not become ready in expected time${NC}"
    echo "Continuing anyway, but you may need to run migrations manually"
fi

# Run migrations
echo ""
echo "Running database migrations..."
docker-compose exec -T backend python manage.py migrate

# Check if superuser exists, if not prompt to create one
echo ""
echo "Do you want to create a superuser? (y/n)"
read -r create_superuser

if [ "$create_superuser" = "y" ]; then
    docker-compose exec backend python manage.py createsuperuser
fi

# Populate initial services
echo ""
echo "Populating initial services..."
docker-compose exec -T backend python manage.py populate_services || echo "Note: populate_services command not available or already run"

echo ""
echo "========================================="
echo -e "${GREEN}Setup Complete! 🎉${NC}"
echo "========================================="
echo ""
echo "Services are running:"
echo "  - Frontend:   http://localhost:3000"
echo "  - Backend:    http://localhost:8000"
echo "  - Admin:      http://localhost:8000/admin"
echo "  - Database:   localhost:5432"
echo "  - Redis:      localhost:6379"
echo ""
echo "To view logs:"
echo "  docker-compose logs -f"
echo ""
echo "To stop services:"
echo "  docker-compose down"
echo ""
echo "To restart services:"
echo "  docker-compose restart"
echo ""
echo -e "${YELLOW}⚠️  Remember to keep your database password safe and consistent!${NC}"
echo "The password is stored in your .env file."
echo ""
