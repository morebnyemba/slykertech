#!/bin/bash
# Fix Database Authentication Issues
# This script helps resolve database password authentication errors

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo "========================================="
echo "Database Authentication Fix"
echo "========================================="
echo ""

# Check if .env file exists
if [ ! -f ".env" ]; then
    echo -e "${RED}❌ .env file not found!${NC}"
    echo ""
    echo "The .env file is required for Docker Compose to work properly."
    echo "Would you like to create it now? (y/n)"
    read -r create_env
    
    if [ "$create_env" = "y" ]; then
        echo ""
        echo "Enter database password (press Enter for default 'devpassword'):"
        read -s -r db_password
        db_password=${db_password:-devpassword}
        echo ""
        
        cp .env.example .env
        sed -i "s/DB_PASSWORD=.*/DB_PASSWORD=${db_password}/" .env
        
        echo -e "${GREEN}✅ .env file created with password: ${db_password}${NC}"
    else
        echo "Please create a .env file manually and try again."
        exit 1
    fi
fi

echo -e "${BLUE}Current database password in .env:${NC}"
grep "DB_PASSWORD=" .env | cut -d'=' -f2

echo ""
echo -e "${YELLOW}⚠️  Database authentication errors usually occur when:${NC}"
echo "1. The .env file doesn't exist"
echo "2. The DB_PASSWORD in .env doesn't match what the database was initialized with"
echo "3. The database volume contains data with a different password"
echo ""

echo "To fix this issue, we need to:"
echo "1. Stop all containers"
echo "2. Remove the existing database volume"
echo "3. Restart with the correct password"
echo ""

echo -e "${RED}⚠️  WARNING: This will delete all existing database data!${NC}"
echo "Do you want to proceed? (y/n)"
read -r proceed

if [ "$proceed" != "y" ]; then
    echo "Operation cancelled."
    exit 0
fi

echo ""
echo "Step 1: Stopping containers..."
docker-compose down

echo ""
echo "Step 2: Removing database volume..."
docker volume rm slykertech_postgres_data 2>/dev/null || echo "Volume not found (this is OK)"

echo ""
echo "Step 3: Starting services with fresh database..."
docker-compose up -d --build

echo ""
echo "Waiting for database to be ready..."
sleep 15

echo ""
echo "Step 4: Running migrations..."
docker-compose exec -T backend python manage.py migrate

echo ""
echo "========================================="
echo -e "${GREEN}Database Reset Complete! ✅${NC}"
echo "========================================="
echo ""
echo "The database has been recreated with the password from your .env file."
echo ""
echo "Next steps:"
echo "1. Create a superuser: docker-compose exec backend python manage.py createsuperuser"
echo "2. Populate initial data: docker-compose exec backend python manage.py populate_services"
echo ""
echo "Your services should now be accessible at:"
echo "  - Frontend: http://localhost:3000"
echo "  - Backend:  http://localhost:8000"
echo "  - Admin:    http://localhost:8000/admin"
echo ""
