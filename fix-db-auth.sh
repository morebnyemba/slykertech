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

# Detect docker compose command (docker compose vs docker-compose)
if docker compose version &> /dev/null; then
    DOCKER_COMPOSE="docker compose"
elif command -v docker-compose &> /dev/null; then
    DOCKER_COMPOSE="docker-compose"
else
    echo -e "${RED}❌ Neither 'docker compose' nor 'docker-compose' is available${NC}"
    echo "Please install Docker Compose first."
    exit 1
fi

# Configuration
# Get the actual volume name from Docker Compose (project name + volume name)
# Docker Compose uses lowercase directory name with non-alphanumeric chars (except _ and -) removed
PROJECT_NAME=$(basename "$(pwd)" | tr '[:upper:]' '[:lower:]' | sed 's/[^a-z0-9_-]//g')
POSTGRES_VOLUME="${PROJECT_NAME}_postgres_data"
MAX_WAIT_TIME=60
CHECK_INTERVAL=3

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
    
    if [[ "$create_env" =~ ^[Yy]([Ee][Ss])?$ ]]; then
        echo ""
        echo "Enter database password (minimum 8 characters, no default):"
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
        
        # Create .env from example
        cp .env.example .env
        
        # Safely update DB_PASSWORD using awk to avoid sed delimiter issues
        awk -v pwd="$db_password" '{if ($0 ~ /^DB_PASSWORD=/) {print "DB_PASSWORD=" pwd} else {print $0}}' .env > .env.tmp && mv .env.tmp .env
        
        echo -e "${GREEN}✅ .env file created${NC}"
    else
        echo "Please create a .env file manually and try again."
        exit 1
    fi
fi

echo -e "${BLUE}Database password is configured in .env${NC}"

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

if [[ ! "$proceed" =~ ^[Yy]([Ee][Ss])?$ ]]; then
    echo "Operation cancelled."
    exit 0
fi

echo ""
echo "Step 1: Stopping containers..."
$DOCKER_COMPOSE down

echo ""
echo "Step 2: Removing database volume..."
echo "Looking for volume: $POSTGRES_VOLUME"
docker volume rm "$POSTGRES_VOLUME" 2>/dev/null || echo "Volume not found or already removed (this is OK)"

echo ""
echo "Step 3: Starting services with fresh database..."
$DOCKER_COMPOSE up -d --build

echo ""
echo "Waiting for database to be ready..."
elapsed=0
while [ $elapsed -lt $MAX_WAIT_TIME ]; do
    if $DOCKER_COMPOSE exec -T db pg_isready -U slykertech >/dev/null 2>&1; then
        echo -e "${GREEN}✅ Database is ready${NC}"
        break
    fi
    echo "Waiting for database... (${elapsed}s / ${MAX_WAIT_TIME}s)"
    sleep $CHECK_INTERVAL
    elapsed=$((elapsed + CHECK_INTERVAL))
done

if [ $elapsed -ge $MAX_WAIT_TIME ]; then
    echo -e "${RED}❌ Database did not become ready in time${NC}"
    echo "Check logs with: $DOCKER_COMPOSE logs db"
    exit 1
fi

echo ""
echo "Waiting for backend service to be ready..."
sleep 5

echo ""
echo "Step 4: Running database cleanup..."
if $DOCKER_COMPOSE exec -T backend python /app/cleanup_db_types.py; then
    echo -e "${GREEN}✅ Database cleanup completed${NC}"
else
    echo -e "${YELLOW}⚠️  Database cleanup warning (may be normal)${NC}"
fi

echo ""
echo "Step 5: Running migrations..."
if $DOCKER_COMPOSE exec -T backend python manage.py migrate; then
    echo -e "${GREEN}✅ Migrations completed successfully${NC}"
else
    echo -e "${RED}❌ Migrations failed${NC}"
    echo "Check backend logs with: $DOCKER_COMPOSE logs backend"
    echo "You may need to run migrations manually: $DOCKER_COMPOSE exec backend python manage.py migrate"
fi

echo ""
echo "========================================="
echo -e "${GREEN}Database Reset Complete! ✅${NC}"
echo "========================================="
echo ""
echo "The database has been recreated with the password from your .env file."
echo ""
echo "Next steps:"
echo "1. Create a superuser: $DOCKER_COMPOSE exec backend python manage.py createsuperuser"
echo "2. Populate initial data: $DOCKER_COMPOSE exec backend python manage.py populate_services"
echo ""
echo "Your services should now be accessible at:"
echo "  - Frontend: http://localhost:3000"
echo "  - Backend:  http://localhost:8000"
echo "  - Admin:    http://localhost:8000/admin"
echo ""
