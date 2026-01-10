#!/bin/bash
# Quick Start Script for Slyker Tech Fullstack Application

set -e

echo "========================================="
echo "Slyker Tech - Quick Start Setup"
echo "========================================="
echo ""

# Check prerequisites
echo "Checking prerequisites..."

if ! command -v python3 &> /dev/null; then
    echo "❌ Python 3 is not installed. Please install Python 3.8 or higher."
    exit 1
fi
echo "✅ Python 3 is installed: $(python3 --version)"

if ! command -v node &> /dev/null; then
    echo "❌ Node.js is not installed. Please install Node.js 18 or higher."
    exit 1
fi
echo "✅ Node.js is installed: $(node --version)"

if ! command -v npm &> /dev/null; then
    echo "❌ npm is not installed. Please install npm."
    exit 1
fi
echo "✅ npm is installed: $(npm --version)"

echo ""
echo "========================================="
echo "Setting up Backend (Django)"
echo "========================================="
echo ""

# Navigate to backend
cd backend

# Create virtual environment if it doesn't exist
if [ ! -d "venv" ]; then
    echo "Creating virtual environment..."
    python3 -m venv venv
fi

# Activate virtual environment
echo "Activating virtual environment..."
source venv/bin/activate

# Install dependencies
echo "Installing Python dependencies..."
pip install -q --upgrade pip
pip install -q -r requirements.txt

# Create .env file if it doesn't exist
if [ ! -f ".env" ]; then
    echo "Creating .env file from template..."
    cp .env.example .env
    echo "⚠️  Please update .env file with your settings before running in production"
fi

# Run migrations
echo "Running database migrations..."
python manage.py migrate

# Create superuser prompt
echo ""
echo "Do you want to create a superuser? (y/n)"
read -r create_superuser
if [ "$create_superuser" = "y" ]; then
    python manage.py createsuperuser
fi

# Populate services
echo "Populating initial services..."
python manage.py populate_services

echo ""
echo "✅ Backend setup complete!"
echo ""

# Deactivate virtual environment
deactivate

# Go back to root
cd ..

echo "========================================="
echo "Setting up Frontend (Next.js)"
echo "========================================="
echo ""

# Install frontend dependencies
echo "Installing Node.js dependencies..."
npm install

echo ""
echo "✅ Frontend setup complete!"
echo ""

echo "========================================="
echo "Setup Complete! 🎉"
echo "========================================="
echo ""
echo "To start the development servers:"
echo ""
echo "Backend (Terminal 1):"
echo "  cd backend"
echo "  source venv/bin/activate"
echo "  python manage.py runserver 8000"
echo ""
echo "Frontend (Terminal 2):"
echo "  npm run dev"
echo ""
echo "Then visit:"
echo "  Frontend: http://localhost:3000"
echo "  Backend API: http://localhost:8000/api"
echo "  Admin Panel: http://localhost:8000/admin"
echo ""
echo "For more information, see:"
echo "  - README.md"
echo "  - backend/README.md"
echo "  - INTEGRATION_GUIDE.md"
echo "  - DEPLOYMENT_GUIDE.md"
echo ""
