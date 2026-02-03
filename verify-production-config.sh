#!/bin/bash
# Production Configuration Verification Script

echo "========================================="
echo "Slyker Tech - Production Config Check"
echo "========================================="

echo ""
echo "1. Checking ALLOWED_HOSTS configuration..."
docker compose exec -T backend python -c "from django.conf import settings; print('ALLOWED_HOSTS:', settings.ALLOWED_HOSTS)"

echo ""
echo "2. Checking if 'backend' is in ALLOWED_HOSTS..."
docker compose exec -T backend python -c "from django.conf import settings; print('backend' in settings.ALLOWED_HOSTS)"

echo ""
echo "3. Checking migration status..."
docker compose exec -T backend python manage.py showmigrations integrations

echo ""
echo "4. Checking for pending migrations..."
docker compose exec -T backend python manage.py makemigrations --dry-run --check

echo ""
echo "5. Testing Gemini configuration..."
docker compose exec -T backend python -c "from livechat.gemini_client import get_gemini_config; config = get_gemini_config(); print('Config loaded:', config is not None); print('Model:', config.get('model') if config else 'N/A')"

echo ""
echo "========================================="
echo "Verification complete!"
echo "========================================="
