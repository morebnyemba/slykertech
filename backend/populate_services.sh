#!/bin/bash

# Script to populate services in the database
# This will create/update hosting products with their categories

echo "=========================================="
echo "Populating Services Database"
echo "=========================================="
echo ""

# Run the management command through Docker
echo "📦 Populating hosting products..."
docker-compose exec backend python manage.py populate_hosting_products

echo ""
echo "=========================================="
echo "✅ Database population complete!"
echo "=========================================="
echo ""
echo "Created/Updated services:"
echo "  • Shared Hosting: 4 packages (starting at \$4/month)"
echo "  • VPS Hosting: 4 packages (starting at \$6.25/month - Contabo + 25%)"
echo "  • Dedicated Servers: 2 packages (starting at \$100/month)"
echo ""
echo "To view the products:"
echo "  • Admin panel: http://localhost:8000/admin/services/hostingproduct/"
echo "  • API endpoint: http://localhost:8000/api/services/hosting/"
echo ""
