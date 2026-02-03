# PowerShell script to populate services in the database
# This will create/update hosting products with their categories

Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "Populating Services Database" -ForegroundColor Cyan
Write-Host "==========================================" -ForegroundColor Cyan
Write-Host ""

# Run the management command through Docker
Write-Host "📦 Populating hosting products..." -ForegroundColor Green
docker-compose exec backend python manage.py populate_hosting_products

Write-Host ""
Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "✅ Database population complete!" -ForegroundColor Green
Write-Host "==========================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "Created/Updated services:" -ForegroundColor White
Write-Host "  • Shared Hosting: 4 packages (starting at `$4/month)" -ForegroundColor Gray
Write-Host "  • VPS Hosting: 4 packages (starting at `$6.25/month - Contabo + 25%)" -ForegroundColor Gray
Write-Host "  • Dedicated Servers: 2 packages (starting at `$100/month)" -ForegroundColor Gray
Write-Host ""
Write-Host "To view the products:" -ForegroundColor White
Write-Host "  • Admin panel: http://localhost:8000/admin/services/hostingproduct/" -ForegroundColor Gray
Write-Host "  • API endpoint: http://localhost:8000/api/services/hosting/" -ForegroundColor Gray
Write-Host ""
