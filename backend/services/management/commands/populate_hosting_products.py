from django.core.management.base import BaseCommand
from services.models import HostingProduct
from decimal import Decimal


class Command(BaseCommand):
    help = 'Populate sample hosting products for testing'

    def handle(self, *args, **kwargs):
        self.stdout.write('Creating sample hosting products...')
        
        # Shared Hosting Products
        shared_products = [
            {
                'name': 'Shared Basic',
                'slug': 'shared-basic',
                'description': 'Perfect for personal websites and blogs',
                'hosting_type': 'shared',

                'disk_space': 5120,  # 5 GB in MB
                'bandwidth': 51200,  # 50 GB in MB
                'email_accounts': 3,
                'databases': 1,
                'ftp_accounts': 1,
                'subdomains': 3,
                'addon_domains': 0,
                'parked_domains': 0,
                'ssl_certificate': True,
                'dedicated_ip': False,
                'cpanel_access': True,
                'ssh_access': False,
                'cron_jobs': False,
                'backups_included': True,
                'monthly_price': Decimal('4.00'),
                'quarterly_price': Decimal('10.80'),
                'annual_price': Decimal('38.40'),
                'is_featured': False,
                'sort_order': 1,
            },
            {
                'name': 'Shared Starter',
                'slug': 'shared-starter',
                'description': 'Great for small websites and portfolios',
                'hosting_type': 'shared',
                'disk_space': 10240,  # 10 GB in MB
                'bandwidth': 102400,  # 100 GB in MB
                'email_accounts': 10,
                'databases': 3,
                'ftp_accounts': 3,
                'subdomains': 10,
                'addon_domains': 1,
                'parked_domains': 1,
                'ssl_certificate': True,
                'dedicated_ip': False,
                'cpanel_access': True,
                'ssh_access': False,
                'cron_jobs': True,
                'backups_included': True,
                'monthly_price': Decimal('8.00'),
                'quarterly_price': Decimal('21.60'),
                'annual_price': Decimal('76.80'),
                'is_featured': True,
                'sort_order': 2,
            },
            {
                'name': 'Shared Business',
                'slug': 'shared-business',
                'description': 'Ideal for growing businesses with multiple sites',
                'hosting_type': 'shared',
                'disk_space': 30720,  # 30 GB in MB
                'bandwidth': 0,  # Unlimited
                'email_accounts': 0,  # Unlimited
                'databases': 10,
                'ftp_accounts': 10,
                'subdomains': 0,  # Unlimited
                'addon_domains': 5,
                'parked_domains': 5,
                'ssl_certificate': True,
                'dedicated_ip': False,
                'cpanel_access': True,
                'ssh_access': False,
                'cron_jobs': True,
                'backups_included': True,
                'monthly_price': Decimal('15.00'),
                'quarterly_price': Decimal('40.50'),
                'annual_price': Decimal('144.00'),
                'is_featured': False,
                'sort_order': 3,
            },
            {
                'name': 'Shared Professional',
                'slug': 'shared-professional',
                'description': 'Maximum shared hosting features for professionals',
                'hosting_type': 'shared',
                'disk_space': 51200,  # 50 GB in MB
                'bandwidth': 0,  # Unlimited
                'email_accounts': 0,  # Unlimited
                'databases': 0,  # Unlimited
                'ftp_accounts': 0,  # Unlimited
                'subdomains': 0,  # Unlimited
                'addon_domains': 0,  # Unlimited
                'parked_domains': 0,  # Unlimited
                'ssl_certificate': True,
                'dedicated_ip': False,
                'cpanel_access': True,
                'ssh_access': True,
                'cron_jobs': True,
                'backups_included': True,
                'monthly_price': Decimal('25.00'),
                'quarterly_price': Decimal('67.50'),
                'annual_price': Decimal('240.00'),
                'is_featured': False,
                'sort_order': 4,
            },
        ]
        
        # VPS Hosting Products (Contabo-based pricing with 25% markup)
        vps_products = [
            {
                'name': 'VPS S',
                'slug': 'vps-s',
                'description': '4 vCPU Cores, 6 GB RAM, 50 GB NVMe SSD',
                'hosting_type': 'vps',
                'cpu_cores': 4,
                'ram_gb': 6,
                'storage_type': 'NVMe',
                'disk_space': 51200,  # 50 GB
                'bandwidth': 0,  # Unlimited (32 TB traffic)
                'email_accounts': 0,
                'databases': 0,
                'ftp_accounts': 0,
                'subdomains': 0,
                'addon_domains': 0,
                'parked_domains': 0,
                'ssl_certificate': True,
                'dedicated_ip': True,
                'cpanel_access': False,  # Optional, not included by default
                'ssh_access': True,
                'cron_jobs': True,
                'backups_included': True,
                'monthly_price': Decimal('6.25'),
                'quarterly_price': Decimal('16.88'),
                'annual_price': Decimal('60.00'),
                'is_featured': False,
                'sort_order': 5,
            },
            {
                'name': 'VPS M',
                'slug': 'vps-m',
                'description': '6 vCPU Cores, 16 GB RAM, 100 GB NVMe SSD',
                'hosting_type': 'vps',
                'cpu_cores': 6,
                'ram_gb': 16,
                'storage_type': 'NVMe',
                'disk_space': 102400,  # 100 GB
                'bandwidth': 0,  # Unlimited
                'email_accounts': 0,  # No limits - full root access
                'databases': 0,  # No limits
                'ftp_accounts': 0,  # No limits
                'subdomains': 0,  # No limits
                'addon_domains': 0,  # Unlimited
                'parked_domains': 0,  # Unlimited
                'ssl_certificate': True,
                'dedicated_ip': True,
                'cpanel_access': False,
                'ssh_access': True,
                'cron_jobs': True,
                'backups_included': True,
                'monthly_price': Decimal('12.50'),
                'quarterly_price': Decimal('33.75'),
                'annual_price': Decimal('120.00'),
                'is_featured': True,
                'sort_order': 6,
            },
            {
                'name': 'VPS L',
                'slug': 'vps-l',
                'description': '8 vCPU Cores, 30 GB RAM, 200 GB NVMe SSD',
                'hosting_type': 'vps',
                'cpu_cores': 8,
                'ram_gb': 30,
                'storage_type': 'NVMe',
                'disk_space': 204800,  # 200 GB
                'bandwidth': 0,  # Unlimited
                'email_accounts': 0,  # No limits - full root access
                'databases': 0,  # No limits
                'ftp_accounts': 0,  # No limits
                'subdomains': 0,  # No limits
                'addon_domains': 0,  # Unlimited
                'parked_domains': 0,  # Unlimited
                'ssl_certificate': True,
                'dedicated_ip': True,
                'cpanel_access': False,
                'ssh_access': True,
                'cron_jobs': True,
                'backups_included': True,
                'monthly_price': Decimal('25.00'),
                'quarterly_price': Decimal('67.50'),
                'annual_price': Decimal('240.00'),
                'is_featured': False,
                'sort_order': 7,
            },
            {
                'name': 'VPS XL',
                'slug': 'vps-xl',
                'description': '10 vCPU Cores, 60 GB RAM, 400 GB NVMe SSD',
                'hosting_type': 'vps',
                'cpu_cores': 10,
                'ram_gb': 60,
                'storage_type': 'NVMe',
                'disk_space': 409600,  # 400 GB
                'bandwidth': 0,  # Unlimited
                'email_accounts': 0,  # No limits - full root access
                'databases': 0,  # No limits
                'ftp_accounts': 0,  # No limits
                'subdomains': 0,  # No limits
                'addon_domains': 0,  # Unlimited
                'parked_domains': 0,  # Unlimited
                'ssl_certificate': True,
                'dedicated_ip': True,
                'cpanel_access': False,
                'ssh_access': True,
                'cron_jobs': True,
                'backups_included': True,
                'monthly_price': Decimal('50.00'),
                'quarterly_price': Decimal('135.00'),
                'annual_price': Decimal('480.00'),
                'is_featured': False,
                'sort_order': 8,
            },
        ]
        
        # Dedicated Server Products
        dedicated_products = [
            {
                'name': 'Dedicated Starter',
                'slug': 'dedicated-starter',
                'description': 'Intel Xeon E-2136, 32 GB RAM, 500 GB NVMe SSD',
                'hosting_type': 'dedicated',
                'cpu_cores': 6,
                'ram_gb': 32,
                'cpu_type': 'Intel Xeon E-2136 (6 Cores @ 3.3 GHz)',
                'storage_type': 'NVMe',
                'disk_space': 512000,  # 500 GB
                'bandwidth': 0,  # Unlimited
                'email_accounts': 0,  # No limits - full root access
                'databases': 0,  # No limits
                'ftp_accounts': 0,  # No limits
                'subdomains': 0,  # No limits
                'addon_domains': 0,  # Unlimited
                'parked_domains': 0,  # Unlimited
                'ssl_certificate': True,
                'dedicated_ip': True,
                'cpanel_access': False,
                'ssh_access': True,
                'cron_jobs': True,
                'backups_included': True,
                'monthly_price': Decimal('100.00'),
                'quarterly_price': Decimal('270.00'),
                'annual_price': Decimal('960.00'),
                'is_featured': False,
                'sort_order': 9,
            },
            {
                'name': 'Dedicated Enterprise',
                'slug': 'dedicated-enterprise',
                'description': 'AMD Ryzen 9 5950X, 128 GB RAM, 2 TB NVMe SSD',
                'hosting_type': 'dedicated',
                'cpu_cores': 16,
                'ram_gb': 128,
                'cpu_type': 'AMD Ryzen 9 5950X (16 Cores @ 3.4 GHz)',
                'storage_type': 'NVMe',
                'disk_space': 2048000,  # 2 TB
                'bandwidth': 0,  # Unlimited
                'email_accounts': 0,  # No limits - full root access
                'databases': 0,  # No limits
                'ftp_accounts': 0,  # No limits
                'subdomains': 0,  # No limits
                'addon_domains': 0,  # Unlimited
                'parked_domains': 0,  # Unlimited
                'ssl_certificate': True,
                'dedicated_ip': True,
                'cpanel_access': False,
                'ssh_access': True,
                'cron_jobs': True,
                'backups_included': True,
                'monthly_price': Decimal('200.00'),
                'quarterly_price': Decimal('540.00'),
                'annual_price': Decimal('1920.00'),
                'is_featured': False,
                'sort_order': 10,
            },
        ]
        
        all_products = shared_products + vps_products + dedicated_products
        
        created_count = 0
        updated_count = 0
        
        for product_data in all_products:
            product, created = HostingProduct.objects.update_or_create(
                slug=product_data['slug'],
                defaults=product_data
            )
            if created:
                created_count += 1
                self.stdout.write(self.style.SUCCESS(f'Created: {product.name}'))
            else:
                updated_count += 1
                self.stdout.write(self.style.WARNING(f'Updated: {product.name}'))
        
        self.stdout.write(self.style.SUCCESS(
            f'\nDone! Created {created_count} new products, updated {updated_count} existing products.'
        ))
