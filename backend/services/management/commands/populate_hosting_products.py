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
                'name': 'Shared Starter',
                'slug': 'shared-starter',
                'description': 'Perfect for small websites and blogs',
                'hosting_type': 'shared',
                'disk_space': 10240,  # 10 GB in MB
                'bandwidth': 102400,  # 100 GB in MB
                'email_accounts': 5,
                'databases': 2,
                'ftp_accounts': 2,
                'subdomains': 5,
                'addon_domains': 0,
                'parked_domains': 0,
                'ssl_certificate': True,
                'dedicated_ip': False,
                'cpanel_access': True,
                'ssh_access': False,
                'cron_jobs': True,
                'backups_included': True,
                'monthly_price': Decimal('10.00'),
                'quarterly_price': Decimal('27.00'),
                'annual_price': Decimal('96.00'),
                'is_featured': False,
                'sort_order': 1,
            },
            {
                'name': 'Shared Business',
                'slug': 'shared-business',
                'description': 'Great for growing businesses with multiple sites',
                'hosting_type': 'shared',
                'disk_space': 51200,  # 50 GB in MB
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
                'monthly_price': Decimal('25.00'),
                'quarterly_price': Decimal('67.50'),
                'annual_price': Decimal('240.00'),
                'is_featured': True,
                'sort_order': 2,
            },
        ]
        
        # VPS Hosting Products
        vps_products = [
            {
                'name': 'VPS Basic',
                'slug': 'vps-basic',
                'description': 'Entry-level VPS with dedicated resources',
                'hosting_type': 'vps',
                'disk_space': 51200,  # 50 GB
                'bandwidth': 0,  # Unlimited
                'email_accounts': 0,  # Unlimited
                'databases': 0,  # Unlimited
                'ftp_accounts': 0,  # Unlimited
                'subdomains': 0,  # Unlimited
                'addon_domains': 0,  # Unlimited
                'parked_domains': 0,  # Unlimited
                'ssl_certificate': True,
                'dedicated_ip': True,
                'cpanel_access': True,
                'ssh_access': True,
                'cron_jobs': True,
                'backups_included': True,
                'monthly_price': Decimal('30.00'),
                'quarterly_price': Decimal('81.00'),
                'annual_price': Decimal('288.00'),
                'is_featured': False,
                'sort_order': 3,
            },
            {
                'name': 'VPS Business',
                'slug': 'vps-business',
                'description': 'Scalable resources for growing businesses',
                'hosting_type': 'vps',
                'disk_space': 102400,  # 100 GB
                'bandwidth': 0,  # Unlimited
                'email_accounts': 0,  # Unlimited
                'databases': 0,  # Unlimited
                'ftp_accounts': 0,  # Unlimited
                'subdomains': 0,  # Unlimited
                'addon_domains': 0,  # Unlimited
                'parked_domains': 0,  # Unlimited
                'ssl_certificate': True,
                'dedicated_ip': True,
                'cpanel_access': True,
                'ssh_access': True,
                'cron_jobs': True,
                'backups_included': True,
                'monthly_price': Decimal('50.00'),
                'quarterly_price': Decimal('135.00'),
                'annual_price': Decimal('480.00'),
                'is_featured': True,
                'sort_order': 4,
            },
        ]
        
        # Dedicated Server Products
        dedicated_products = [
            {
                'name': 'Dedicated Starter',
                'slug': 'dedicated-starter',
                'description': 'Entry-level dedicated server',
                'hosting_type': 'dedicated',
                'disk_space': 512000,  # 500 GB
                'bandwidth': 0,  # Unlimited
                'email_accounts': 0,  # Unlimited
                'databases': 0,  # Unlimited
                'ftp_accounts': 0,  # Unlimited
                'subdomains': 0,  # Unlimited
                'addon_domains': 0,  # Unlimited
                'parked_domains': 0,  # Unlimited
                'ssl_certificate': True,
                'dedicated_ip': True,
                'cpanel_access': True,
                'ssh_access': True,
                'cron_jobs': True,
                'backups_included': True,
                'monthly_price': Decimal('100.00'),
                'quarterly_price': Decimal('270.00'),
                'annual_price': Decimal('960.00'),
                'is_featured': False,
                'sort_order': 5,
            },
            {
                'name': 'Dedicated Enterprise',
                'slug': 'dedicated-enterprise',
                'description': 'Maximum performance and control',
                'hosting_type': 'dedicated',
                'disk_space': 2048000,  # 2 TB
                'bandwidth': 0,  # Unlimited
                'email_accounts': 0,  # Unlimited
                'databases': 0,  # Unlimited
                'ftp_accounts': 0,  # Unlimited
                'subdomains': 0,  # Unlimited
                'addon_domains': 0,  # Unlimited
                'parked_domains': 0,  # Unlimited
                'ssl_certificate': True,
                'dedicated_ip': True,
                'cpanel_access': True,
                'ssh_access': True,
                'cron_jobs': True,
                'backups_included': True,
                'monthly_price': Decimal('200.00'),
                'quarterly_price': Decimal('540.00'),
                'annual_price': Decimal('1920.00'),
                'is_featured': False,
                'sort_order': 6,
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
