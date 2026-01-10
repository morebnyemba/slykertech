from django.core.management.base import BaseCommand
from services.models import Service


class Command(BaseCommand):
    help = 'Populate database with initial services'

    def handle(self, *args, **kwargs):
        services_data = [
            {
                'name': 'Web Hosting',
                'category': 'hosting',
                'description': 'Reliable web hosting with cPanel access',
                'features': ['cPanel Access', 'Email Accounts', 'MySQL Databases', '24/7 Support', 'SSL Certificate'],
                'base_price': 9.99,
            },
            {
                'name': 'Cloud VPS',
                'category': 'cloud',
                'description': 'Scalable cloud VPS hosting',
                'features': ['Root Access', 'Choice of OS', 'SSD Storage', 'DDoS Protection', 'Backups'],
                'base_price': 29.99,
            },
            {
                'name': 'Custom Web Development',
                'category': 'development',
                'description': 'Bespoke web application development',
                'features': ['Custom Design', 'Responsive Layout', 'SEO Optimized', 'CMS Integration', 'Maintenance'],
                'base_price': 1999.99,
            },
            {
                'name': 'Website Maintenance',
                'category': 'maintenance',
                'description': 'Ongoing website maintenance and updates',
                'features': ['Regular Updates', 'Security Monitoring', 'Backup Management', 'Content Updates', 'Priority Support'],
                'base_price': 99.99,
            },
            {
                'name': 'SEO Services',
                'category': 'marketing',
                'description': 'Search engine optimization services',
                'features': ['Keyword Research', 'On-Page SEO', 'Link Building', 'Analytics', 'Monthly Reports'],
                'base_price': 299.99,
            },
            {
                'name': 'Domain Registration',
                'category': 'hosting',
                'description': 'Domain name registration and management',
                'features': ['Multiple TLDs', 'DNS Management', 'WHOIS Privacy', 'Auto-Renewal', 'Transfer Support'],
                'base_price': 14.99,
            },
        ]

        for service_data in services_data:
            service, created = Service.objects.get_or_create(
                name=service_data['name'],
                defaults=service_data
            )
            if created:
                self.stdout.write(self.style.SUCCESS(f'Created service: {service.name}'))
            else:
                self.stdout.write(self.style.WARNING(f'Service already exists: {service.name}'))

        self.stdout.write(self.style.SUCCESS('Successfully populated services'))
