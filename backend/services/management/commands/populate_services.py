from django.core.management.base import BaseCommand
from services.models import Service


class Command(BaseCommand):
    help = 'Populate database with all services from frontend'

    def handle(self, *args, **kwargs):
        services_data = [
            {
                'name': 'Custom Software Development',
                'category': 'development',
                'description': 'Bespoke software solutions tailored to your business requirements',
                'features': [
                    'Web application development',
                    'Desktop software solutions',
                    'Legacy system modernization',
                    'API development',
                    'Microservices architecture'
                ],
                'base_price': 2999.99,
            },
            {
                'name': 'Mobile App Development',
                'category': 'development',
                'description': 'Cross-platform mobile applications for iOS and Android',
                'features': [
                    'React Native development',
                    'Flutter applications',
                    'Native iOS/Android apps',
                    'Mobile UI/UX design',
                    'App store optimization'
                ],
                'base_price': 3999.99,
            },
            {
                'name': 'E-commerce Solutions',
                'category': 'development',
                'description': 'Complete online store development and integration',
                'features': [
                    'Shopping cart systems',
                    'Payment gateway integration',
                    'Product catalog management',
                    'Order processing systems',
                    'Multi-vendor platforms'
                ],
                'base_price': 4999.99,
            },
            {
                'name': 'Web Design & Development',
                'category': 'design',
                'description': 'Beautiful, functional websites that convert visitors',
                'features': [
                    'Responsive web design',
                    'CMS development (WordPress, etc.)',
                    'Landing page creation',
                    'UI/UX optimization',
                    'Website maintenance'
                ],
                'base_price': 1499.99,
            },
            {
                'name': 'Cloud Services',
                'category': 'cloud',
                'description': 'Scalable cloud infrastructure solutions',
                'features': [
                    'AWS/Azure/GCP setup',
                    'Cloud migration',
                    'Serverless architecture',
                    'Container orchestration',
                    'Cloud security'
                ],
                'base_price': 499.99,
            },
            {
                'name': 'Database Solutions',
                'category': 'development',
                'description': 'Optimized database architecture and management',
                'features': [
                    'Database design',
                    'SQL/NoSQL solutions',
                    'Data migration',
                    'Performance tuning',
                    'Backup solutions'
                ],
                'base_price': 799.99,
            },
            {
                'name': 'Cyber Security',
                'category': 'other',
                'description': 'Comprehensive protection for your digital assets',
                'features': [
                    'Vulnerability assessments',
                    'Penetration testing',
                    'Security audits',
                    'Compliance consulting',
                    'Incident response'
                ],
                'base_price': 1999.99,
            },
            {
                'name': 'Digital Marketing',
                'category': 'marketing',
                'description': 'Data-driven marketing strategies for growth',
                'features': [
                    'SEO services',
                    'PPC advertising',
                    'Social media marketing',
                    'Content marketing',
                    'Analytics & reporting'
                ],
                'base_price': 599.99,
            },
            {
                'name': 'Business Intelligence',
                'category': 'development',
                'description': 'Turn your data into actionable insights',
                'features': [
                    'Data visualization',
                    'Dashboard development',
                    'Predictive analytics',
                    'KPI tracking',
                    'Data warehousing'
                ],
                'base_price': 1499.99,
            },
            {
                'name': 'IT Support & Maintenance',
                'category': 'maintenance',
                'description': 'Reliable technical support for your business',
                'features': [
                    '24/7 monitoring',
                    'Help desk services',
                    'System maintenance',
                    'Backup management',
                    'Disaster recovery'
                ],
                'base_price': 299.99,
            },
            {
                'name': 'Video Production',
                'category': 'design',
                'description': 'Professional video content for your brand',
                'features': [
                    'Promotional videos',
                    'Product demos',
                    'Animation services',
                    'Video editing',
                    'Live streaming'
                ],
                'base_price': 999.99,
            },
            {
                'name': 'Web Hosting',
                'category': 'hosting',
                'description': 'Reliable web hosting with cPanel access',
                'features': ['cPanel Access', 'Email Accounts', 'MySQL Databases', '24/7 Support', 'SSL Certificate'],
                'base_price': 9.99,
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
                # Update existing service with new data - only valid fields
                service.category = service_data['category']
                service.description = service_data['description']
                service.features = service_data['features']
                service.base_price = service_data['base_price']
                service.save()
                self.stdout.write(self.style.WARNING(f'Updated service: {service.name}'))

        self.stdout.write(self.style.SUCCESS(f'Successfully populated {len(services_data)} services'))
