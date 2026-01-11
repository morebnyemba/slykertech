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
                'payment_type': 'one_time',
                'pricing_options': {
                    'basic': {'price': 2999.99, 'description': 'Basic application'},
                    'standard': {'price': 5999.99, 'description': 'Standard with integrations'},
                    'premium': {'price': 9999.99, 'description': 'Enterprise solution'}
                },
                'requires_provisioning': False,
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
                'payment_type': 'one_time',
                'pricing_options': {
                    'basic': {'price': 3999.99, 'description': 'Single platform'},
                    'standard': {'price': 6999.99, 'description': 'Cross-platform'},
                    'premium': {'price': 11999.99, 'description': 'Native both platforms'}
                },
                'requires_provisioning': False,
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
                'payment_type': 'one_time',
                'pricing_options': {
                    'basic': {'price': 4999.99, 'description': 'Basic online store'},
                    'standard': {'price': 8999.99, 'description': 'With payment integration'},
                    'premium': {'price': 14999.99, 'description': 'Multi-vendor marketplace'}
                },
                'requires_provisioning': False,
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
                'payment_type': 'one_time',
                'pricing_options': {
                    'basic': {'price': 1499.99, 'description': '5-page website'},
                    'standard': {'price': 2999.99, 'description': '10-page website with CMS'},
                    'premium': {'price': 4999.99, 'description': 'Custom design & development'}
                },
                'requires_provisioning': False,
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
                'payment_type': 'recurring',
                'pricing_options': {
                    'monthly': {'price': 499.99, 'description': 'Basic cloud setup'},
                    'quarterly': {'price': 1399.99, 'description': '3 months - 10% off'},
                    'annual': {'price': 4999.99, 'description': '12 months - 20% off'}
                },
                'requires_provisioning': False,
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
                'payment_type': 'one_time',
                'pricing_options': {
                    'basic': {'price': 799.99, 'description': 'Database design'},
                    'standard': {'price': 1499.99, 'description': 'Design + migration'},
                    'premium': {'price': 2499.99, 'description': 'Full setup + optimization'}
                },
                'requires_provisioning': False,
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
                'payment_type': 'one_time',
                'pricing_options': {
                    'basic': {'price': 1999.99, 'description': 'Security audit'},
                    'standard': {'price': 3999.99, 'description': 'Audit + penetration testing'},
                    'premium': {'price': 7999.99, 'description': 'Full security assessment'}
                },
                'requires_provisioning': False,
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
                'payment_type': 'recurring',
                'pricing_options': {
                    'monthly': {'price': 599.99, 'description': 'Basic SEO & marketing'},
                    'quarterly': {'price': 1699.99, 'description': '3 months - 5% off'},
                    'annual': {'price': 6299.99, 'description': '12 months - 15% off'}
                },
                'requires_provisioning': False,
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
                'payment_type': 'one_time',
                'pricing_options': {
                    'basic': {'price': 1499.99, 'description': 'Basic dashboard'},
                    'standard': {'price': 2999.99, 'description': 'Advanced analytics'},
                    'premium': {'price': 5999.99, 'description': 'AI-powered insights'}
                },
                'requires_provisioning': False,
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
                'payment_type': 'recurring',
                'pricing_options': {
                    'monthly': {'price': 299.99, 'description': 'Basic support'},
                    'quarterly': {'price': 849.99, 'description': '3 months - 5% off'},
                    'annual': {'price': 3199.99, 'description': '12 months - 15% off'}
                },
                'requires_provisioning': False,
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
                'payment_type': 'one_time',
                'pricing_options': {
                    'basic': {'price': 999.99, 'description': '1-2 minute video'},
                    'standard': {'price': 1999.99, 'description': '3-5 minute video'},
                    'premium': {'price': 3999.99, 'description': '5+ minutes + animation'}
                },
                'requires_provisioning': False,
            },
            {
                'name': 'Web Hosting',
                'category': 'hosting',
                'description': 'Reliable web hosting with cPanel access',
                'features': ['cPanel Access', 'Email Accounts', 'MySQL Databases', '24/7 Support', 'SSL Certificate'],
                'base_price': 9.99,
                'payment_type': 'recurring',
                'pricing_options': {
                    'monthly': {'price': 9.99, 'description': 'Basic hosting - 5GB'},
                    'quarterly': {'price': 27.99, 'description': '3 months - 10GB'},
                    'annual': {'price': 99.99, 'description': '12 months - 20GB + free domain'}
                },
                'requires_provisioning': True,
                'provisioning_type': 'cpanel',
            },
            {
                'name': 'Domain Registration',
                'category': 'hosting',
                'description': 'Domain name registration and management',
                'features': ['Multiple TLDs', 'DNS Management', 'WHOIS Privacy', 'Auto-Renewal', 'Transfer Support'],
                'base_price': 14.99,
                'payment_type': 'recurring',
                'pricing_options': {
                    '1_year': {'price': 14.99, 'description': '1 year registration'},
                    '2_years': {'price': 27.99, 'description': '2 years - 10% off'},
                    '5_years': {'price': 64.99, 'description': '5 years - 15% off'}
                },
                'requires_provisioning': True,
                'provisioning_type': 'domain',
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
                # Update existing service with new data
                for field in ['category', 'description', 'features', 'base_price', 
                            'payment_type', 'pricing_options', 'requires_provisioning', 
                            'provisioning_type']:
                    if field in service_data:
                        setattr(service, field, service_data[field])
                service.save()
                self.stdout.write(self.style.WARNING(f'Updated service: {service.name}'))

        self.stdout.write(self.style.SUCCESS(f'Successfully populated {len(services_data)} services'))
