# Generated data migration for domain products pricing

from django.db import migrations
from decimal import Decimal


def populate_domain_products(apps, schema_editor):
    """
    Populate DomainProduct table with common TLDs and their pricing.
    Prices vary: some at $5, some at $15, some at $20 per year.
    """
    DomainProduct = apps.get_model('services', 'DomainProduct')
    
    # Define domain products with varied pricing
    # Format: (tld, description, price_1yr, renewal, transfer, is_featured)
    domain_products = [
        # Premium TLDs at $20/year
        ('.com', 'Commercial domain - most popular TLD', Decimal('20.00'), Decimal('20.00'), Decimal('20.00'), True),
        ('.net', 'Network domain - great for tech companies', Decimal('20.00'), Decimal('20.00'), Decimal('20.00'), True),
        ('.org', 'Organization domain - ideal for non-profits', Decimal('20.00'), Decimal('20.00'), Decimal('20.00'), False),
        ('.io', 'Tech startups and SaaS products', Decimal('20.00'), Decimal('20.00'), Decimal('20.00'), True),
        
        # Standard TLDs at $15/year
        ('.info', 'Information websites', Decimal('15.00'), Decimal('15.00'), Decimal('15.00'), False),
        ('.biz', 'Business websites', Decimal('15.00'), Decimal('15.00'), Decimal('15.00'), False),
        ('.co', 'Company or Colombia domain', Decimal('15.00'), Decimal('15.00'), Decimal('15.00'), False),
        ('.me', 'Personal websites and portfolios', Decimal('15.00'), Decimal('15.00'), Decimal('15.00'), False),
        ('.dev', 'Developer websites and projects', Decimal('15.00'), Decimal('15.00'), Decimal('15.00'), False),
        ('.app', 'Application websites', Decimal('15.00'), Decimal('15.00'), Decimal('15.00'), False),
        
        # Budget TLDs at $5/year
        ('.xyz', 'Generic domain for any purpose', Decimal('5.00'), Decimal('5.00'), Decimal('5.00'), False),
        ('.online', 'Online presence', Decimal('5.00'), Decimal('5.00'), Decimal('5.00'), False),
        ('.site', 'Website domain', Decimal('5.00'), Decimal('5.00'), Decimal('5.00'), False),
        ('.store', 'E-commerce stores', Decimal('5.00'), Decimal('5.00'), Decimal('5.00'), False),
        ('.tech', 'Technology websites', Decimal('5.00'), Decimal('5.00'), Decimal('5.00'), False),
        ('.club', 'Club and community websites', Decimal('5.00'), Decimal('5.00'), Decimal('5.00'), False),
        
        # Country-code TLDs (mixed pricing)
        ('.co.zw', 'Zimbabwe commercial domain', Decimal('15.00'), Decimal('15.00'), Decimal('15.00'), True),
        ('.org.zw', 'Zimbabwe organization domain', Decimal('15.00'), Decimal('15.00'), Decimal('15.00'), False),
        ('.ac.zw', 'Zimbabwe academic domain', Decimal('15.00'), Decimal('15.00'), Decimal('15.00'), False),
        ('.co.za', 'South Africa commercial domain', Decimal('5.00'), Decimal('5.00'), Decimal('5.00'), False),
        ('.co.uk', 'United Kingdom commercial domain', Decimal('15.00'), Decimal('15.00'), Decimal('15.00'), False),
    ]
    
    for idx, (tld, description, price_1yr, renewal, transfer, is_featured) in enumerate(domain_products):
        # Check if the TLD already exists to avoid duplicates
        if not DomainProduct.objects.filter(tld=tld).exists():
            DomainProduct.objects.create(
                tld=tld,
                description=description,
                registration_price_1yr=price_1yr,
                registration_price_2yr=price_1yr * 2,
                registration_price_3yr=price_1yr * 3,
                registration_price_5yr=price_1yr * 5,
                registration_price_10yr=price_1yr * 10,
                renewal_price=renewal,
                transfer_price=transfer,
                redemption_price=price_1yr * 2,  # Double the registration price for redemption
                whois_privacy_price=Decimal('5.00'),  # Standard WHOIS privacy price
                auto_renew_default=True,
                epp_code_required=True,
                grace_period_days=30,
                redemption_period_days=30,
                min_registration_years=1,
                max_registration_years=10,
                is_active=True,
                is_featured=is_featured,
                sort_order=idx,
            )


def reverse_populate_domain_products(apps, schema_editor):
    """Remove the populated domain products."""
    DomainProduct = apps.get_model('services', 'DomainProduct')
    # Only delete the TLDs we created
    tlds_to_delete = [
        '.com', '.net', '.org', '.io',
        '.info', '.biz', '.co', '.me', '.dev', '.app',
        '.xyz', '.online', '.site', '.store', '.tech', '.club',
        '.co.zw', '.org.zw', '.ac.zw', '.co.za', '.co.uk',
    ]
    DomainProduct.objects.filter(tld__in=tlds_to_delete).delete()


class Migration(migrations.Migration):

    dependencies = [
        ('services', '0008_add_service_addon_support'),
    ]

    operations = [
        migrations.RunPython(
            populate_domain_products,
            reverse_populate_domain_products,
        ),
    ]
