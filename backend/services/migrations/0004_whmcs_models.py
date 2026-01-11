# Generated migration for WHMCS models

from django.db import migrations, models
import django.db.models.deletion


class Migration(migrations.Migration):

    dependencies = [
        ('services', '0003_service_payment_type_pricing_provisioning'),
        ('clients', '0001_initial'),
    ]

    operations = [
        # HostingProduct model
        migrations.CreateModel(
            name='HostingProduct',
            fields=[
                ('id', models.BigAutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('name', models.CharField(help_text='e.g., Basic Hosting, Business Hosting', max_length=255)),
                ('slug', models.SlugField(unique=True)),
                ('description', models.TextField()),
                ('disk_space', models.IntegerField(help_text='Disk space in MB, 0 = unlimited')),
                ('bandwidth', models.IntegerField(help_text='Bandwidth in MB per month, 0 = unlimited')),
                ('email_accounts', models.IntegerField(help_text='Number of email accounts, 0 = unlimited')),
                ('databases', models.IntegerField(help_text='Number of databases, 0 = unlimited')),
                ('ftp_accounts', models.IntegerField(help_text='Number of FTP accounts, 0 = unlimited')),
                ('subdomains', models.IntegerField(help_text='Number of subdomains, 0 = unlimited')),
                ('addon_domains', models.IntegerField(help_text='Number of addon domains')),
                ('parked_domains', models.IntegerField(help_text='Number of parked domains')),
                ('ssl_certificate', models.BooleanField(default=False, help_text='Free SSL certificate included')),
                ('dedicated_ip', models.BooleanField(default=False, help_text='Dedicated IP address included')),
                ('cpanel_access', models.BooleanField(default=True)),
                ('ssh_access', models.BooleanField(default=False)),
                ('cron_jobs', models.BooleanField(default=True)),
                ('backups_included', models.BooleanField(default=True, help_text='Automated backups')),
                ('monthly_price', models.DecimalField(decimal_places=2, max_digits=10)),
                ('quarterly_price', models.DecimalField(blank=True, decimal_places=2, max_digits=10, null=True)),
                ('semi_annual_price', models.DecimalField(blank=True, decimal_places=2, max_digits=10, null=True)),
                ('annual_price', models.DecimalField(blank=True, decimal_places=2, max_digits=10, null=True)),
                ('biennial_price', models.DecimalField(blank=True, decimal_places=2, max_digits=10, null=True)),
                ('triennial_price', models.DecimalField(blank=True, decimal_places=2, max_digits=10, null=True)),
                ('setup_fee_monthly', models.DecimalField(decimal_places=2, default=0.0, max_digits=10)),
                ('setup_fee_quarterly', models.DecimalField(decimal_places=2, default=0.0, max_digits=10)),
                ('setup_fee_annual', models.DecimalField(decimal_places=2, default=0.0, max_digits=10)),
                ('server_id', models.CharField(blank=True, help_text='Server identifier for provisioning', max_length=50, null=True)),
                ('cpanel_package_name', models.CharField(blank=True, help_text='cPanel package name', max_length=100, null=True)),
                ('is_featured', models.BooleanField(default=False)),
                ('sort_order', models.IntegerField(default=0)),
                ('is_active', models.BooleanField(default=True)),
                ('created_at', models.DateTimeField(auto_now_add=True)),
                ('updated_at', models.DateTimeField(auto_now=True)),
            ],
            options={
                'verbose_name': 'hosting product',
                'verbose_name_plural': 'hosting products',
                'ordering': ['sort_order', 'name'],
            },
        ),
        
        # DomainProduct model
        migrations.CreateModel(
            name='DomainProduct',
            fields=[
                ('id', models.BigAutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('tld', models.CharField(help_text='e.g., .com, .net, .co.zw', max_length=50, unique=True)),
                ('description', models.CharField(blank=True, max_length=255)),
                ('registration_price_1yr', models.DecimalField(decimal_places=2, help_text='1 year registration', max_digits=10)),
                ('registration_price_2yr', models.DecimalField(blank=True, decimal_places=2, max_digits=10, null=True)),
                ('registration_price_3yr', models.DecimalField(blank=True, decimal_places=2, max_digits=10, null=True)),
                ('registration_price_5yr', models.DecimalField(blank=True, decimal_places=2, max_digits=10, null=True)),
                ('registration_price_10yr', models.DecimalField(blank=True, decimal_places=2, max_digits=10, null=True)),
                ('renewal_price', models.DecimalField(decimal_places=2, help_text='Annual renewal price', max_digits=10)),
                ('transfer_price', models.DecimalField(decimal_places=2, help_text='Domain transfer price', max_digits=10)),
                ('redemption_price', models.DecimalField(blank=True, decimal_places=2, help_text='Grace/redemption period fee for expired domains', max_digits=10, null=True)),
                ('whois_privacy_price', models.DecimalField(decimal_places=2, default=0.0, help_text='ID Protection/WHOIS privacy annual fee', max_digits=10)),
                ('auto_renew_default', models.BooleanField(default=True, help_text='Auto-renew enabled by default')),
                ('epp_code_required', models.BooleanField(default=True, help_text='Requires EPP/auth code for transfer')),
                ('grace_period_days', models.IntegerField(default=30, help_text='Grace period after expiry')),
                ('redemption_period_days', models.IntegerField(default=30, help_text='Redemption period after grace')),
                ('min_registration_years', models.IntegerField(default=1)),
                ('max_registration_years', models.IntegerField(default=10)),
                ('is_active', models.BooleanField(default=True)),
                ('is_featured', models.BooleanField(default=False)),
                ('sort_order', models.IntegerField(default=0)),
                ('created_at', models.DateTimeField(auto_now_add=True)),
                ('updated_at', models.DateTimeField(auto_now=True)),
            ],
            options={
                'verbose_name': 'domain product',
                'verbose_name_plural': 'domain products',
                'ordering': ['sort_order', 'tld'],
            },
        ),
        
        # ServiceAddon model
        migrations.CreateModel(
            name='ServiceAddon',
            fields=[
                ('id', models.BigAutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('name', models.CharField(max_length=255)),
                ('description', models.TextField()),
                ('addon_type', models.CharField(choices=[('disk_space', 'Extra Disk Space'), ('bandwidth', 'Extra Bandwidth'), ('email_accounts', 'Additional Email Accounts'), ('databases', 'Additional Databases'), ('ip_address', 'Dedicated IP Address'), ('ssl_certificate', 'SSL Certificate'), ('backup_service', 'Backup Service'), ('cdn', 'CDN Service'), ('other', 'Other')], max_length=50)),
                ('billing_type', models.CharField(choices=[('one_time', 'One Time Fee'), ('recurring', 'Recurring Fee'), ('free', 'Free')], default='recurring', max_length=20)),
                ('monthly_price', models.DecimalField(decimal_places=2, default=0.0, max_digits=10)),
                ('quarterly_price', models.DecimalField(blank=True, decimal_places=2, max_digits=10, null=True)),
                ('annual_price', models.DecimalField(blank=True, decimal_places=2, max_digits=10, null=True)),
                ('one_time_price', models.DecimalField(blank=True, decimal_places=2, max_digits=10, null=True)),
                ('quantity', models.IntegerField(default=1, help_text='Amount to add (e.g., 5000 MB disk, 1 IP)')),
                ('is_active', models.BooleanField(default=True)),
                ('requires_hosting', models.BooleanField(default=True, help_text='Requires active hosting service')),
                ('created_at', models.DateTimeField(auto_now_add=True)),
                ('updated_at', models.DateTimeField(auto_now=True)),
                ('compatible_products', models.ManyToManyField(blank=True, help_text='Compatible hosting products (empty = all)', to='services.HostingProduct')),
            ],
            options={
                'verbose_name': 'service addon',
                'verbose_name_plural': 'service addons',
                'ordering': ['name'],
            },
        ),
        
        # DomainRegistration model
        migrations.CreateModel(
            name='DomainRegistration',
            fields=[
                ('id', models.BigAutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('domain_name', models.CharField(help_text='Full domain name (e.g., example.com)', max_length=255)),
                ('registration_date', models.DateField()),
                ('expiry_date', models.DateField()),
                ('registration_years', models.IntegerField(default=1)),
                ('status', models.CharField(choices=[('active', 'Active'), ('pending', 'Pending Registration'), ('pending_transfer', 'Pending Transfer'), ('expired', 'Expired'), ('cancelled', 'Cancelled'), ('transferred_away', 'Transferred Away'), ('fraud', 'Fraud'), ('pending_delete', 'Pending Delete'), ('redemption', 'Redemption Period')], default='pending', max_length=20)),
                ('auto_renew', models.BooleanField(default=True, help_text='Automatically renew before expiry')),
                ('whois_privacy', models.BooleanField(default=False, help_text='ID Protection/WHOIS privacy enabled')),
                ('whois_privacy_expiry', models.DateField(blank=True, null=True)),
                ('epp_code', models.CharField(blank=True, help_text='EPP/Authorization code', max_length=100, null=True)),
                ('is_transfer', models.BooleanField(default=False, help_text='Was this a transfer-in?')),
                ('nameserver1', models.CharField(blank=True, default='ns1.yourdomain.com', max_length=255)),
                ('nameserver2', models.CharField(blank=True, default='ns2.yourdomain.com', max_length=255)),
                ('nameserver3', models.CharField(blank=True, max_length=255, null=True)),
                ('nameserver4', models.CharField(blank=True, max_length=255, null=True)),
                ('nameserver5', models.CharField(blank=True, max_length=255, null=True)),
                ('renewal_reminder_30d', models.BooleanField(default=False)),
                ('renewal_reminder_14d', models.BooleanField(default=False)),
                ('renewal_reminder_7d', models.BooleanField(default=False)),
                ('renewal_reminder_1d', models.BooleanField(default=False)),
                ('registrar_id', models.CharField(blank=True, help_text='Domain ID at registrar', max_length=100, null=True)),
                ('notes', models.TextField(blank=True, null=True)),
                ('created_at', models.DateTimeField(auto_now_add=True)),
                ('updated_at', models.DateTimeField(auto_now=True)),
                ('client', models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, related_name='domains', to='clients.client')),
                ('domain_product', models.ForeignKey(on_delete=django.db.models.deletion.PROTECT, related_name='registrations', to='services.domainproduct')),
            ],
            options={
                'verbose_name': 'domain registration',
                'verbose_name_plural': 'domain registrations',
                'ordering': ['-expiry_date'],
                'unique_together': {('domain_name', 'client')},
            },
        ),
        
        # Update ServiceSubscription with WHMCS fields
        migrations.AddField(
            model_name='servicesubscription',
            name='hosting_product',
            field=models.ForeignKey(blank=True, help_text='Hosting package if service is web hosting', null=True, on_delete=django.db.models.deletion.SET_NULL, related_name='subscriptions', to='services.hostingproduct'),
        ),
        migrations.AddField(
            model_name='servicesubscription',
            name='next_due_date',
            field=models.DateField(blank=True, help_text='Next invoice due date', null=True),
        ),
        migrations.AddField(
            model_name='servicesubscription',
            name='next_invoice_date',
            field=models.DateField(blank=True, help_text='When to generate next invoice', null=True),
        ),
        migrations.AddField(
            model_name='servicesubscription',
            name='renewal_reminder_30d',
            field=models.BooleanField(default=False),
        ),
        migrations.AddField(
            model_name='servicesubscription',
            name='renewal_reminder_14d',
            field=models.BooleanField(default=False),
        ),
        migrations.AddField(
            model_name='servicesubscription',
            name='renewal_reminder_7d',
            field=models.BooleanField(default=False),
        ),
        migrations.AddField(
            model_name='servicesubscription',
            name='renewal_reminder_1d',
            field=models.BooleanField(default=False),
        ),
        migrations.AddField(
            model_name='servicesubscription',
            name='grace_period_days',
            field=models.IntegerField(default=14, help_text='Days after due date before suspension'),
        ),
        migrations.AddField(
            model_name='servicesubscription',
            name='suspension_date',
            field=models.DateField(blank=True, help_text='Date service was suspended', null=True),
        ),
        migrations.AddField(
            model_name='servicesubscription',
            name='termination_date',
            field=models.DateField(blank=True, help_text='Date service was terminated', null=True),
        ),
        migrations.AlterField(
            model_name='servicesubscription',
            name='billing_cycle',
            field=models.CharField(choices=[('monthly', 'Monthly'), ('quarterly', 'Quarterly'), ('semi_annual', 'Semi-Annual'), ('annual', 'Annual'), ('biennial', 'Biennial (2 years)'), ('triennial', 'Triennial (3 years)'), ('one_time', 'One Time')], default='monthly', max_length=20),
        ),
        migrations.AlterField(
            model_name='servicesubscription',
            name='status',
            field=models.CharField(choices=[('active', 'Active'), ('pending', 'Pending'), ('provisioning', 'Provisioning'), ('suspended', 'Suspended'), ('cancelled', 'Cancelled'), ('expired', 'Expired'), ('terminated', 'Terminated')], default='pending', max_length=20),
        ),
    ]
