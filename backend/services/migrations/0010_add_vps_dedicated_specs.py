# Generated migration to add VPS and Dedicated server specifications

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('services', '0009_populate_domain_products'),
    ]

    operations = [
        migrations.AddField(
            model_name='hostingproduct',
            name='cpu_cores',
            field=models.IntegerField(
                default=0,
                help_text='Number of CPU cores (for VPS/Dedicated), 0 for shared hosting'
            ),
        ),
        migrations.AddField(
            model_name='hostingproduct',
            name='ram_gb',
            field=models.IntegerField(
                default=0,
                help_text='RAM in GB (for VPS/Dedicated), 0 for shared hosting'
            ),
        ),
        migrations.AddField(
            model_name='hostingproduct',
            name='cpu_type',
            field=models.CharField(
                max_length=200,
                blank=True,
                null=True,
                help_text='CPU type/model (for Dedicated servers)'
            ),
        ),
        migrations.AddField(
            model_name='hostingproduct',
            name='storage_type',
            field=models.CharField(
                max_length=50,
                default='SSD',
                choices=[
                    ('HDD', 'HDD'),
                    ('SSD', 'SSD'),
                    ('NVMe', 'NVMe SSD'),
                ],
                help_text='Storage type'
            ),
        ),
    ]
