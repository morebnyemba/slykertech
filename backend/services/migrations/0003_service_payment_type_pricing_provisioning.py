# Generated migration for configurable pricing and provisioning

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('services', '0002_projectmilestone_projecttracker_projecttask_and_more'),
    ]

    operations = [
        migrations.AddField(
            model_name='service',
            name='payment_type',
            field=models.CharField(
                choices=[('one_time', 'One Time'), ('recurring', 'Recurring'), ('both', 'Both Options Available')],
                default='one_time',
                help_text='Payment structure for this service',
                max_length=20
            ),
        ),
        migrations.AddField(
            model_name='service',
            name='pricing_options',
            field=models.JSONField(blank=True, default=dict, help_text='Different pricing tiers/options for this service'),
        ),
        migrations.AddField(
            model_name='service',
            name='requires_provisioning',
            field=models.BooleanField(default=False, help_text='Requires automatic provisioning (e.g., cPanel account)'),
        ),
        migrations.AddField(
            model_name='service',
            name='provisioning_type',
            field=models.CharField(blank=True, help_text='Type of provisioning needed: cpanel, directadmin, domain, etc.', max_length=50, null=True),
        ),
        migrations.AddField(
            model_name='servicesubscription',
            name='provisioning_completed',
            field=models.BooleanField(default=False, help_text='Has automated provisioning completed?'),
        ),
        migrations.AddField(
            model_name='servicesubscription',
            name='provisioning_error',
            field=models.TextField(blank=True, help_text='Error message if provisioning failed', null=True),
        ),
        migrations.AlterField(
            model_name='servicesubscription',
            name='status',
            field=models.CharField(
                choices=[
                    ('active', 'Active'),
                    ('pending', 'Pending'),
                    ('provisioning', 'Provisioning'),
                    ('suspended', 'Suspended'),
                    ('cancelled', 'Cancelled'),
                    ('expired', 'Expired')
                ],
                default='pending',
                max_length=20
            ),
        ),
    ]
