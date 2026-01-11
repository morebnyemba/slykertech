# Generated manually for SDK integration

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('integrations', '0003_apiconfiguration'),
    ]

    operations = [
        migrations.AlterField(
            model_name='apiconfiguration',
            name='provider',
            field=models.CharField(choices=[('whatsapp', 'WhatsApp Business API'), ('namecheap', 'Namecheap'), ('cloudflare', 'Cloudflare'), ('stripe', 'Stripe'), ('paypal', 'PayPal'), ('paynow', 'Paynow'), ('other', 'Other')], max_length=50, unique=True),
        ),
    ]
