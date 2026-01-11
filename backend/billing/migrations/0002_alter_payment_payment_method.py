# Generated manually for SDK integration

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('billing', '0001_initial'),
    ]

    operations = [
        migrations.AlterField(
            model_name='payment',
            name='payment_method',
            field=models.CharField(choices=[('cash', 'Cash'), ('bank_transfer', 'Bank Transfer'), ('credit_card', 'Credit Card'), ('paypal', 'PayPal'), ('stripe', 'Stripe'), ('paynow', 'Paynow'), ('ecocash', 'EcoCash'), ('onemoney', 'OneMoney'), ('mobile_money', 'Mobile Money'), ('other', 'Other')], max_length=20),
        ),
    ]
