# Generated manually
from django.conf import settings
from django.db import migrations, models
import django.db.models.deletion
import django.utils.timezone


class Migration(migrations.Migration):

    initial = True

    dependencies = [
        migrations.swappable_dependency(settings.AUTH_USER_MODEL),
    ]

    operations = [
        migrations.CreateModel(
            name='InvestmentPackage',
            fields=[
                ('id', models.BigAutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('name', models.CharField(max_length=255)),
                ('description', models.TextField()),
                ('minimum_amount', models.DecimalField(decimal_places=2, max_digits=12)),
                ('expected_return', models.DecimalField(decimal_places=2, help_text='Expected return percentage', max_digits=5)),
                ('duration_months', models.IntegerField(help_text='Investment duration in months')),
                ('is_active', models.BooleanField(default=True)),
                ('created_at', models.DateTimeField(auto_now_add=True)),
                ('updated_at', models.DateTimeField(auto_now=True)),
            ],
            options={
                'verbose_name': 'Investment Package',
                'verbose_name_plural': 'Investment Packages',
                'ordering': ['minimum_amount'],
            },
        ),
        migrations.CreateModel(
            name='Investment',
            fields=[
                ('id', models.BigAutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('amount', models.DecimalField(decimal_places=2, max_digits=12)),
                ('start_date', models.DateTimeField(default=django.utils.timezone.now)),
                ('maturity_date', models.DateTimeField()),
                ('status', models.CharField(choices=[('active', 'Active'), ('matured', 'Matured'), ('withdrawn', 'Withdrawn')], default='active', max_length=20)),
                ('current_value', models.DecimalField(decimal_places=2, max_digits=12)),
                ('created_at', models.DateTimeField(auto_now_add=True)),
                ('updated_at', models.DateTimeField(auto_now=True)),
                ('investor', models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, related_name='investments', to=settings.AUTH_USER_MODEL)),
                ('package', models.ForeignKey(on_delete=django.db.models.deletion.PROTECT, related_name='investments', to='investments.investmentpackage')),
            ],
            options={
                'verbose_name': 'Investment',
                'verbose_name_plural': 'Investments',
                'ordering': ['-start_date'],
            },
        ),
        migrations.CreateModel(
            name='BankingDetails',
            fields=[
                ('id', models.BigAutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('bank_name', models.CharField(max_length=255)),
                ('account_number', models.CharField(max_length=50)),
                ('account_holder_name', models.CharField(max_length=255)),
                ('swift_code', models.CharField(blank=True, max_length=20, null=True)),
                ('branch_code', models.CharField(blank=True, max_length=20, null=True)),
                ('is_primary', models.BooleanField(default=True)),
                ('created_at', models.DateTimeField(auto_now_add=True)),
                ('updated_at', models.DateTimeField(auto_now=True)),
                ('investor', models.ForeignKey(on_delete=django.db.models.deletion.CASCADE, related_name='banking_details', to=settings.AUTH_USER_MODEL)),
            ],
            options={
                'verbose_name': 'Banking Details',
                'verbose_name_plural': 'Banking Details',
                'ordering': ['-is_primary', '-created_at'],
            },
        ),
    ]
