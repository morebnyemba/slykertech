# Generated migration to add VPS and Dedicated server specifications

from django.db import migrations, models


def add_vps_dedicated_fields(apps, schema_editor):
    """Add VPS/Dedicated fields only if they don't already exist."""
    connection = schema_editor.connection
    vendor = connection.vendor

    # Get existing columns in a database-agnostic way
    if vendor == 'sqlite':
        with connection.cursor() as cursor:
            cursor.execute("PRAGMA table_info('services_hostingproduct');")
            existing_columns = {row[1] for row in cursor.fetchall()}
    else:
        with connection.cursor() as cursor:
            cursor.execute(
                "SELECT column_name FROM information_schema.columns "
                "WHERE table_name = 'services_hostingproduct';"
            )
            existing_columns = {row[0] for row in cursor.fetchall()}

    fields_to_add = {
        'cpu_cores': "integer NOT NULL DEFAULT 0",
        'ram_gb': "integer NOT NULL DEFAULT 0",
        'cpu_type': "varchar(200) NULL",
        'storage_type': "varchar(50) NOT NULL DEFAULT 'SSD'",
    }

    with connection.cursor() as cursor:
        for col_name, col_def in fields_to_add.items():
            if col_name not in existing_columns:
                cursor.execute(
                    f'ALTER TABLE "services_hostingproduct" ADD COLUMN "{col_name}" {col_def};'
                )


class Migration(migrations.Migration):

    dependencies = [
        ('services', '0009_populate_domain_products'),
    ]

    operations = [
        migrations.SeparateDatabaseAndState(
            state_operations=[
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
            ],
            database_operations=[
                migrations.RunPython(
                    add_vps_dedicated_fields,
                    migrations.RunPython.noop,
                ),
            ],
        ),
    ]
