"""
Management command to create a PostgreSQL database backup.

Usage:
    python manage.py dbbackup
    python manage.py dbbackup --output /custom/path/backup.sql.gz
"""
import os
import time
import subprocess
from datetime import datetime

from django.core.management.base import BaseCommand
from django.conf import settings

from accounts.models import BackupLog


class Command(BaseCommand):
    help = 'Create a compressed PostgreSQL database backup'

    def add_arguments(self, parser):
        parser.add_argument(
            '--output',
            type=str,
            help='Custom output path for the backup file',
        )

    def handle(self, *args, **options):
        db_settings = settings.DATABASES['default']
        db_name = db_settings.get('NAME', 'slykertech')
        db_user = db_settings.get('USER', 'slykertech')
        db_host = db_settings.get('HOST', 'db')
        db_port = db_settings.get('PORT', '5432')
        db_password = db_settings.get('PASSWORD', '')

        backup_dir = os.environ.get('BACKUP_DIR', os.path.join(settings.BASE_DIR, 'backups'))
        os.makedirs(backup_dir, exist_ok=True)

        timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
        filename = f'slykertech_backup_{timestamp}.sql.gz'
        filepath = options.get('output') or os.path.join(backup_dir, filename)

        # Create a BackupLog entry
        log_entry = BackupLog.objects.create(
            filename=filename,
            database_name=db_name,
            status='started',
        )

        start_time = time.time()

        try:
            env = os.environ.copy()
            if db_password:
                env['PGPASSWORD'] = db_password

            cmd = [
                'pg_dump',
                '-h', db_host,
                '-p', str(db_port),
                '-U', db_user,
                '-F', 'c',  # custom format (compressed)
                db_name,
            ]

            with open(filepath, 'wb') as f:
                result = subprocess.run(
                    cmd,
                    stdout=f,
                    stderr=subprocess.PIPE,
                    env=env,
                    timeout=3600,  # 1 hour timeout
                )

            duration = time.time() - start_time

            if result.returncode != 0:
                error_msg = result.stderr.decode('utf-8', errors='replace')
                log_entry.status = 'failed'
                log_entry.error_message = error_msg
                log_entry.duration_seconds = duration
                log_entry.save()
                self.stderr.write(self.style.ERROR(f'Backup failed: {error_msg}'))
                return

            file_size = os.path.getsize(filepath)
            log_entry.status = 'completed'
            log_entry.file_size = file_size
            log_entry.duration_seconds = duration
            log_entry.save()

            self.stdout.write(self.style.SUCCESS(
                f'Backup completed: {filepath} ({file_size / (1024 * 1024):.1f} MB in {duration:.1f}s)'
            ))

        except FileNotFoundError:
            duration = time.time() - start_time
            log_entry.status = 'failed'
            log_entry.error_message = 'pg_dump not found. Ensure PostgreSQL client is installed.'
            log_entry.duration_seconds = duration
            log_entry.save()
            self.stderr.write(self.style.ERROR(
                'pg_dump not found. Ensure PostgreSQL client is installed.'
            ))

        except subprocess.TimeoutExpired:
            duration = time.time() - start_time
            log_entry.status = 'failed'
            log_entry.error_message = 'Backup timed out after 1 hour.'
            log_entry.duration_seconds = duration
            log_entry.save()
            self.stderr.write(self.style.ERROR('Backup timed out after 1 hour.'))

        except Exception as e:
            duration = time.time() - start_time
            log_entry.status = 'failed'
            log_entry.error_message = str(e)
            log_entry.duration_seconds = duration
            log_entry.save()
            self.stderr.write(self.style.ERROR(f'Backup error: {e}'))
