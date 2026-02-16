"""
Management command to clean up old database backups.

Usage:
    python manage.py cleanup_old_backups
    python manage.py cleanup_old_backups --days 7
"""
import os
import time
from datetime import datetime, timedelta

from django.core.management.base import BaseCommand
from django.conf import settings

from accounts.models import BackupLog


class Command(BaseCommand):
    help = 'Delete database backups older than the specified retention period'

    def add_arguments(self, parser):
        parser.add_argument(
            '--days',
            type=int,
            default=None,
            help='Number of days to retain backups (default: BACKUP_RETENTION_DAYS env var or 30)',
        )
        parser.add_argument(
            '--dry-run',
            action='store_true',
            help='Show which files would be deleted without actually deleting them',
        )

    def handle(self, *args, **options):
        retention_days = options['days'] or int(os.environ.get('BACKUP_RETENTION_DAYS', 30))
        dry_run = options['dry_run']
        backup_dir = os.environ.get('BACKUP_DIR', os.path.join(settings.BASE_DIR, 'backups'))

        if not os.path.exists(backup_dir):
            self.stdout.write(self.style.WARNING(f'Backup directory does not exist: {backup_dir}'))
            return

        cutoff_time = time.time() - (retention_days * 86400)
        deleted_count = 0
        freed_bytes = 0

        for filename in os.listdir(backup_dir):
            filepath = os.path.join(backup_dir, filename)
            if not os.path.isfile(filepath):
                continue

            # Only process backup files
            if not filename.startswith('slykertech_backup_'):
                continue

            file_mtime = os.path.getmtime(filepath)
            if file_mtime < cutoff_time:
                file_size = os.path.getsize(filepath)
                if dry_run:
                    self.stdout.write(f'Would delete: {filename} ({file_size / (1024 * 1024):.1f} MB)')
                else:
                    os.remove(filepath)
                    self.stdout.write(f'Deleted: {filename} ({file_size / (1024 * 1024):.1f} MB)')
                deleted_count += 1
                freed_bytes += file_size

        # Clean up old BackupLog entries too
        cutoff_date = datetime.now() - timedelta(days=retention_days)
        if not dry_run:
            old_logs = BackupLog.objects.filter(created_at__lt=cutoff_date)
            log_count = old_logs.count()
            old_logs.delete()
        else:
            log_count = BackupLog.objects.filter(created_at__lt=cutoff_date).count()

        action = 'Would delete' if dry_run else 'Deleted'
        self.stdout.write(self.style.SUCCESS(
            f'{action} {deleted_count} backup file(s), freeing {freed_bytes / (1024 * 1024):.1f} MB. '
            f'{action} {log_count} old log entries. Retention: {retention_days} days.'
        ))
