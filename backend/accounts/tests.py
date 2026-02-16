"""
Tests for accounts app - audit models, backup management, and tracking
"""
import os
import tempfile
from datetime import date, timedelta
from unittest.mock import patch

from django.contrib.contenttypes.models import ContentType
from django.test import TestCase

from accounts.models import (
    User, AuditMixin, StatusChangeLog, ActivityLog, BackupLog,
)


class StatusChangeLogModelTest(TestCase):
    """Test StatusChangeLog model"""

    def setUp(self):
        self.user = User.objects.create_user(
            email='admin@example.com',
            password='testpass123',
            user_type='admin',
        )

    def test_create_status_change_log(self):
        ct = ContentType.objects.get_for_model(User)
        log = StatusChangeLog.objects.create(
            content_type=ct,
            object_id=self.user.pk,
            old_status='active',
            new_status='suspended',
            changed_by=self.user,
            reason='Test suspension',
        )
        self.assertEqual(log.old_status, 'active')
        self.assertEqual(log.new_status, 'suspended')
        self.assertEqual(log.changed_by, self.user)
        self.assertIn('→', str(log))

    def test_status_change_log_without_user(self):
        ct = ContentType.objects.get_for_model(User)
        log = StatusChangeLog.objects.create(
            content_type=ct,
            object_id=self.user.pk,
            old_status='pending',
            new_status='active',
            reason='Auto-activated',
        )
        self.assertIsNone(log.changed_by)

    def test_status_change_log_with_metadata(self):
        ct = ContentType.objects.get_for_model(User)
        log = StatusChangeLog.objects.create(
            content_type=ct,
            object_id=self.user.pk,
            old_status='active',
            new_status='suspended',
            metadata={'days_overdue': 15, 'auto': True},
        )
        self.assertEqual(log.metadata['days_overdue'], 15)


class ActivityLogModelTest(TestCase):
    """Test ActivityLog model"""

    def setUp(self):
        self.user = User.objects.create_user(
            email='staff@example.com',
            password='testpass123',
            user_type='staff',
        )

    def test_create_activity_log(self):
        log = ActivityLog.objects.create(
            user=self.user,
            action='create',
            description='Created a new service subscription',
        )
        self.assertEqual(log.action, 'create')
        self.assertIn('staff@example.com', str(log))

    def test_activity_log_with_target(self):
        ct = ContentType.objects.get_for_model(User)
        log = ActivityLog.objects.create(
            user=self.user,
            action='update',
            description='Updated user profile',
            content_type=ct,
            object_id=self.user.pk,
            ip_address='127.0.0.1',
        )
        self.assertEqual(log.content_type, ct)
        self.assertEqual(log.ip_address, '127.0.0.1')

    def test_system_activity_log(self):
        """Test activity log without a user (system action)"""
        log = ActivityLog.objects.create(
            action='backup',
            description='Scheduled database backup completed',
        )
        self.assertIsNone(log.user)
        self.assertIn('System', str(log))


class BackupLogModelTest(TestCase):
    """Test BackupLog model"""

    def test_create_backup_log(self):
        log = BackupLog.objects.create(
            filename='slykertech_backup_20260216_020000.sql.gz',
            database_name='slykertech',
            status='completed',
            file_size=1024 * 1024 * 50,  # 50MB
            duration_seconds=12.5,
        )
        self.assertEqual(log.status, 'completed')
        self.assertIn('Completed', str(log))

    def test_failed_backup_log(self):
        log = BackupLog.objects.create(
            filename='slykertech_backup_failed.sql.gz',
            status='failed',
            error_message='pg_dump not found',
        )
        self.assertEqual(log.status, 'failed')
        self.assertEqual(log.error_message, 'pg_dump not found')

    def test_backup_log_ordering(self):
        """Backup logs should be ordered by most recent first"""
        BackupLog.objects.create(filename='backup1.sql.gz', status='completed')
        BackupLog.objects.create(filename='backup2.sql.gz', status='completed')
        logs = BackupLog.objects.all()
        self.assertEqual(logs[0].filename, 'backup2.sql.gz')


class CleanupOldBackupsCommandTest(TestCase):
    """Test the cleanup_old_backups management command"""

    def test_cleanup_dry_run(self):
        """Dry run should not delete anything"""
        from django.core.management import call_command
        from io import StringIO

        BackupLog.objects.create(
            filename='old_backup.sql.gz',
            status='completed',
        )

        # Create a temporary backup directory for the test
        with tempfile.TemporaryDirectory() as tmpdir:
            with patch.dict(os.environ, {'BACKUP_DIR': tmpdir}):
                out = StringIO()
                call_command('cleanup_old_backups', '--dry-run', '--days=0', stdout=out)
                output = out.getvalue()
                self.assertIn('Would delete', output)

    def test_cleanup_with_retention(self):
        """Old log entries should be cleaned up"""
        from django.core.management import call_command
        from io import StringIO

        # Create a log entry (will be within retention period)
        BackupLog.objects.create(
            filename='recent_backup.sql.gz',
            status='completed',
        )

        with tempfile.TemporaryDirectory() as tmpdir:
            with patch.dict(os.environ, {'BACKUP_DIR': tmpdir}):
                out = StringIO()
                call_command('cleanup_old_backups', '--days=30', stdout=out)
                # Recent entry should still exist
                self.assertEqual(BackupLog.objects.count(), 1)

