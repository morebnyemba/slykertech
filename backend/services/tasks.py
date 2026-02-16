from celery import shared_task
from django.utils import timezone
from datetime import timedelta
from .models import ServiceSubscription
from notifications.services import WhatsAppService
import logging

logger = logging.getLogger(__name__)

@shared_task(bind=True, max_retries=3, default_retry_delay=60)
def send_renewal_reminders(self, days_before):
    """Send renewal reminders for subscriptions expiring in X days"""
    target_date = timezone.now().date() + timedelta(days=days_before)
    
    subscriptions = ServiceSubscription.objects.filter(
        end_date=target_date,
        status='active',
        auto_renew=False
    )
    
    whatsapp = WhatsAppService()
    
    for sub in subscriptions:
        try:
            whatsapp.send_renewal_reminder(
                to_number=sub.client.mobile_number,
                subscription_id=sub.id,
                days_remaining=days_before
            )
        except Exception as e:
            logger.warning(f"Failed to send reminder for subscription {sub.id}: {e}")
    
    return f"Sent {subscriptions.count()} renewal reminders for {days_before} days"

@shared_task(bind=True, max_retries=3, default_retry_delay=120)
def check_suspensions(self):
    """Check for overdue subscriptions and suspend them"""
    try:
        today = timezone.now().date()
        
        # Find expired subscriptions still active
        expired_subs = ServiceSubscription.objects.filter(
            end_date__lt=today,
            status='active'
        )
        
        suspended_count = 0
        for sub in expired_subs:
            days_overdue = (today - sub.end_date).days
            grace_period = getattr(sub, 'grace_period_days', 14)
            
            if days_overdue > grace_period:
                old_status = sub.status
                sub.status = 'suspended'
                sub.suspension_date = today
                sub.save()
                suspended_count += 1
                
                # Log the status change
                _log_status_change(sub, old_status, 'suspended', reason='Auto-suspended: overdue payment')
                
                # Suspend cPanel account if applicable
                if hasattr(sub, 'cpanel_accounts'):
                    for cpanel in sub.cpanel_accounts.all():
                        # Call cPanel API to suspend account
                        pass
        
        return f"Suspended {suspended_count} overdue subscriptions"
    except Exception as exc:
        logger.error(f"Error in check_suspensions: {exc}")
        raise self.retry(exc=exc)

@shared_task(bind=True, max_retries=3, default_retry_delay=60)
def sync_cpanel_usage(self):
    """Sync disk and bandwidth usage from cPanel"""
    try:
        from integrations.cpanel import cPanelAPIClient
        
        subscriptions = ServiceSubscription.objects.filter(
            status='active',
            service__provisioning_type='cpanel'
        )
        
        synced_count = 0
        for sub in subscriptions:
            if hasattr(sub, 'cpanel_accounts'):
                for cpanel in sub.cpanel_accounts.all():
                    try:
                        client = cPanelAPIClient()
                        usage = client.get_account_usage(cpanel.cpanel_username)
                        
                        # Update usage in metadata
                        sub.metadata['disk_used'] = usage.get('disk_used', 0)
                        sub.metadata['bandwidth_used'] = usage.get('bandwidth_used', 0)
                        sub.save()
                        synced_count += 1
                    except Exception as e:
                        logger.warning(f"Failed to sync usage for {cpanel.cpanel_username}: {e}")
        
        return f"Synced usage for {synced_count} cPanel accounts"
    except Exception as exc:
        logger.error(f"Error in sync_cpanel_usage: {exc}")
        raise self.retry(exc=exc)


@shared_task
def run_database_backup():
    """Run a scheduled database backup via management command."""
    from django.core.management import call_command
    try:
        call_command('dbbackup')
        logger.info("Scheduled database backup completed successfully")
        return "Database backup completed successfully"
    except Exception as e:
        logger.error(f"Scheduled database backup failed: {e}")
        return f"Database backup failed: {e}"


@shared_task
def cleanup_old_backups():
    """Clean up old database backups based on retention policy."""
    from django.core.management import call_command
    try:
        call_command('cleanup_old_backups')
        logger.info("Old backup cleanup completed successfully")
        return "Backup cleanup completed successfully"
    except Exception as e:
        logger.error(f"Backup cleanup failed: {e}")
        return f"Backup cleanup failed: {e}"


@shared_task
def check_service_health():
    """Check health of all critical services and log any issues."""
    from django.db import connection
    results = {}

    # Check database
    try:
        connection.ensure_connection()
        results['database'] = 'healthy'
    except Exception as e:
        results['database'] = f'unhealthy: {e}'
        logger.error(f"Health check - Database unhealthy: {e}")

    # Check Redis
    try:
        import redis as redis_lib
        import os
        redis_host = os.environ.get('REDIS_HOST', 'redis')
        redis_port = int(os.environ.get('REDIS_PORT', 6379))
        r = redis_lib.Redis(host=redis_host, port=redis_port, socket_timeout=5)
        r.ping()
        results['redis'] = 'healthy'
    except Exception as e:
        results['redis'] = f'unhealthy: {e}'
        logger.error(f"Health check - Redis unhealthy: {e}")

    # Check for pending provisioning failures
    from services.models import ProvisioningFailure
    pending_failures = ProvisioningFailure.objects.filter(status='pending').count()
    results['pending_provisioning_failures'] = pending_failures
    if pending_failures > 0:
        logger.warning(f"Health check - {pending_failures} pending provisioning failure(s)")

    # Check for overdue subscriptions
    overdue_count = ServiceSubscription.objects.filter(
        end_date__lt=timezone.now().date(),
        status='active'
    ).count()
    results['overdue_subscriptions'] = overdue_count

    logger.info(f"Health check completed: {results}")
    return results


def _log_status_change(instance, old_status, new_status, reason='', user=None):
    """Helper to log a status change on any model."""
    try:
        from django.contrib.contenttypes.models import ContentType
        from accounts.models import StatusChangeLog
        ct = ContentType.objects.get_for_model(instance)
        StatusChangeLog.objects.create(
            content_type=ct,
            object_id=instance.pk,
            old_status=old_status,
            new_status=new_status,
            changed_by=user,
            reason=reason,
        )
    except Exception as e:
        logger.warning(f"Failed to log status change: {e}")
