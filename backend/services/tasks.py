from celery import shared_task
from django.utils import timezone
from datetime import timedelta
from .models import ServiceSubscription
from notifications.services import WhatsAppService

@shared_task
def send_renewal_reminders(days_before):
    """Send renewal reminders for subscriptions expiring in X days"""
    target_date = timezone.now().date() + timedelta(days=days_before)
    
    subscriptions = ServiceSubscription.objects.filter(
        end_date=target_date,
        status='active',
        auto_renew=False
    )
    
    whatsapp = WhatsAppService()
    
    for sub in subscriptions:
        message = f"Your {sub.service.name} subscription expires in {days_before} days. Renew now to avoid service interruption."
        try:
            whatsapp.send_renewal_reminder(
                to_number=sub.client.mobile_number,
                subscription_id=sub.id,
                days_remaining=days_before
            )
        except Exception as e:
            print(f"Failed to send reminder for subscription {sub.id}: {e}")
    
    return f"Sent {subscriptions.count()} renewal reminders for {days_before} days"

@shared_task
def check_suspensions():
    """Check for overdue subscriptions and suspend them"""
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
            sub.status = 'suspended'
            sub.suspension_date = today
            sub.save()
            suspended_count += 1
            
            # Suspend cPanel account if applicable
            if hasattr(sub, 'cpanel_accounts'):
                for cpanel in sub.cpanel_accounts.all():
                    # Call cPanel API to suspend account
                    pass
    
    return f"Suspended {suspended_count} overdue subscriptions"

@shared_task
def sync_cpanel_usage():
    """Sync disk and bandwidth usage from cPanel"""
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
                    print(f"Failed to sync usage for {cpanel.cpanel_username}: {e}")
    
    return f"Synced usage for {synced_count} cPanel accounts"
