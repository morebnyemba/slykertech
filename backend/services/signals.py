"""
Django signals for automatic provisioning with fallback admin notification
"""
from django.db.models.signals import post_save
from django.dispatch import receiver
from django.utils import timezone
from django.contrib.auth import get_user_model
from services.models import ServiceSubscription, ProvisioningFailure
from services.provisioning import provisioning_service
import logging

logger = logging.getLogger(__name__)
User = get_user_model()


def notify_admins_of_failure(failure):
    """
    Send notifications to all admin/staff users about a provisioning failure
    """
    from notifications.models import Notification
    
    # Get all staff users
    admin_users = User.objects.filter(is_staff=True, is_active=True)
    
    subscription = failure.subscription
    client = subscription.client
    
    for admin_user in admin_users:
        Notification.objects.create(
            user=admin_user,
            notification_type='in_app',
            category='system',
            status='sent',
            title=f'Provisioning Failed: {subscription.service.name}',
            message=(
                f'Automatic provisioning failed for client {client.company_name}.\n'
                f'Service: {subscription.service.name}\n'
                f'Error: {failure.error_message}\n\n'
                f'Manual intervention may be required.'
            ),
            link_url='/admin/provisioning-failures',
            link_text='View Failure Details',
            metadata={
                'failure_id': failure.id,
                'subscription_id': subscription.id,
                'client_id': client.id,
                'provisioning_type': subscription.service.provisioning_type,
            }
        )
    
    # Mark notification as sent
    failure.admin_notified = True
    failure.notification_sent_at = timezone.now()
    failure.save(update_fields=['admin_notified', 'notification_sent_at'])
    
    logger.info(f"Notified {admin_users.count()} admins about provisioning failure {failure.id}")


def create_provisioning_failure_record(subscription, error_message):
    """
    Create a provisioning failure record with all data needed for manual provisioning
    """
    client = subscription.client
    
    # Collect all data admin would need for manual provisioning
    provisioning_data = {
        'client_info': {
            'company_name': client.company_name,
            'email': client.user.email if client.user else '',
            'id': client.id,
        },
        'service_info': {
            'name': subscription.service.name,
            'provisioning_type': subscription.service.provisioning_type,
        },
        'subscription_metadata': subscription.metadata,
    }
    
    # Add provisioning-type-specific suggested values
    if subscription.service.provisioning_type == 'cpanel':
        suggested_username = subscription.metadata.get('cpanel_username') or \
            f"{client.company_name[:8].lower().replace(' ', '')}{client.id}"
        suggested_domain = subscription.metadata.get('domain') or \
            f"{client.company_name.lower().replace(' ', '')}.yourdomain.com"
        
        provisioning_data['suggested_values'] = {
            'username': suggested_username,
            'domain': suggested_domain,
            'disk_quota': subscription.metadata.get('disk_quota', 1024),
            'bandwidth_quota': subscription.metadata.get('bandwidth_quota', 10240),
            'plan': subscription.metadata.get('plan', 'default'),
        }
    elif subscription.service.provisioning_type == 'directadmin':
        suggested_username = subscription.metadata.get('da_username') or \
            f"{client.company_name[:8].lower().replace(' ', '')}{client.id}"
        suggested_domain = subscription.metadata.get('domain') or \
            f"{client.company_name.lower().replace(' ', '')}.yourdomain.com"
        
        provisioning_data['suggested_values'] = {
            'username': suggested_username,
            'domain': suggested_domain,
            'disk_quota': subscription.metadata.get('disk_quota', 1024),
            'bandwidth_quota': subscription.metadata.get('bandwidth_quota', 10240),
        }
    elif subscription.service.provisioning_type == 'domain':
        provisioning_data['suggested_values'] = {
            'domain': subscription.metadata.get('domain', ''),
            'years': subscription.metadata.get('years', 1),
        }
    
    failure = ProvisioningFailure.objects.create(
        subscription=subscription,
        error_message=error_message,
        error_details={
            'provisioning_type': subscription.service.provisioning_type,
            'attempted_at': timezone.now().isoformat(),
        },
        provisioning_data=provisioning_data,
    )
    
    return failure


@receiver(post_save, sender=ServiceSubscription)
def auto_provision_subscription(sender, instance, created, **kwargs):
    """
    Automatically provision resources when a subscription is created or activated.
    On failure, creates a failure record and notifies admins for manual intervention.
    """
    if created and instance.service.requires_provisioning:
        # New subscription created that requires provisioning
        if not instance.provisioning_completed:
            logger.info(f"Starting automatic provisioning for subscription {instance.id}")
            instance.status = 'provisioning'
            instance.save(update_fields=['status'])
            
            success, message = provisioning_service.provision_subscription(instance)
            
            if not success:
                # Provisioning failed - create failure record and notify admins
                instance.provisioning_error = message
                instance.status = 'pending'
                instance.save(update_fields=['provisioning_error', 'status'])
                logger.error(f"Provisioning failed for subscription {instance.id}: {message}")
                
                # Create failure record for admin review
                try:
                    failure = create_provisioning_failure_record(instance, message)
                    notify_admins_of_failure(failure)
                    logger.info(f"Created provisioning failure record {failure.id} for subscription {instance.id}")
                except Exception as e:
                    logger.error(f"Failed to create failure record: {str(e)}")
            else:
                logger.info(f"Provisioning successful for subscription {instance.id}: {message}")
