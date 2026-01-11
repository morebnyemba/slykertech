"""
Django signals for automatic provisioning
"""
from django.db.models.signals import post_save
from django.dispatch import receiver
from services.models import ServiceSubscription
from services.provisioning import provisioning_service
import logging

logger = logging.getLogger(__name__)


@receiver(post_save, sender=ServiceSubscription)
def auto_provision_subscription(sender, instance, created, **kwargs):
    """
    Automatically provision resources when a subscription is created or activated
    """
    if created and instance.service.requires_provisioning:
        # New subscription created that requires provisioning
        if not instance.provisioning_completed:
            logger.info(f"Starting automatic provisioning for subscription {instance.id}")
            instance.status = 'provisioning'
            instance.save(update_fields=['status'])
            
            success, message = provisioning_service.provision_subscription(instance)
            
            if not success:
                # Provisioning failed
                instance.provisioning_error = message
                instance.status = 'pending'
                instance.save(update_fields=['provisioning_error', 'status'])
                logger.error(f"Provisioning failed for subscription {instance.id}: {message}")
            else:
                logger.info(f"Provisioning successful for subscription {instance.id}: {message}")
