from celery import shared_task
from django.utils import timezone
from .models import ResellerCommission

@shared_task
def process_commissions():
    """Process approved commissions for monthly payout"""
    commissions = ResellerCommission.objects.filter(
        status='approved',
        paid_at__isnull=True
    )
    
    processed_count = 0
    for commission in commissions:
        try:
            # Credit reseller wallet
            wallet = commission.reseller.user.wallet
            wallet.credit(
                amount=commission.amount,
                category='commission',
                description=f'Commission payout for subscription {commission.subscription.id}',
                reference=f'COMM-{commission.id}'
            )
            
            commission.status = 'paid'
            commission.paid_at = timezone.now()
            commission.save()
            processed_count += 1
        except Exception as e:
            print(f"Failed to process commission {commission.id}: {e}")
    
    return f"Processed {processed_count} commission payments"

@shared_task
def calculate_commissions():
    """Calculate commissions for new subscriptions"""
    from services.models import ServiceSubscription
    
    # Find subscriptions without commissions
    subscriptions = ServiceSubscription.objects.filter(
        status='active',
        reseller_commission__isnull=True
    ).select_related('client')
    
    calculated_count = 0
    for sub in subscriptions:
        # Check if client belongs to a reseller
        reseller_clients = sub.client.reseller_subscriptions.all()
        
        for rc in reseller_clients:
            commission_amount = sub.price * (rc.reseller.commission_percentage / 100)
            
            ResellerCommission.objects.create(
                reseller=rc.reseller,
                subscription=sub,
                amount=commission_amount,
                status='pending'
            )
            calculated_count += 1
    
    return f"Calculated {calculated_count} new commissions"
