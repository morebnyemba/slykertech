"""
Automatic provisioning service for creating hosting accounts, domains, etc.
"""
from django.utils import timezone
from integrations.models import IntegrationCredential, cPanelAccount, DirectAdminAccount
from integrations.api_clients import cPanelAPIClient, DirectAdminAPIClient, NamecheapAPIClient
import logging

logger = logging.getLogger(__name__)


class ProvisioningService:
    """Service for automatically provisioning resources when subscriptions are created"""
    
    def provision_subscription(self, subscription):
        """
        Automatically provision resources for a subscription based on service type
        """
        if not subscription.service.requires_provisioning:
            return True, "No provisioning required"
        
        provisioning_type = subscription.service.provisioning_type
        
        try:
            if provisioning_type == 'cpanel':
                return self.provision_cpanel_account(subscription)
            elif provisioning_type == 'directadmin':
                return self.provision_directadmin_account(subscription)
            elif provisioning_type == 'domain':
                return self.provision_domain(subscription)
            else:
                return False, f"Unknown provisioning type: {provisioning_type}"
        except Exception as e:
            logger.error(f"Provisioning error for subscription {subscription.id}: {str(e)}")
            return False, str(e)
    
    def provision_cpanel_account(self, subscription):
        """
        Create a cPanel account for the client
        """
        client = subscription.client
        
        # Get cPanel server credentials (admin credentials for account creation)
        try:
            server_credential = IntegrationCredential.objects.filter(
                provider='cpanel',
                is_active=True,
                client__isnull=True  # Server-level credential
            ).first()
            
            if not server_credential:
                return False, "No cPanel server credentials configured"
            
            # Get domain from metadata or generate one
            domain = subscription.metadata.get('domain')
            if not domain:
                # Generate a subdomain or use client name
                domain = f"{client.company_name.lower().replace(' ', '')}.yourdomain.com"
            
            # Get username from metadata or generate one
            username = subscription.metadata.get('cpanel_username')
            if not username:
                username = f"{client.company_name[:8].lower().replace(' ', '')}{client.id}"
            
            # Get package details from metadata or use defaults
            plan = subscription.metadata.get('plan', 'default')
            disk_quota = subscription.metadata.get('disk_quota', 1024)  # MB
            bandwidth_quota = subscription.metadata.get('bandwidth_quota', 10240)  # MB
            
            # Initialize cPanel API client
            cpanel_client = cPanelAPIClient(
                host=server_credential.host,
                username=server_credential.username,
                api_token=server_credential.get_api_token()
            )
            
            # Generate a secure password
            import secrets
            import string
            alphabet = string.ascii_letters + string.digits + string.punctuation
            password = ''.join(secrets.choice(alphabet) for _ in range(16))
            
            # Create cPanel account via WHM API
            result = cpanel_client.create_account(
                username=username,
                domain=domain,
                password=password,
                plan=plan,
                quota=disk_quota,
                bwlimit=bandwidth_quota
            )
            
            if result.get('success'):
                # Create client-specific credential
                client_credential, _ = IntegrationCredential.objects.get_or_create(
                    client=client,
                    provider='cpanel',
                    name=f"cPanel - {domain}",
                    defaults={
                        'host': server_credential.host,
                        'username': username,
                        'port': server_credential.port,
                        'is_active': True,
                        'metadata': {'domain': domain}
                    }
                )
                client_credential.set_password(password)
                client_credential.save()
                
                # Create cPanel account record
                cpanel_account = cPanelAccount.objects.create(
                    credential=client_credential,
                    subscription=subscription,
                    cpanel_username=username,
                    domain=domain,
                    disk_quota=disk_quota,
                    bandwidth_quota=bandwidth_quota,
                    last_sync=timezone.now()
                )
                
                # Update subscription metadata
                subscription.metadata.update({
                    'cpanel_account_id': cpanel_account.id,
                    'domain': domain,
                    'cpanel_username': username,
                    'provisioned_at': timezone.now().isoformat()
                })
                subscription.provisioning_completed = True
                subscription.status = 'active'
                subscription.save()
                
                logger.info(f"Successfully provisioned cPanel account for subscription {subscription.id}")
                return True, f"cPanel account created: {username}@{domain}"
            else:
                error_msg = result.get('error', 'Unknown error')
                return False, f"cPanel account creation failed: {error_msg}"
                
        except Exception as e:
            logger.error(f"Error provisioning cPanel account: {str(e)}")
            return False, str(e)
    
    def provision_directadmin_account(self, subscription):
        """
        Create a DirectAdmin account for the client
        """
        client = subscription.client
        
        try:
            server_credential = IntegrationCredential.objects.filter(
                provider='directadmin',
                is_active=True,
                client__isnull=True
            ).first()
            
            if not server_credential:
                return False, "No DirectAdmin server credentials configured"
            
            domain = subscription.metadata.get('domain')
            if not domain:
                domain = f"{client.company_name.lower().replace(' ', '')}.yourdomain.com"
            
            username = subscription.metadata.get('da_username')
            if not username:
                username = f"{client.company_name[:8].lower().replace(' ', '')}{client.id}"
            
            disk_quota = subscription.metadata.get('disk_quota', 1024)
            bandwidth_quota = subscription.metadata.get('bandwidth_quota', 10240)
            
            # Initialize DirectAdmin API client
            da_client = DirectAdminAPIClient(
                host=server_credential.host,
                username=server_credential.username,
                password=server_credential.get_password()
            )
            
            # Generate password
            import secrets
            import string
            alphabet = string.ascii_letters + string.digits
            password = ''.join(secrets.choice(alphabet) for _ in range(16))
            
            # Create DirectAdmin account
            result = da_client.create_account(
                username=username,
                email=client.email,
                password=password,
                domain=domain,
                quota=disk_quota,
                bandwidth=bandwidth_quota
            )
            
            if result.get('success'):
                client_credential, _ = IntegrationCredential.objects.get_or_create(
                    client=client,
                    provider='directadmin',
                    name=f"DirectAdmin - {domain}",
                    defaults={
                        'host': server_credential.host,
                        'username': username,
                        'port': server_credential.port,
                        'is_active': True,
                        'metadata': {'domain': domain}
                    }
                )
                client_credential.set_password(password)
                client_credential.save()
                
                da_account = DirectAdminAccount.objects.create(
                    credential=client_credential,
                    subscription=subscription,
                    da_username=username,
                    domain=domain,
                    disk_quota=disk_quota,
                    bandwidth_quota=bandwidth_quota,
                    last_sync=timezone.now()
                )
                
                subscription.metadata.update({
                    'da_account_id': da_account.id,
                    'domain': domain,
                    'da_username': username,
                    'provisioned_at': timezone.now().isoformat()
                })
                subscription.provisioning_completed = True
                subscription.status = 'active'
                subscription.save()
                
                logger.info(f"Successfully provisioned DirectAdmin account for subscription {subscription.id}")
                return True, f"DirectAdmin account created: {username}@{domain}"
            else:
                error_msg = result.get('error', 'Unknown error')
                return False, f"DirectAdmin account creation failed: {error_msg}"
                
        except Exception as e:
            logger.error(f"Error provisioning DirectAdmin account: {str(e)}")
            return False, str(e)
    
    def provision_domain(self, subscription):
        """
        Register a domain for the client via Namecheap
        """
        try:
            from integrations.models import APIConfiguration
            
            # Get Namecheap API configuration
            namecheap_config = APIConfiguration.objects.filter(
                provider='namecheap',
                is_active=True
            ).first()
            
            if not namecheap_config:
                return False, "No Namecheap API configuration found"
            
            domain = subscription.metadata.get('domain')
            if not domain:
                return False, "Domain name not specified in subscription metadata"
            
            years = subscription.metadata.get('years', 1)
            
            # Initialize Namecheap API client
            namecheap_client = NamecheapAPIClient(
                api_user=namecheap_config.get_api_key(),
                api_key=namecheap_config.get_api_secret(),
                username=namecheap_config.config_data.get('username'),
                client_ip=namecheap_config.config_data.get('client_ip')
            )
            
            # Check domain availability
            availability = namecheap_client.check_domain_availability(domain)
            
            if not availability.get('available'):
                return False, f"Domain {domain} is not available for registration"
            
            # Register domain
            result = namecheap_client.register_domain(domain, years)
            
            if result.get('success'):
                subscription.metadata.update({
                    'domain': domain,
                    'domain_registered': True,
                    'registration_date': timezone.now().isoformat(),
                    'expiry_years': years
                })
                subscription.provisioning_completed = True
                subscription.status = 'active'
                subscription.save()
                
                logger.info(f"Successfully registered domain {domain} for subscription {subscription.id}")
                return True, f"Domain {domain} registered successfully"
            else:
                error_msg = result.get('error', 'Unknown error')
                return False, f"Domain registration failed: {error_msg}"
                
        except Exception as e:
            logger.error(f"Error provisioning domain: {str(e)}")
            return False, str(e)


# Global provisioning service instance
provisioning_service = ProvisioningService()
