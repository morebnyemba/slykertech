"""
WhatsApp Business API integration service using official Heyoo SDK
"""
from typing import Dict, Optional
from django.conf import settings

try:
    from heyoo import WhatsApp
    HEYOO_AVAILABLE = True
except ImportError:
    HEYOO_AVAILABLE = False
    WhatsApp = None


class WhatsAppService:
    """Service for sending WhatsApp messages via Business API using Heyoo SDK"""
    
    def __init__(self, config=None):
        """
        Initialize WhatsApp service with API credentials from database
        
        Args:
            config: Optional APIConfiguration object. If not provided, fetches from database.
        """
        if not HEYOO_AVAILABLE:
            raise ImportError("Heyoo SDK is not installed. Install with: pip install heyoo")
            
        if config is None:
            # Fetch from database
            from integrations.models import APIConfiguration
            try:
                config = APIConfiguration.objects.get(provider='whatsapp', is_active=True)
                self.access_token = config.get_access_token() or ''
                
                # Get additional config from config_data
                config_data = config.config_data or {}
                self.phone_number_id = config_data.get('phone_number_id', '')
                self.business_account_id = config_data.get('business_account_id', '')
            except APIConfiguration.DoesNotExist:
                # Fallback to environment variables if no DB config exists
                from decouple import config as env_config
                self.phone_number_id = env_config('WHATSAPP_PHONE_NUMBER_ID', default='')
                self.access_token = env_config('WHATSAPP_ACCESS_TOKEN', default='')
                self.business_account_id = env_config('WHATSAPP_BUSINESS_ACCOUNT_ID', default='')
        else:
            # Use provided config
            self.access_token = config.get_access_token() or ''
            config_data = config.config_data or {}
            self.phone_number_id = config_data.get('phone_number_id', '')
            self.business_account_id = config_data.get('business_account_id', '')
        
        # Initialize Heyoo WhatsApp client
        if self.access_token and self.phone_number_id:
            self.client = WhatsApp(token=self.access_token, phone_number_id=self.phone_number_id)
        else:
            self.client = None

    
    def send_message(self, to: str, message: str, template_name: str = None, 
                    template_params: list = None) -> Dict:
        """
        Send a WhatsApp message using Heyoo SDK
        
        Args:
            to: Recipient phone number with country code (e.g., 263771234567)
            message: Message text (for non-template messages)
            template_name: Template name (for template messages)
            template_params: Template parameters
            
        Returns:
            Dict with success status and message_id or error
        """
        if not self.client:
            return {
                "success": False,
                "error": "WhatsApp API not configured. Please set WHATSAPP_PHONE_NUMBER_ID and WHATSAPP_ACCESS_TOKEN"
            }
        
        try:
            if template_name:
                # Template message - use Heyoo's send_template method
                response = self.client.send_template(
                    template=template_name,
                    recipient_id=to,
                    components=template_params or [],
                    lang="en"
                )
            else:
                # Text message - use Heyoo's send_message method
                response = self.client.send_message(
                    message=message,
                    recipient_id=to
                )
            
            # Heyoo returns a dict with the response
            if isinstance(response, dict):
                if response.get('messages'):
                    return {
                        "success": True,
                        "message_id": response['messages'][0]['id'],
                        "data": response
                    }
                elif response.get('error'):
                    return {
                        "success": False,
                        "error": response['error'].get('message', 'Unknown error'),
                        "data": response
                    }
                else:
                    return {
                        "success": True,
                        "data": response
                    }
            else:
                return {
                    "success": False,
                    "error": "Unexpected response format from WhatsApp API"
                }
                
        except Exception as e:
            return {
                "success": False,
                "error": str(e)
            }
    
    def send_template_message(self, to: str, template_name: str, params: list = None) -> Dict:
        """
        Send a template-based WhatsApp message
        
        Args:
            to: Recipient phone number
            template_name: Approved template name
            params: List of parameters for template placeholders
        """
        return self.send_message(to, "", template_name=template_name, template_params=params)
    
    def get_message_status(self, message_id: str) -> Dict:
        """
        Get message delivery status using Heyoo SDK
        
        Args:
            message_id: WhatsApp message ID
            
        Returns:
            Dict with message status
        """
        if not self.client:
            return {
                "success": False,
                "error": "WhatsApp API not configured"
            }
        
        try:
            # Heyoo doesn't have a direct get_message_status method
            # We need to use the query_message_status or rely on webhooks
            # For now, return a placeholder response
            return {
                "success": True,
                "message": "Message status tracking requires webhook configuration",
                "message_id": message_id
            }
        except Exception as e:
            return {
                "success": False,
                "error": str(e)
            }
    
    def send_invoice_notification(self, to: str, invoice_number: str, 
                                  amount: str, due_date: str) -> Dict:
        """
        Send invoice notification via WhatsApp
        
        Args:
            to: Recipient phone number
            invoice_number: Invoice number
            amount: Invoice amount
            due_date: Due date
        """
        message = f"""
🧾 *New Invoice Generated*

Invoice #: {invoice_number}
Amount: ${amount}
Due Date: {due_date}

Please log in to your client portal to view and pay this invoice.

Thank you for your business!
        """.strip()
        
        return self.send_message(to, message)
    
    def send_payment_confirmation(self, to: str, invoice_number: str, amount: str) -> Dict:
        """
        Send payment confirmation via WhatsApp
        
        Args:
            to: Recipient phone number
            invoice_number: Invoice number
            amount: Payment amount
        """
        message = f"""
✅ *Payment Received*

Invoice #: {invoice_number}
Amount Paid: ${amount}

Thank you for your payment! Your invoice has been marked as paid.

Have a great day!
        """.strip()
        
        return self.send_message(to, message)
    
    def send_service_notification(self, to: str, service_name: str, 
                                  action: str, details: str = "") -> Dict:
        """
        Send service-related notification
        
        Args:
            to: Recipient phone number
            service_name: Name of service
            action: Action performed (activated, suspended, etc.)
            details: Additional details
        """
        message = f"""
🔔 *Service Update*

Service: {service_name}
Status: {action.title()}

{details}

For more information, please visit your client portal.
        """.strip()
        
        return self.send_message(to, message)
