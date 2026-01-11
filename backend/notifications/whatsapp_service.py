"""
WhatsApp Business API integration service
"""
import requests
from typing import Dict, Optional
from django.conf import settings


class WhatsAppService:
    """Service for sending WhatsApp messages via Business API"""
    
    def __init__(self, config=None):
        """
        Initialize WhatsApp service with API credentials from database
        
        Args:
            config: Optional APIConfiguration object. If not provided, fetches from database.
        """
        if config is None:
            # Fetch from database
            from integrations.models import APIConfiguration
            try:
                config = APIConfiguration.objects.get(provider='whatsapp', is_active=True)
                self.api_url = config.api_url or 'https://graph.facebook.com/v18.0'
                self.access_token = config.get_access_token() or ''
                
                # Get additional config from config_data
                config_data = config.config_data or {}
                self.phone_number_id = config_data.get('phone_number_id', '')
                self.business_account_id = config_data.get('business_account_id', '')
            except APIConfiguration.DoesNotExist:
                # Fallback to environment variables if no DB config exists
                from decouple import config as env_config
                self.api_url = env_config('WHATSAPP_API_URL', default='https://graph.facebook.com/v18.0')
                self.phone_number_id = env_config('WHATSAPP_PHONE_NUMBER_ID', default='')
                self.access_token = env_config('WHATSAPP_ACCESS_TOKEN', default='')
                self.business_account_id = env_config('WHATSAPP_BUSINESS_ACCOUNT_ID', default='')
        else:
            # Use provided config
            self.api_url = config.api_url or 'https://graph.facebook.com/v18.0'
            self.access_token = config.get_access_token() or ''
            config_data = config.config_data or {}
            self.phone_number_id = config_data.get('phone_number_id', '')
            self.business_account_id = config_data.get('business_account_id', '')

    
    def send_message(self, to: str, message: str, template_name: str = None, 
                    template_params: list = None) -> Dict:
        """
        Send a WhatsApp message
        
        Args:
            to: Recipient phone number with country code (e.g., 263771234567)
            message: Message text (for non-template messages)
            template_name: Template name (for template messages)
            template_params: Template parameters
            
        Returns:
            Dict with success status and message_id or error
        """
        if not self.phone_number_id or not self.access_token:
            return {
                "success": False,
                "error": "WhatsApp API not configured. Please set WHATSAPP_PHONE_NUMBER_ID and WHATSAPP_ACCESS_TOKEN"
            }
        
        url = f"{self.api_url}/{self.phone_number_id}/messages"
        headers = {
            "Authorization": f"Bearer {self.access_token}",
            "Content-Type": "application/json"
        }
        
        # Build message payload
        if template_name:
            # Template message
            payload = {
                "messaging_product": "whatsapp",
                "to": to,
                "type": "template",
                "template": {
                    "name": template_name,
                    "language": {
                        "code": "en"
                    }
                }
            }
            
            if template_params:
                payload["template"]["components"] = [
                    {
                        "type": "body",
                        "parameters": [
                            {"type": "text", "text": param} for param in template_params
                        ]
                    }
                ]
        else:
            # Text message
            payload = {
                "messaging_product": "whatsapp",
                "to": to,
                "type": "text",
                "text": {
                    "body": message
                }
            }
        
        try:
            response = requests.post(url, json=payload, headers=headers, timeout=(10, 30))
            response.raise_for_status()
            data = response.json()
            
            if data.get('messages'):
                return {
                    "success": True,
                    "message_id": data['messages'][0]['id'],
                    "data": data
                }
            else:
                return {
                    "success": False,
                    "error": "No message ID returned",
                    "data": data
                }
                
        except requests.exceptions.RequestException as e:
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
        Get message delivery status
        
        Args:
            message_id: WhatsApp message ID
            
        Returns:
            Dict with message status
        """
        url = f"{self.api_url}/{message_id}"
        headers = {
            "Authorization": f"Bearer {self.access_token}"
        }
        
        try:
            response = requests.get(url, headers=headers, timeout=(10, 30))
            response.raise_for_status()
            return {
                "success": True,
                "data": response.json()
            }
        except requests.exceptions.RequestException as e:
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
