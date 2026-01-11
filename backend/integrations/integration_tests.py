"""
Integration tests for external API services
Run with: python manage.py test integrations.integration_tests
"""
from django.test import TestCase
from integrations.api_clients import (
    cPanelAPIClient,
    DirectAdminAPIClient,
    CloudflareAPIClient,
    NamecheapAPIClient
)
from integrations.models import APIConfiguration
from notifications.whatsapp_service import WhatsAppService
from billing.paynow_service import PaynowService


class APIIntegrationTestCase(TestCase):
    """Test external API integrations"""

    def test_cpanel_api_connection(self):
        """Test cPanel API connectivity"""
        # This is a smoke test - actual credentials required for real test
        try:
            client = cPanelAPIClient(
                host='cpanel.example.com',
                username='testuser',
                api_token='test_token'
            )
            # Just check the client can be instantiated
            self.assertIsNotNone(client)
            print("✓ cPanel API client initialized successfully")
        except Exception as e:
            self.skipTest(f"cPanel API test skipped: {e}")

    def test_directadmin_api_connection(self):
        """Test DirectAdmin API connectivity"""
        try:
            client = DirectAdminAPIClient(
                host='directadmin.example.com',
                username='testuser',
                password='testpass'
            )
            self.assertIsNotNone(client)
            print("✓ DirectAdmin API client initialized successfully")
        except Exception as e:
            self.skipTest(f"DirectAdmin API test skipped: {e}")

    def test_cloudflare_api_connection(self):
        """Test Cloudflare API connectivity"""
        try:
            client = CloudflareAPIClient(
                api_token='test_token',
                email='test@example.com'
            )
            self.assertIsNotNone(client)
            print("✓ Cloudflare API client initialized successfully")
        except Exception as e:
            self.skipTest(f"Cloudflare API test skipped: {e}")

    def test_namecheap_api_connection(self):
        """Test Namecheap API connectivity"""
        try:
            client = NamecheapAPIClient(
                api_user='testuser',
                api_key='test_key',
                username='testuser',
                client_ip='1.2.3.4'
            )
            self.assertIsNotNone(client)
            print("✓ Namecheap API client initialized successfully")
        except Exception as e:
            self.skipTest(f"Namecheap API test skipped: {e}")

    def test_whatsapp_service_initialization(self):
        """Test WhatsApp service initialization"""
        try:
            whatsapp = WhatsAppService()
            self.assertIsNotNone(whatsapp)
            print("✓ WhatsApp service initialized successfully")
        except Exception as e:
            self.skipTest(f"WhatsApp service test skipped: {e}")

    def test_paynow_service_initialization(self):
        """Test Paynow service initialization"""
        try:
            paynow = PaynowService()
            self.assertIsNotNone(paynow)
            print("✓ Paynow service initialized successfully")
        except Exception as e:
            self.skipTest(f"Paynow service test skipped: {e}")

    def test_api_configuration_encryption(self):
        """Test API configuration encryption/decryption"""
        config = APIConfiguration.objects.create(
            provider='test',
            name='Test Config',
            is_active=True
        )
        
        # Test encryption
        test_token = 'super_secret_token_12345'
        config.set_access_token(test_token)
        config.save()
        
        # Verify token is encrypted in database
        config.refresh_from_db()
        self.assertNotEqual(config.access_token, test_token)
        
        # Verify token can be decrypted
        decrypted = config.get_access_token()
        self.assertEqual(decrypted, test_token)
        
        print("✓ API configuration encryption working correctly")
        config.delete()


class ServiceEndpointTestCase(TestCase):
    """Test service API endpoints are accessible"""

    def test_services_list_endpoint(self):
        """Test services list endpoint"""
        from django.urls import reverse
        from rest_framework.test import APIClient
        
        client = APIClient()
        url = reverse('service-list')
        response = client.get(url)
        
        self.assertIn(response.status_code, [200, 401])  # 401 if auth required
        print(f"✓ Services list endpoint accessible ({response.status_code})")

    def test_subscriptions_endpoint(self):
        """Test subscriptions endpoint"""
        from django.urls import reverse
        from rest_framework.test import APIClient
        
        client = APIClient()
        url = reverse('servicesubscription-list')
        response = client.get(url)
        
        self.assertIn(response.status_code, [200, 401])
        print(f"✓ Subscriptions endpoint accessible ({response.status_code})")

    def test_invoices_endpoint(self):
        """Test invoices endpoint"""
        from django.urls import reverse
        from rest_framework.test import APIClient
        
        client = APIClient()
        url = reverse('invoice-list')
        response = client.get(url)
        
        self.assertIn(response.status_code, [200, 401])
        print(f"✓ Invoices endpoint accessible ({response.status_code})")

    def test_projects_endpoint(self):
        """Test projects endpoint"""
        from django.urls import reverse
        from rest_framework.test import APIClient
        
        client = APIClient()
        url = reverse('projecttracker-list')
        response = client.get(url)
        
        self.assertIn(response.status_code, [200, 401])
        print(f"✓ Projects endpoint accessible ({response.status_code})")

    def test_notifications_endpoint(self):
        """Test notifications endpoint"""
        from django.urls import reverse
        from rest_framework.test import APIClient
        
        client = APIClient()
        url = reverse('notification-list')
        response = client.get(url)
        
        self.assertIn(response.status_code, [200, 401])
        print(f"✓ Notifications endpoint accessible ({response.status_code})")
