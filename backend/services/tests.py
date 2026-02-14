"""
Tests for DNS record management
"""
from django.test import TestCase
from django.urls import reverse
from rest_framework.test import APIClient
from rest_framework import status
from datetime import date

from accounts.models import User
from clients.models import Client
from services.models import Service, ServiceSubscription, DNSRecord


class DNSRecordModelTest(TestCase):
    """Test DNSRecord model"""

    def setUp(self):
        self.user = User.objects.create_user(
            email='client@example.com',
            password='testpass123',
            user_type='client',
        )
        self.client_obj = Client.objects.create(
            user=self.user,
            company_name='Test Corp',
        )
        self.service = Service.objects.create(
            name='Web Hosting',
            category='hosting',
            description='Basic hosting',
            base_price=10.00,
        )
        self.subscription = ServiceSubscription.objects.create(
            client=self.client_obj,
            service=self.service,
            status='active',
            billing_cycle='monthly',
            price=10.00,
            start_date=date.today(),
        )

    def test_create_dns_record(self):
        record = DNSRecord.objects.create(
            subscription=self.subscription,
            domain='example.com',
            record_type='A',
            name='@',
            content='192.168.1.1',
            ttl=3600,
        )
        self.assertEqual(record.domain, 'example.com')
        self.assertEqual(record.record_type, 'A')
        self.assertTrue(record.is_active)

    def test_dns_record_str(self):
        record = DNSRecord.objects.create(
            subscription=self.subscription,
            domain='example.com',
            record_type='MX',
            name='@',
            content='mail.example.com',
            ttl=3600,
            priority=10,
        )
        self.assertIn('example.com', str(record))
        self.assertIn('MX', str(record))


class DNSRecordSerializerTest(TestCase):
    """Test DNSRecord serializer validation"""

    def setUp(self):
        self.user = User.objects.create_user(
            email='client@example.com',
            password='testpass123',
            user_type='client',
        )
        self.client_obj = Client.objects.create(
            user=self.user,
            company_name='Test Corp',
        )
        self.service = Service.objects.create(
            name='Web Hosting',
            category='hosting',
            description='Basic hosting',
            base_price=10.00,
        )
        self.subscription = ServiceSubscription.objects.create(
            client=self.client_obj,
            service=self.service,
            status='active',
            billing_cycle='monthly',
            price=10.00,
            start_date=date.today(),
        )

    def test_serializer_validates_ttl_range(self):
        from services.serializers import DNSRecordSerializer
        data = {
            'subscription': self.subscription.id,
            'domain': 'example.com',
            'record_type': 'A',
            'name': '@',
            'content': '1.2.3.4',
            'ttl': 10,  # Too low
        }
        serializer = DNSRecordSerializer(data=data)
        self.assertFalse(serializer.is_valid())
        self.assertIn('ttl', serializer.errors)

    def test_serializer_validates_ttl_max(self):
        from services.serializers import DNSRecordSerializer
        data = {
            'subscription': self.subscription.id,
            'domain': 'example.com',
            'record_type': 'A',
            'name': '@',
            'content': '1.2.3.4',
            'ttl': 100000,  # Too high
        }
        serializer = DNSRecordSerializer(data=data)
        self.assertFalse(serializer.is_valid())
        self.assertIn('ttl', serializer.errors)

    def test_serializer_requires_mx_priority(self):
        from services.serializers import DNSRecordSerializer
        data = {
            'subscription': self.subscription.id,
            'domain': 'example.com',
            'record_type': 'MX',
            'name': '@',
            'content': 'mail.example.com',
            'ttl': 3600,
            # No priority
        }
        serializer = DNSRecordSerializer(data=data)
        self.assertFalse(serializer.is_valid())
        self.assertIn('priority', serializer.errors)

    def test_serializer_valid_record(self):
        from services.serializers import DNSRecordSerializer
        data = {
            'subscription': self.subscription.id,
            'domain': 'example.com',
            'record_type': 'A',
            'name': '@',
            'content': '1.2.3.4',
            'ttl': 3600,
        }
        serializer = DNSRecordSerializer(data=data)
        self.assertTrue(serializer.is_valid(), serializer.errors)


class DNSRecordViewSetTest(TestCase):
    """Test DNSRecord REST API viewset"""

    def setUp(self):
        self.api_client = APIClient()
        self.user = User.objects.create_user(
            email='client@example.com',
            password='testpass123',
            user_type='client',
        )
        self.client_obj = Client.objects.create(
            user=self.user,
            company_name='Test Corp',
        )
        self.service = Service.objects.create(
            name='Web Hosting',
            category='hosting',
            description='Basic hosting',
            base_price=10.00,
        )
        self.subscription = ServiceSubscription.objects.create(
            client=self.client_obj,
            service=self.service,
            status='active',
            billing_cycle='monthly',
            price=10.00,
            start_date=date.today(),
        )
        self.record = DNSRecord.objects.create(
            subscription=self.subscription,
            domain='example.com',
            record_type='A',
            name='@',
            content='1.2.3.4',
            ttl=3600,
        )
        self.api_client.force_authenticate(user=self.user)

    def test_list_dns_records(self):
        url = reverse('dns-record-list')
        response = self.api_client.get(url)
        self.assertEqual(response.status_code, status.HTTP_200_OK)
        self.assertEqual(len(response.data['results']), 1)

    def test_list_filter_by_domain(self):
        url = reverse('dns-record-list')
        response = self.api_client.get(url, {'domain': 'example.com'})
        self.assertEqual(response.status_code, status.HTTP_200_OK)
        self.assertEqual(len(response.data['results']), 1)

        response = self.api_client.get(url, {'domain': 'other.com'})
        self.assertEqual(response.status_code, status.HTTP_200_OK)
        self.assertEqual(len(response.data['results']), 0)

    def test_list_filter_by_record_type(self):
        url = reverse('dns-record-list')
        response = self.api_client.get(url, {'record_type': 'A'})
        self.assertEqual(response.status_code, status.HTTP_200_OK)
        self.assertEqual(len(response.data['results']), 1)

        response = self.api_client.get(url, {'record_type': 'MX'})
        self.assertEqual(response.status_code, status.HTTP_200_OK)
        self.assertEqual(len(response.data['results']), 0)

    def test_list_filter_by_subscription(self):
        url = reverse('dns-record-list')
        response = self.api_client.get(url, {'subscription': self.subscription.id})
        self.assertEqual(response.status_code, status.HTTP_200_OK)
        self.assertEqual(len(response.data['results']), 1)

    def test_create_dns_record(self):
        url = reverse('dns-record-list')
        data = {
            'subscription': self.subscription.id,
            'domain': 'example.com',
            'record_type': 'CNAME',
            'name': 'www',
            'content': 'example.com',
            'ttl': 3600,
        }
        response = self.api_client.post(url, data, format='json')
        self.assertEqual(response.status_code, status.HTTP_201_CREATED)

    def test_create_dns_record_invalid_ttl(self):
        url = reverse('dns-record-list')
        data = {
            'subscription': self.subscription.id,
            'domain': 'example.com',
            'record_type': 'A',
            'name': '@',
            'content': '1.2.3.4',
            'ttl': 10,  # Invalid
        }
        response = self.api_client.post(url, data, format='json')
        self.assertEqual(response.status_code, status.HTTP_400_BAD_REQUEST)

    def test_other_user_cannot_see_records(self):
        other_user = User.objects.create_user(
            email='other@example.com',
            password='testpass123',
            user_type='client',
        )
        self.api_client.force_authenticate(user=other_user)
        url = reverse('dns-record-list')
        response = self.api_client.get(url)
        self.assertEqual(response.status_code, status.HTTP_200_OK)
        self.assertEqual(len(response.data['results']), 0)

    def test_unauthenticated_user_rejected(self):
        self.api_client.force_authenticate(user=None)
        url = reverse('dns-record-list')
        response = self.api_client.get(url)
        self.assertEqual(response.status_code, status.HTTP_401_UNAUTHORIZED)

    def test_admin_sees_all_records(self):
        admin_user = User.objects.create_superuser(
            email='admin@example.com',
            password='testpass123',
        )
        self.api_client.force_authenticate(user=admin_user)
        url = reverse('dns-record-list')
        response = self.api_client.get(url)
        self.assertEqual(response.status_code, status.HTTP_200_OK)
        self.assertEqual(len(response.data['results']), 1)

    def test_ownership_check_on_create(self):
        """Ensure a user cannot add DNS records to another user's subscription"""
        other_user = User.objects.create_user(
            email='other@example.com',
            password='testpass123',
            user_type='client',
        )
        Client.objects.create(user=other_user, company_name='Other Corp')
        self.api_client.force_authenticate(user=other_user)

        url = reverse('dns-record-list')
        data = {
            'subscription': self.subscription.id,  # Belongs to first user
            'domain': 'evil.com',
            'record_type': 'A',
            'name': '@',
            'content': '6.6.6.6',
            'ttl': 3600,
        }
        response = self.api_client.post(url, data, format='json')
        self.assertEqual(response.status_code, status.HTTP_403_FORBIDDEN)
