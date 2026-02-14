"""
Tests for services app - DNS records, project packages, and project tracking
"""
from django.test import TestCase
from django.urls import reverse
from rest_framework.test import APIClient
from rest_framework import status
from datetime import date
from decimal import Decimal

from accounts.models import User
from clients.models import Client
from services.models import (Service, ServiceSubscription, DNSRecord,
                            ProjectPackage, ProjectTracker, ProjectMilestone, 
                            ProjectTask, ProjectComment)


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


class ProjectPackageModelTest(TestCase):
    """Test ProjectPackage model"""

    def test_create_project_package(self):
        package = ProjectPackage.objects.create(
            name='Basic Website',
            slug='basic-website',
            description='A simple 5-page website',
            project_type='web_development',
            deliverables=['5-page website', 'Contact form', 'SEO setup'],
            estimated_duration_days=30,
            max_revisions=3,
            base_price=Decimal('500.00'),
        )
        self.assertEqual(package.name, 'Basic Website')
        self.assertEqual(package.project_type, 'web_development')
        self.assertEqual(package.base_price, Decimal('500.00'))
        self.assertEqual(len(package.deliverables), 3)
        self.assertTrue(package.is_active)

    def test_project_package_str(self):
        package = ProjectPackage.objects.create(
            name='E-commerce Store',
            slug='ecommerce-store',
            description='Full e-commerce solution',
            base_price=Decimal('2000.00'),
        )
        self.assertEqual(str(package), 'E-commerce Store')

    def test_project_package_defaults(self):
        package = ProjectPackage.objects.create(
            name='Test Package',
            slug='test-package',
            description='Test',
            base_price=Decimal('100.00'),
        )
        self.assertEqual(package.project_type, 'web_development')
        self.assertEqual(package.estimated_duration_days, 30)
        self.assertEqual(package.max_revisions, 3)
        self.assertFalse(package.is_featured)
        self.assertTrue(package.is_active)


class ProjectTrackerModelTest(TestCase):
    """Test improved ProjectTracker model"""

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
            name='Web Development',
            category='development',
            description='Web dev service',
            base_price=Decimal('1000.00'),
        )
        self.subscription = ServiceSubscription.objects.create(
            client=self.client_obj,
            service=self.service,
            status='active',
            billing_cycle='one_time',
            price=Decimal('1000.00'),
            start_date=date.today(),
        )
        self.package = ProjectPackage.objects.create(
            name='Basic Website',
            slug='basic-website',
            description='A basic website package',
            base_price=Decimal('500.00'),
        )

    def test_create_project_with_package(self):
        project = ProjectTracker.objects.create(
            subscription=self.subscription,
            project_package=self.package,
            client=self.client_obj,
            title='Client Website',
            project_type='web_development',
            budget=Decimal('500.00'),
        )
        self.assertEqual(project.project_package, self.package)
        self.assertEqual(project.client, self.client_obj)
        self.assertEqual(project.project_type, 'web_development')
        self.assertEqual(project.budget, Decimal('500.00'))
        self.assertEqual(project.amount_spent, 0)

    def test_project_budget_tracking(self):
        project = ProjectTracker.objects.create(
            subscription=self.subscription,
            title='Budget Project',
            budget=Decimal('1000.00'),
            amount_spent=Decimal('250.00'),
        )
        self.assertEqual(project.budget, Decimal('1000.00'))
        self.assertEqual(project.amount_spent, Decimal('250.00'))

    def test_project_type_field(self):
        project = ProjectTracker.objects.create(
            subscription=self.subscription,
            title='SEO Project',
            project_type='seo',
        )
        self.assertEqual(project.project_type, 'seo')

    def test_update_progress_completes_project(self):
        project = ProjectTracker.objects.create(
            subscription=self.subscription,
            title='Progress Project',
        )
        project.update_progress(100)
        project.refresh_from_db()
        self.assertEqual(project.progress_percentage, 100)
        self.assertEqual(project.status, 'completed')
        self.assertIsNotNone(project.actual_completion_date)


class ProjectMilestoneModelTest(TestCase):
    """Test improved ProjectMilestone model"""

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
            name='Web Development',
            category='development',
            description='Web dev service',
            base_price=Decimal('1000.00'),
        )
        self.subscription = ServiceSubscription.objects.create(
            client=self.client_obj,
            service=self.service,
            status='active',
            billing_cycle='one_time',
            price=Decimal('1000.00'),
            start_date=date.today(),
        )
        self.project = ProjectTracker.objects.create(
            subscription=self.subscription,
            title='Test Project',
        )

    def test_milestone_with_deliverables(self):
        milestone = ProjectMilestone.objects.create(
            project=self.project,
            title='Design Phase',
            deliverables=['Wireframes', 'Mockups', 'Style guide'],
        )
        self.assertEqual(len(milestone.deliverables), 3)
        self.assertIn('Wireframes', milestone.deliverables)

    def test_billable_milestone(self):
        milestone = ProjectMilestone.objects.create(
            project=self.project,
            title='Design Approval',
            is_billable=True,
            amount=Decimal('250.00'),
            payment_status='pending',
        )
        self.assertTrue(milestone.is_billable)
        self.assertEqual(milestone.amount, Decimal('250.00'))
        self.assertEqual(milestone.payment_status, 'pending')

    def test_milestone_payment_status_default(self):
        milestone = ProjectMilestone.objects.create(
            project=self.project,
            title='Non-billable Milestone',
        )
        self.assertFalse(milestone.is_billable)
        self.assertEqual(milestone.payment_status, 'not_applicable')


class ProjectTaskModelTest(TestCase):
    """Test improved ProjectTask model"""

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
            name='Web Development',
            category='development',
            description='Web dev service',
            base_price=Decimal('1000.00'),
        )
        self.subscription = ServiceSubscription.objects.create(
            client=self.client_obj,
            service=self.service,
            status='active',
            billing_cycle='one_time',
            price=Decimal('1000.00'),
            start_date=date.today(),
        )
        self.project = ProjectTracker.objects.create(
            subscription=self.subscription,
            title='Test Project',
        )

    def test_task_with_priority(self):
        task = ProjectTask.objects.create(
            project=self.project,
            title='Urgent Bug Fix',
            priority='urgent',
        )
        self.assertEqual(task.priority, 'urgent')

    def test_task_priority_default(self):
        task = ProjectTask.objects.create(
            project=self.project,
            title='Normal Task',
        )
        self.assertEqual(task.priority, 'medium')

    def test_task_dependency(self):
        task1 = ProjectTask.objects.create(
            project=self.project,
            title='Design Homepage',
        )
        task2 = ProjectTask.objects.create(
            project=self.project,
            title='Develop Homepage',
            depends_on=task1,
        )
        self.assertEqual(task2.depends_on, task1)
        self.assertIn(task2, task1.dependents.all())


class ProjectPackageViewSetTest(TestCase):
    """Test ProjectPackage REST API viewset"""

    def setUp(self):
        self.api_client = APIClient()
        self.package = ProjectPackage.objects.create(
            name='Basic Website',
            slug='basic-website',
            description='A simple website',
            project_type='web_development',
            base_price=Decimal('500.00'),
            is_active=True,
        )
        self.inactive_package = ProjectPackage.objects.create(
            name='Old Package',
            slug='old-package',
            description='Discontinued',
            base_price=Decimal('100.00'),
            is_active=False,
        )

    def test_list_packages_unauthenticated(self):
        """Unauthenticated users can see active packages"""
        url = reverse('project-package-list')
        response = self.api_client.get(url)
        self.assertEqual(response.status_code, status.HTTP_200_OK)
        self.assertEqual(len(response.data['results']), 1)
        self.assertEqual(response.data['results'][0]['name'], 'Basic Website')

    def test_admin_sees_all_packages(self):
        """Admin users can see all packages including inactive"""
        admin_user = User.objects.create_superuser(
            email='admin@example.com',
            password='testpass123',
        )
        self.api_client.force_authenticate(user=admin_user)
        url = reverse('project-package-list')
        response = self.api_client.get(url)
        self.assertEqual(response.status_code, status.HTTP_200_OK)
        self.assertEqual(len(response.data['results']), 2)

    def test_retrieve_package(self):
        """Can retrieve a single package by ID"""
        url = reverse('project-package-detail', args=[self.package.id])
        response = self.api_client.get(url)
        self.assertEqual(response.status_code, status.HTTP_200_OK)
        self.assertEqual(response.data['name'], 'Basic Website')
        self.assertEqual(response.data['project_type'], 'web_development')
