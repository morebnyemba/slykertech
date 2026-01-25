from django.test import TestCase
from django.contrib.auth import get_user_model
from rest_framework.test import APITestCase
from rest_framework import status
from .models import ReferralProfile, Referral, ReferralReward, ReferralSettings

User = get_user_model()


class ReferralProfileModelTest(TestCase):
    def setUp(self):
        self.user = User.objects.create_user(
            email='test@example.com',
            password='testpass123'
        )
    
    def test_referral_profile_creation(self):
        profile = ReferralProfile.objects.create(user=self.user)
        self.assertIsNotNone(profile.referral_code)
        self.assertEqual(len(profile.referral_code), 8)
        self.assertEqual(profile.total_referrals, 0)
        self.assertEqual(profile.successful_referrals, 0)
    
    def test_referral_code_uniqueness(self):
        profile1 = ReferralProfile.objects.create(user=self.user)
        user2 = User.objects.create_user(
            email='test2@example.com',
            password='testpass123'
        )
        profile2 = ReferralProfile.objects.create(user=user2)
        self.assertNotEqual(profile1.referral_code, profile2.referral_code)


class ReferralAPITest(APITestCase):
    def setUp(self):
        self.user = User.objects.create_user(
            email='test@example.com',
            password='testpass123'
        )
        self.client.force_authenticate(user=self.user)
    
    def test_get_my_profile(self):
        response = self.client.get('/api/referrals/profiles/my_profile/')
        self.assertEqual(response.status_code, status.HTTP_200_OK)
        self.assertIn('referral_code', response.data)
    
    def test_get_stats(self):
        response = self.client.get('/api/referrals/profiles/stats/')
        self.assertEqual(response.status_code, status.HTTP_200_OK)
        self.assertIn('total_referrals', response.data)
        self.assertIn('referral_code', response.data)
    
    def test_create_referral(self):
        response = self.client.post('/api/referrals/referrals/', {
            'referred_email': 'newuser@example.com'
        })
        self.assertEqual(response.status_code, status.HTTP_201_CREATED)
        self.assertEqual(response.data['referred_email'], 'newuser@example.com')
    
    def test_validate_code(self):
        profile = ReferralProfile.objects.create(user=self.user)
        response = self.client.post('/api/referrals/referrals/validate_code/', {
            'referral_code': profile.referral_code
        })
        self.assertEqual(response.status_code, status.HTTP_200_OK)
        self.assertTrue(response.data['valid'])
