from django.urls import path, include
from rest_framework.routers import DefaultRouter
from . import views

router = DefaultRouter()
router.register(r'profiles', views.ReferralProfileViewSet, basename='referral-profile')
router.register(r'referrals', views.ReferralViewSet, basename='referral')
router.register(r'rewards', views.ReferralRewardViewSet, basename='referral-reward')
router.register(r'settings', views.ReferralSettingsViewSet, basename='referral-settings')

urlpatterns = [
    path('', include(router.urls)),
]
