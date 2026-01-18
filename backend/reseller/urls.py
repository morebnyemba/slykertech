from django.urls import path, include
from rest_framework.routers import DefaultRouter
from . import views

router = DefaultRouter()
router.register(r'partners', views.PartnerProfileViewSet, basename='partner')
router.register(r'resellers', views.ResellerProfileViewSet, basename='reseller')
router.register(r'agencies', views.AgencyPartnerViewSet, basename='agency')
router.register(r'technology-alliances', views.TechnologyAllianceViewSet, basename='technology-alliance')
router.register(r'reseller-clients', views.ResellerClientViewSet, basename='reseller-client')
router.register(r'reseller-commissions', views.ResellerCommissionViewSet, basename='reseller-commission')
router.register(r'agency-referrals', views.AgencyReferralViewSet, basename='agency-referral')

urlpatterns = [
    path('', include(router.urls)),
]
