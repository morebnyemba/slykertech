from django.urls import path, include
from rest_framework.routers import DefaultRouter
from . import views

router = DefaultRouter()
router.register(r'profiles', views.ResellerProfileViewSet, basename='reseller-profile')
router.register(r'clients', views.ResellerClientViewSet, basename='reseller-client')
router.register(r'commissions', views.ResellerCommissionViewSet, basename='reseller-commission')

urlpatterns = [
    path('', include(router.urls)),
]
