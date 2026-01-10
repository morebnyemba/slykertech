from django.urls import path, include
from rest_framework.routers import DefaultRouter
from .views import ServiceViewSet, ServiceSubscriptionViewSet, DNSRecordViewSet

router = DefaultRouter()
router.register(r'services', ServiceViewSet, basename='service')
router.register(r'subscriptions', ServiceSubscriptionViewSet, basename='subscription')
router.register(r'dns-records', DNSRecordViewSet, basename='dns-record')

urlpatterns = [
    path('', include(router.urls)),
]
