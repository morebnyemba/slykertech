from django.urls import path, include
from rest_framework.routers import DefaultRouter
from .views import (
    NotificationViewSet, WhatsAppMessageViewSet, 
    NotificationTemplateViewSet, NotificationPreferenceViewSet
)

router = DefaultRouter()
router.register(r'notifications', NotificationViewSet, basename='notification')
router.register(r'whatsapp', WhatsAppMessageViewSet, basename='whatsapp')
router.register(r'templates', NotificationTemplateViewSet, basename='template')
router.register(r'preferences', NotificationPreferenceViewSet, basename='preference')

urlpatterns = [
    path('', include(router.urls)),
]
