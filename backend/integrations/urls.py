from django.urls import path, include
from rest_framework.routers import DefaultRouter
from .views import IntegrationCredentialViewSet, cPanelAccountViewSet, DirectAdminAccountViewSet

router = DefaultRouter()
router.register(r'credentials', IntegrationCredentialViewSet, basename='credential')
router.register(r'cpanel', cPanelAccountViewSet, basename='cpanel')
router.register(r'directadmin', DirectAdminAccountViewSet, basename='directadmin')

urlpatterns = [
    path('', include(router.urls)),
]
