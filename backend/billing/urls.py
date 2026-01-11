from django.urls import path, include
from rest_framework.routers import DefaultRouter
from .views import InvoiceViewSet, PaymentViewSet, BillingProfileViewSet

router = DefaultRouter()
router.register(r'invoices', InvoiceViewSet, basename='invoice')
router.register(r'payments', PaymentViewSet, basename='payment')
router.register(r'billing-profiles', BillingProfileViewSet, basename='billing-profile')

urlpatterns = [
    path('', include(router.urls)),
]
