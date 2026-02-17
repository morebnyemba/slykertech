from django.urls import path, include
from rest_framework.routers import DefaultRouter
from .views import InvoiceViewSet, PaymentViewSet, BillingProfileViewSet, CartViewSet, ExpenseViewSet, PromotionViewSet

router = DefaultRouter()
router.register(r'carts', CartViewSet, basename='cart')
router.register(r'invoices', InvoiceViewSet, basename='invoice')
router.register(r'payments', PaymentViewSet, basename='payment')
router.register(r'billing-profiles', BillingProfileViewSet, basename='billing-profile')
router.register(r'expenses', ExpenseViewSet, basename='expense')
router.register(r'promotions', PromotionViewSet, basename='promotion')

urlpatterns = [
    path('', include(router.urls)),
]

