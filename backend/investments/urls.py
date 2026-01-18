from django.urls import path, include
from rest_framework.routers import DefaultRouter
from .views import InvestmentPackageViewSet, InvestmentViewSet, BankingDetailsViewSet

router = DefaultRouter()
router.register(r'packages', InvestmentPackageViewSet, basename='investment-package')
router.register(r'investments', InvestmentViewSet, basename='investment')
router.register(r'banking', BankingDetailsViewSet, basename='banking-details')

urlpatterns = [
    path('', include(router.urls)),
]
