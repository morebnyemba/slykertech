from rest_framework import viewsets, permissions, filters, status
from rest_framework.decorators import action
from rest_framework.response import Response
from django_filters.rest_framework import DjangoFilterBackend
from .models import InvestmentPackage, Investment, BankingDetails
from .serializers import (
    InvestmentPackageSerializer, InvestmentSerializer, 
    InvestmentCreateSerializer, BankingDetailsSerializer
)

class InvestmentPackageViewSet(viewsets.ModelViewSet):
    queryset = InvestmentPackage.objects.filter(is_active=True)
    serializer_class = InvestmentPackageSerializer
    filter_backends = [filters.OrderingFilter]
    ordering_fields = ['minimum_amount', 'expected_return', 'duration_months']
    ordering = ['minimum_amount']
    
    def get_permissions(self):
        if self.action in ['list', 'retrieve']:
            return [permissions.AllowAny()]
        return [permissions.IsAdminUser()]

class InvestmentViewSet(viewsets.ModelViewSet):
    queryset = Investment.objects.all()
    serializer_class = InvestmentSerializer
    permission_classes = [permissions.IsAuthenticated]
    filter_backends = [DjangoFilterBackend, filters.OrderingFilter]
    filterset_fields = ['package', 'status']
    ordering_fields = ['start_date', 'maturity_date', 'amount']
    ordering = ['-start_date']
    
    def get_queryset(self):
        user = self.request.user
        if user.is_staff:
            return Investment.objects.all()
        return Investment.objects.filter(investor=user)
    
    def get_serializer_class(self):
        if self.action == 'create':
            return InvestmentCreateSerializer
        return InvestmentSerializer
    
    @action(detail=False, methods=['get'])
    def my_investments(self, request):
        investments = Investment.objects.filter(investor=request.user)
        serializer = self.get_serializer(investments, many=True)
        return Response(serializer.data)
    
    @action(detail=False, methods=['get'])
    def dashboard(self, request):
        investments = Investment.objects.filter(investor=request.user)
        total_invested = sum(inv.amount for inv in investments)
        total_current_value = sum(inv.calculate_current_value() for inv in investments)
        active_investments = investments.filter(status='active').count()
        
        return Response({
            'total_invested': total_invested,
            'total_current_value': total_current_value,
            'total_return': total_current_value - total_invested,
            'active_investments': active_investments,
            'total_investments': investments.count(),
            'investments': InvestmentSerializer(investments, many=True).data
        })
    
    @action(detail=True, methods=['post'])
    def update_value(self, request, pk=None):
        investment = self.get_object()
        investment.current_value = investment.calculate_current_value()
        investment.save()
        return Response(self.get_serializer(investment).data)

class BankingDetailsViewSet(viewsets.ModelViewSet):
    queryset = BankingDetails.objects.all()
    serializer_class = BankingDetailsSerializer
    permission_classes = [permissions.IsAuthenticated]
    
    def get_queryset(self):
        user = self.request.user
        if user.is_staff:
            return BankingDetails.objects.all()
        return BankingDetails.objects.filter(investor=user)
    
    @action(detail=False, methods=['get'])
    def my_banking_details(self, request):
        details = BankingDetails.objects.filter(investor=request.user)
        serializer = self.get_serializer(details, many=True)
        return Response(serializer.data)
