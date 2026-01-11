from rest_framework import viewsets, status
from rest_framework.decorators import action
from rest_framework.response import Response
from rest_framework.permissions import IsAuthenticated
from .models import Wallet, WalletTransaction
from .serializers import WalletSerializer, WalletTransactionSerializer, WalletTopUpSerializer
from decimal import Decimal

class WalletViewSet(viewsets.ReadOnlyModelViewSet):
    queryset = Wallet.objects.all()
    serializer_class = WalletSerializer
    permission_classes = [IsAuthenticated]
    
    def get_queryset(self):
        if self.request.user.is_staff:
            return Wallet.objects.all()
        return Wallet.objects.filter(user=self.request.user)
    
    @action(detail=True, methods=['post'])
    def topup(self, request, pk=None):
        wallet = self.get_object()
        serializer = WalletTopUpSerializer(data=request.data)
        
        if serializer.is_valid():
            amount = serializer.validated_data['amount']
            payment_method = serializer.validated_data['payment_method']
            payment_ref = serializer.validated_data.get('payment_reference', f'TOPUP-{wallet.id}')
            
            # Credit wallet
            new_balance = wallet.credit(
                amount=amount,
                category='top_up',
                description=f'Wallet top-up via {payment_method}',
                reference=payment_ref
            )
            
            return Response({
                'status': 'success',
                'new_balance': new_balance,
                'transaction_id': wallet.transactions.first().id
            })
        
        return Response(serializer.errors, status=status.HTTP_400_BAD_REQUEST)

class WalletTransactionViewSet(viewsets.ReadOnlyModelViewSet):
    queryset = WalletTransaction.objects.all()
    serializer_class = WalletTransactionSerializer
    permission_classes = [IsAuthenticated]
    
    def get_queryset(self):
        if self.request.user.is_staff:
            return WalletTransaction.objects.all()
        return WalletTransaction.objects.filter(wallet__user=self.request.user)
