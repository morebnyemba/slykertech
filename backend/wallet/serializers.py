from rest_framework import serializers
from .models import Wallet, WalletTransaction
from django.contrib.auth import get_user_model

User = get_user_model()

class WalletSerializer(serializers.ModelSerializer):
    user_email = serializers.EmailField(source='user.email', read_only=True)
    
    class Meta:
        model = Wallet
        fields = [
            'id', 'user', 'user_email', 'balance', 'currency',
            'low_balance_threshold', 'auto_topup_enabled', 'auto_topup_amount',
            'created_at', 'updated_at'
        ]
        read_only_fields = ['balance', 'created_at', 'updated_at']

class WalletTransactionSerializer(serializers.ModelSerializer):
    wallet_user = serializers.EmailField(source='wallet.user.email', read_only=True)
    
    class Meta:
        model = WalletTransaction
        fields = [
            'id', 'wallet', 'wallet_user', 'transaction_type', 'category',
            'amount', 'balance_before', 'balance_after', 'description',
            'reference', 'created_at'
        ]
        read_only_fields = ['balance_before', 'balance_after', 'created_at']

class WalletTopUpSerializer(serializers.Serializer):
    amount = serializers.DecimalField(max_digits=10, decimal_places=2, min_value=1)
    payment_method = serializers.ChoiceField(choices=['paynow', 'card', 'bank_transfer'])
    payment_reference = serializers.CharField(max_length=255, required=False)
