from django.contrib import admin
from .models import Wallet, WalletTransaction

@admin.register(Wallet)
class WalletAdmin(admin.ModelAdmin):
    list_display = ['user', 'balance', 'currency', 'low_balance_threshold', 'auto_topup_enabled']
    list_filter = ['currency', 'auto_topup_enabled']
    search_fields = ['user__email']
    readonly_fields = ['balance', 'created_at', 'updated_at']

@admin.register(WalletTransaction)
class WalletTransactionAdmin(admin.ModelAdmin):
    list_display = ['wallet', 'transaction_type', 'category', 'amount', 'created_at']
    list_filter = ['transaction_type', 'category', 'created_at']
    search_fields = ['wallet__user__email', 'reference', 'description']
    readonly_fields = ['created_at']
