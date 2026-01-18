from django.contrib import admin
from .models import InvestmentPackage, Investment, BankingDetails

@admin.register(InvestmentPackage)
class InvestmentPackageAdmin(admin.ModelAdmin):
    list_display = ['name', 'minimum_amount', 'expected_return', 'duration_months', 'is_active']
    list_filter = ['is_active', 'duration_months']
    search_fields = ['name', 'description']
    ordering = ['minimum_amount']
    
    fieldsets = (
        ('Package Information', {
            'fields': ('name', 'description', 'is_active')
        }),
        ('Financial Details', {
            'fields': ('minimum_amount', 'expected_return', 'duration_months')
        }),
    )

@admin.register(Investment)
class InvestmentAdmin(admin.ModelAdmin):
    list_display = ['investor', 'package', 'amount', 'start_date', 'maturity_date', 'status', 'current_value']
    list_filter = ['status', 'start_date', 'package']
    search_fields = ['investor__email', 'package__name']
    date_hierarchy = 'start_date'
    ordering = ['-start_date']
    readonly_fields = ['start_date', 'maturity_date']
    
    fieldsets = (
        ('Investment Information', {
            'fields': ('investor', 'package', 'amount', 'status')
        }),
        ('Dates & Value', {
            'fields': ('start_date', 'maturity_date', 'current_value')
        }),
    )
    
    def save_model(self, request, obj, form, change):
        if not change:
            obj.current_value = obj.amount
        super().save_model(request, obj, form, change)

@admin.register(BankingDetails)
class BankingDetailsAdmin(admin.ModelAdmin):
    list_display = ['investor', 'bank_name', 'account_number', 'is_primary']
    list_filter = ['bank_name', 'is_primary']
    search_fields = ['investor__email', 'bank_name', 'account_number', 'account_holder_name']
    ordering = ['-is_primary', '-created_at']
    
    fieldsets = (
        ('Investor Information', {
            'fields': ('investor', 'is_primary')
        }),
        ('Banking Details', {
            'fields': ('bank_name', 'account_number', 'account_holder_name', 'swift_code', 'branch_code')
        }),
    )
