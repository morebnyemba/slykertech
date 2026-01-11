from django.contrib import admin
from .models import Invoice, InvoiceItem, Payment, BillingProfile


class InvoiceItemInline(admin.TabularInline):
    model = InvoiceItem
    extra = 1


class PaymentInline(admin.TabularInline):
    model = Payment
    extra = 0
    readonly_fields = ('payment_date', 'created_at')


@admin.register(Invoice)
class InvoiceAdmin(admin.ModelAdmin):
    list_display = ('invoice_number', 'client', 'status', 'issue_date', 'due_date', 'total', 'created_at')
    list_filter = ('status', 'issue_date', 'due_date')
    search_fields = ('invoice_number', 'client__company_name')
    readonly_fields = ('invoice_number', 'subtotal', 'tax_amount', 'total', 'created_at', 'updated_at')
    inlines = [InvoiceItemInline, PaymentInline]
    
    fieldsets = (
        ('Basic Information', {
            'fields': ('client', 'invoice_number', 'status')
        }),
        ('Dates', {
            'fields': ('issue_date', 'due_date', 'paid_date')
        }),
        ('Amounts', {
            'fields': ('subtotal', 'tax_rate', 'tax_amount', 'discount_amount', 'total')
        }),
        ('Additional Info', {
            'fields': ('notes', 'terms')
        }),
        ('Timestamps', {
            'fields': ('created_at', 'updated_at'),
            'classes': ('collapse',)
        }),
    )


@admin.register(Payment)
class PaymentAdmin(admin.ModelAdmin):
    list_display = ('id', 'invoice', 'payment_method', 'amount', 'status', 'payment_date')
    list_filter = ('status', 'payment_method', 'payment_date')
    search_fields = ('invoice__invoice_number', 'transaction_id')
    readonly_fields = ('payment_date', 'created_at', 'updated_at')


@admin.register(BillingProfile)
class BillingProfileAdmin(admin.ModelAdmin):
    list_display = ('client', 'auto_pay', 'payment_method', 'billing_email')
    list_filter = ('auto_pay', 'payment_method')
    search_fields = ('client__company_name', 'billing_email')
    readonly_fields = ('created_at', 'updated_at')

