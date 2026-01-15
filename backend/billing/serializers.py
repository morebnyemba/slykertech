from rest_framework import serializers
from .models import Invoice, InvoiceItem, Payment, BillingProfile, Cart, CartItem
from clients.serializers import ClientSerializer


class CartItemSerializer(serializers.ModelSerializer):
    """Serializer for CartItem model"""
    
    service_name = serializers.CharField(source='service.name', read_only=True)
    service_category = serializers.CharField(source='service.category', read_only=True)
    total_price = serializers.SerializerMethodField()
    
    class Meta:
        model = CartItem
        fields = ['id', 'cart', 'service', 'service_name', 'service_category', 
                  'service_metadata', 'quantity', 'unit_price', 'billing_cycle', 
                  'total_price', 'created_at']
        read_only_fields = ['id', 'created_at', 'total_price']
    
    def get_total_price(self, obj):
        return float(obj.get_price())
    
    def validate_service_metadata(self, value):
        """Validate service-specific metadata"""
        # Get service from context or instance
        service = None
        if self.instance:
            service = self.instance.service
        elif 'service' in self.initial_data:
            from services.models import Service
            try:
                service = Service.objects.get(id=self.initial_data['service'])
            except Service.DoesNotExist:
                pass
        
        if service and service.category == 'domain':
            # Validate domain transfer has EPP code
            if value.get('action') == 'transfer' and not value.get('epp_code'):
                raise serializers.ValidationError(
                    'EPP/Auth code is required for domain transfers'
                )
        
        return value


class CartSerializer(serializers.ModelSerializer):
    """Serializer for Cart model"""
    
    items = CartItemSerializer(many=True, read_only=True)
    total = serializers.SerializerMethodField()
    item_count = serializers.SerializerMethodField()
    
    class Meta:
        model = Cart
        fields = ['id', 'client', 'session_id', 'status', 'items', 'total', 
                  'item_count', 'created_at', 'updated_at']
        read_only_fields = ['id', 'created_at', 'updated_at', 'total', 'item_count']
    
    def get_total(self, obj):
        return float(obj.get_total())
    
    def get_item_count(self, obj):
        return obj.get_item_count()


class InvoiceItemSerializer(serializers.ModelSerializer):
    """Serializer for InvoiceItem model"""
    
    class Meta:
        model = InvoiceItem
        fields = ['id', 'subscription', 'description', 'quantity', 'unit_price', 'amount', 'created_at']
        read_only_fields = ['id', 'amount', 'created_at']


class InvoiceSerializer(serializers.ModelSerializer):
    """Serializer for Invoice model"""
    
    client = ClientSerializer(read_only=True)
    items = InvoiceItemSerializer(many=True, read_only=True)
    payments = serializers.SerializerMethodField()
    
    class Meta:
        model = Invoice
        fields = ['id', 'client', 'invoice_number', 'status', 'issue_date', 'due_date', 
                  'paid_date', 'subtotal', 'tax_rate', 'tax_amount', 'discount_amount', 
                  'total', 'notes', 'terms', 'items', 'payments', 'created_at', 'updated_at']
        read_only_fields = ['id', 'invoice_number', 'tax_amount', 'total', 'created_at', 'updated_at']
    
    def get_payments(self, obj):
        return PaymentSerializer(obj.payments.all(), many=True).data


class InvoiceCreateSerializer(serializers.ModelSerializer):
    """Serializer for creating invoices"""
    
    items = InvoiceItemSerializer(many=True, required=False)
    
    class Meta:
        model = Invoice
        fields = ['client', 'issue_date', 'due_date', 'tax_rate', 'discount_amount', 
                  'notes', 'terms', 'items']
    
    def create(self, validated_data):
        items_data = validated_data.pop('items', [])
        invoice = Invoice.objects.create(**validated_data)
        
        for item_data in items_data:
            InvoiceItem.objects.create(invoice=invoice, **item_data)
        
        return invoice


class PaymentSerializer(serializers.ModelSerializer):
    """Serializer for Payment model"""
    
    class Meta:
        model = Payment
        fields = ['id', 'invoice', 'payment_method', 'amount', 'status', 'transaction_id', 
                  'payment_date', 'notes', 'metadata', 'created_at', 'updated_at']
        read_only_fields = ['id', 'payment_date', 'created_at', 'updated_at']


class BillingProfileSerializer(serializers.ModelSerializer):
    """Serializer for BillingProfile model"""
    
    class Meta:
        model = BillingProfile
        fields = ['id', 'client', 'auto_pay', 'payment_method', 'billing_email', 
                  'billing_phone', 'billing_address', 'stripe_customer_id', 'paypal_email', 
                  'created_at', 'updated_at']
        read_only_fields = ['id', 'created_at', 'updated_at']
