from rest_framework import serializers
from .models import Invoice, InvoiceItem, Payment, BillingProfile, Cart, CartItem
from clients.serializers import ClientSerializer


class CartItemSerializer(serializers.ModelSerializer):
    """Serializer for CartItem model"""
    
    service_name = serializers.CharField(source='service.name', read_only=True)
    service_category = serializers.CharField(source='service.category', read_only=True)
    total_price = serializers.SerializerMethodField()
    # Accept domain_product ID for domain-related cart items
    domain_product = serializers.IntegerField(write_only=True, required=False)
    # Accept hosting_product ID for hosting-related cart items
    hosting_product = serializers.IntegerField(write_only=True, required=False)
    
    class Meta:
        model = CartItem
        fields = ['id', 'cart', 'service', 'service_name', 'service_category', 
                  'service_metadata', 'quantity', 'unit_price', 'billing_cycle', 
                  'total_price', 'created_at', 'domain_product', 'hosting_product']
        read_only_fields = ['id', 'cart', 'created_at', 'total_price']
        extra_kwargs = {
            'service': {'required': False}  # Not required when domain_product or hosting_product is provided
        }
    
    def get_total_price(self, obj):
        return float(obj.get_price())
    
    def validate(self, attrs):
        """Validate and handle domain_product and hosting_product fields"""
        domain_product_id = attrs.pop('domain_product', None)
        hosting_product_id = attrs.pop('hosting_product', None)
        
        if hosting_product_id is not None:
            # Handle hosting product - get or create hosting service
            from services.models import Service
            from services.whmcs_models import HostingProduct
            
            # Verify the hosting product exists
            try:
                hosting_product = HostingProduct.objects.get(id=hosting_product_id)
            except HostingProduct.DoesNotExist:
                raise serializers.ValidationError({
                    'hosting_product': f'Hosting product with ID {hosting_product_id} not found'
                })
            
            # Get or create a service for this hosting type
            hosting_type_map = {
                'shared': 'Shared Web Hosting',
                'vps': 'VPS Hosting',
                'dedicated': 'Dedicated Server Hosting',
                'cloud': 'Cloud Hosting',
                'reseller': 'Reseller Hosting',
            }
            service_name = hosting_type_map.get(hosting_product.hosting_type, 'Web Hosting')
            
            service, created = Service.objects.get_or_create(
                name=service_name,
                category='hosting',
                defaults={
                    'description': f'{service_name} - {hosting_product.description}',
                    'is_active': True,
                    'payment_type': 'recurring',
                }
            )
            
            attrs['service'] = service
            
            # Store hosting_product details in service_metadata
            service_metadata = attrs.get('service_metadata', {})
            if service_metadata is None:
                service_metadata = {}
            service_metadata['hosting_product_id'] = hosting_product_id
            service_metadata['hosting_product_name'] = hosting_product.name
            service_metadata['hosting_type'] = hosting_product.hosting_type
            attrs['service_metadata'] = service_metadata
            
            # Set unit_price if not already set
            if 'unit_price' not in attrs or attrs['unit_price'] is None:
                billing_cycle = attrs.get('billing_cycle', 'monthly')
                price = hosting_product.get_price(billing_cycle)
                if price:
                    attrs['unit_price'] = price
                else:
                    attrs['unit_price'] = hosting_product.monthly_price
        
        elif domain_product_id is not None:
            # Handle domain product - get or create domain service
            from services.models import Service
            from services.whmcs_models import DomainProduct
            
            # Verify the domain product exists
            try:
                domain_product = DomainProduct.objects.get(id=domain_product_id)
            except DomainProduct.DoesNotExist:
                raise serializers.ValidationError({
                    'domain_product': f'Domain product with ID {domain_product_id} not found'
                })
            
            # Get or create a generic Domain Registration service
            service, created = Service.objects.get_or_create(
                name='Domain Registration',
                category='domain',
                defaults={
                    'description': 'Domain name registration and management services',
                    'is_active': True,
                    'payment_type': 'recurring',
                }
            )
            
            attrs['service'] = service
            
            # Store domain_product ID in service_metadata
            service_metadata = attrs.get('service_metadata', {})
            if service_metadata is None:
                service_metadata = {}
            service_metadata['domain_product_id'] = domain_product_id
            service_metadata['domain_product_tld'] = domain_product.tld
            attrs['service_metadata'] = service_metadata
        
        # Ensure we have a service (either from service field, domain_product, or hosting_product)
        if 'service' not in attrs or attrs.get('service') is None:
            raise serializers.ValidationError({
                'service': 'Either service, domain_product, or hosting_product must be provided'
            })
        
        return attrs
    
    def _validate_domain_transfer_epp_code(self, metadata):
        """Helper to validate EPP code is provided for domain transfers"""
        if metadata.get('action') == 'transfer' and not metadata.get('epp_code'):
            raise serializers.ValidationError(
                'EPP/Auth code is required for domain transfers'
            )
    
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
        
        # Validate domain transfer EPP code for domain services or domain_product
        is_domain_service = service and service.category == 'domain'
        is_domain_product = 'domain_product' in self.initial_data
        
        if is_domain_service or is_domain_product:
            self._validate_domain_transfer_epp_code(value)
        
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
