from rest_framework import serializers
from .models import InvestmentPackage, Investment, BankingDetails

class InvestmentPackageSerializer(serializers.ModelSerializer):
    investment_count = serializers.SerializerMethodField()
    
    class Meta:
        model = InvestmentPackage
        fields = [
            'id', 'name', 'description', 'minimum_amount', 'expected_return',
            'duration_months', 'is_active', 'investment_count', 'created_at', 'updated_at'
        ]
        read_only_fields = ['created_at', 'updated_at']
    
    def get_investment_count(self, obj):
        return obj.investments.filter(status='active').count()

class InvestmentSerializer(serializers.ModelSerializer):
    package_name = serializers.CharField(source='package.name', read_only=True)
    investor_email = serializers.CharField(source='investor.email', read_only=True)
    investor_name = serializers.CharField(source='investor.full_name', read_only=True)
    calculated_value = serializers.SerializerMethodField()
    
    class Meta:
        model = Investment
        fields = [
            'id', 'investor', 'investor_email', 'investor_name', 'package', 'package_name',
            'amount', 'start_date', 'maturity_date', 'status', 'current_value',
            'calculated_value', 'created_at', 'updated_at'
        ]
        read_only_fields = ['investor', 'maturity_date', 'current_value', 'created_at', 'updated_at']
    
    def get_calculated_value(self, obj):
        return float(obj.calculate_current_value())
    
    def create(self, validated_data):
        validated_data['investor'] = self.context['request'].user
        return super().create(validated_data)

class InvestmentCreateSerializer(serializers.ModelSerializer):
    class Meta:
        model = Investment
        fields = ['package', 'amount', 'start_date']
    
    def validate_amount(self, value):
        package = self.initial_data.get('package')
        if package:
            try:
                pkg = InvestmentPackage.objects.get(id=package)
                if value < pkg.minimum_amount:
                    raise serializers.ValidationError(
                        f"Amount must be at least {pkg.minimum_amount}"
                    )
            except InvestmentPackage.DoesNotExist:
                pass
        return value
    
    def create(self, validated_data):
        validated_data['investor'] = self.context['request'].user
        return super().create(validated_data)

class BankingDetailsSerializer(serializers.ModelSerializer):
    investor_email = serializers.CharField(source='investor.email', read_only=True)
    
    class Meta:
        model = BankingDetails
        fields = [
            'id', 'investor', 'investor_email', 'bank_name', 'account_number',
            'account_holder_name', 'swift_code', 'branch_code', 'is_primary',
            'created_at', 'updated_at'
        ]
        read_only_fields = ['investor', 'created_at', 'updated_at']
    
    def create(self, validated_data):
        validated_data['investor'] = self.context['request'].user
        return super().create(validated_data)
