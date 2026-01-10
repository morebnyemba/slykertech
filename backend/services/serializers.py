from rest_framework import serializers
from .models import Service, ServiceSubscription, DNSRecord
from clients.serializers import ClientSerializer


class ServiceSerializer(serializers.ModelSerializer):
    """Serializer for Service model"""
    
    class Meta:
        model = Service
        fields = ['id', 'name', 'category', 'description', 'features', 'base_price', 
                  'is_active', 'created_at', 'updated_at']
        read_only_fields = ['id', 'created_at', 'updated_at']


class DNSRecordSerializer(serializers.ModelSerializer):
    """Serializer for DNSRecord model"""
    
    class Meta:
        model = DNSRecord
        fields = ['id', 'domain', 'record_type', 'name', 'content', 'ttl', 'priority', 
                  'is_active', 'external_id', 'created_at', 'updated_at']
        read_only_fields = ['id', 'external_id', 'created_at', 'updated_at']


class ServiceSubscriptionSerializer(serializers.ModelSerializer):
    """Serializer for ServiceSubscription model"""
    
    client = ClientSerializer(read_only=True)
    service = ServiceSerializer(read_only=True)
    dns_records = DNSRecordSerializer(many=True, read_only=True)
    
    class Meta:
        model = ServiceSubscription
        fields = ['id', 'client', 'service', 'status', 'billing_cycle', 'price', 
                  'start_date', 'end_date', 'auto_renew', 'notes', 'metadata', 
                  'dns_records', 'created_at', 'updated_at']
        read_only_fields = ['id', 'created_at', 'updated_at']


class ServiceSubscriptionCreateSerializer(serializers.ModelSerializer):
    """Serializer for creating ServiceSubscription"""
    
    class Meta:
        model = ServiceSubscription
        fields = ['client', 'service', 'status', 'billing_cycle', 'price', 
                  'start_date', 'end_date', 'auto_renew', 'notes', 'metadata']
