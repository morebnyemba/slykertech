from rest_framework import serializers
from .models import Client, ClientContact
from accounts.serializers import UserSerializer


class ClientContactSerializer(serializers.ModelSerializer):
    """Serializer for ClientContact model"""
    
    class Meta:
        model = ClientContact
        fields = ['id', 'name', 'email', 'phone', 'position', 'is_primary', 'created_at']
        read_only_fields = ['id', 'created_at']


class ClientSerializer(serializers.ModelSerializer):
    """Serializer for Client model"""
    
    user = UserSerializer(read_only=True)
    contacts = ClientContactSerializer(many=True, read_only=True)
    
    class Meta:
        model = Client
        fields = ['id', 'user', 'company_name', 'company_website', 'address', 'city', 
                  'country', 'billing_email', 'tax_id', 'is_active', 'notes', 
                  'contacts', 'created_at', 'updated_at']
        read_only_fields = ['id', 'created_at', 'updated_at']


class ClientCreateSerializer(serializers.ModelSerializer):
    """Serializer for creating Client with user details"""
    
    class Meta:
        model = Client
        fields = ['company_name', 'company_website', 'address', 'city', 'country', 
                  'billing_email', 'tax_id', 'notes']
