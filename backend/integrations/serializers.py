from rest_framework import serializers
from .models import IntegrationCredential, cPanelAccount, DirectAdminAccount


class IntegrationCredentialSerializer(serializers.ModelSerializer):
    """Serializer for IntegrationCredential model"""
    
    password = serializers.CharField(write_only=True, required=False, allow_blank=True)
    api_token = serializers.CharField(write_only=True, required=False, allow_blank=True)
    
    class Meta:
        model = IntegrationCredential
        fields = ['id', 'provider', 'name', 'host', 'username', 'password', 'api_token', 
                  'port', 'is_active', 'metadata', 'last_verified', 'created_at', 'updated_at']
        read_only_fields = ['id', 'last_verified', 'created_at', 'updated_at']
        extra_kwargs = {
            'encrypted_password': {'write_only': True},
            'encrypted_api_token': {'write_only': True},
        }
    
    def create(self, validated_data):
        password = validated_data.pop('password', None)
        api_token = validated_data.pop('api_token', None)
        
        instance = IntegrationCredential.objects.create(**validated_data)
        
        if password:
            instance.set_password(password)
        if api_token:
            instance.set_api_token(api_token)
        
        instance.save()
        return instance
    
    def update(self, instance, validated_data):
        password = validated_data.pop('password', None)
        api_token = validated_data.pop('api_token', None)
        
        for attr, value in validated_data.items():
            setattr(instance, attr, value)
        
        if password:
            instance.set_password(password)
        if api_token:
            instance.set_api_token(api_token)
        
        instance.save()
        return instance


class cPanelAccountSerializer(serializers.ModelSerializer):
    """Serializer for cPanelAccount model"""
    
    credential = IntegrationCredentialSerializer(read_only=True)
    
    class Meta:
        model = cPanelAccount
        fields = ['id', 'credential', 'subscription', 'cpanel_username', 'domain', 
                  'disk_quota', 'disk_used', 'bandwidth_quota', 'bandwidth_used', 
                  'email_accounts', 'databases', 'subdomains', 'is_suspended', 
                  'last_sync', 'created_at', 'updated_at']
        read_only_fields = ['id', 'last_sync', 'created_at', 'updated_at']


class DirectAdminAccountSerializer(serializers.ModelSerializer):
    """Serializer for DirectAdminAccount model"""
    
    credential = IntegrationCredentialSerializer(read_only=True)
    
    class Meta:
        model = DirectAdminAccount
        fields = ['id', 'credential', 'subscription', 'da_username', 'domain', 
                  'disk_quota', 'disk_used', 'bandwidth_quota', 'bandwidth_used', 
                  'email_accounts', 'databases', 'subdomains', 'is_suspended', 
                  'last_sync', 'created_at', 'updated_at']
        read_only_fields = ['id', 'last_sync', 'created_at', 'updated_at']
