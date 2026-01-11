from rest_framework import serializers
from .models import APIConfiguration, IntegrationCredential, cPanelAccount, DirectAdminAccount


class APIConfigurationSerializer(serializers.ModelSerializer):
    """Serializer for APIConfiguration model"""
    
    api_key = serializers.CharField(write_only=True, required=False, allow_blank=True)
    api_secret = serializers.CharField(write_only=True, required=False, allow_blank=True)
    access_token = serializers.CharField(write_only=True, required=False, allow_blank=True)
    
    class Meta:
        model = APIConfiguration
        fields = ['id', 'provider', 'name', 'api_url', 'api_key', 'api_secret', 
                  'access_token', 'config_data', 'is_active', 'is_sandbox', 
                  'created_at', 'updated_at']
        read_only_fields = ['id', 'created_at', 'updated_at']
        extra_kwargs = {
            'encrypted_api_key': {'write_only': True},
            'encrypted_api_secret': {'write_only': True},
            'encrypted_access_token': {'write_only': True},
        }
    
    def create(self, validated_data):
        api_key = validated_data.pop('api_key', None)
        api_secret = validated_data.pop('api_secret', None)
        access_token = validated_data.pop('access_token', None)
        
        instance = APIConfiguration.objects.create(**validated_data)
        
        if api_key:
            instance.set_api_key(api_key)
        if api_secret:
            instance.set_api_secret(api_secret)
        if access_token:
            instance.set_access_token(access_token)
        
        instance.save()
        return instance
    
    def update(self, instance, validated_data):
        api_key = validated_data.pop('api_key', None)
        api_secret = validated_data.pop('api_secret', None)
        access_token = validated_data.pop('access_token', None)
        
        for attr, value in validated_data.items():
            setattr(instance, attr, value)
        
        if api_key:
            instance.set_api_key(api_key)
        if api_secret:
            instance.set_api_secret(api_secret)
        if access_token:
            instance.set_access_token(access_token)
        
        instance.save()
        return instance


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
