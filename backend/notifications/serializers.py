from rest_framework import serializers
from .models import Notification, WhatsAppMessage, NotificationTemplate, NotificationPreference
from accounts.serializers import UserSerializer


class NotificationSerializer(serializers.ModelSerializer):
    """Serializer for Notification model"""
    
    user = UserSerializer(read_only=True)
    
    class Meta:
        model = Notification
        fields = ['id', 'user', 'notification_type', 'category', 'status', 'title', 
                  'message', 'link_url', 'link_text', 'metadata', 'sent_at', 
                  'delivered_at', 'read_at', 'created_at']
        read_only_fields = ['id', 'sent_at', 'delivered_at', 'read_at', 'created_at']


class WhatsAppMessageSerializer(serializers.ModelSerializer):
    """Serializer for WhatsAppMessage model"""
    
    class Meta:
        model = WhatsAppMessage
        fields = ['id', 'notification', 'recipient', 'message', 'status', 'message_id', 
                  'error_message', 'sent_at', 'delivered_at', 'read_at', 'created_at', 'updated_at']
        read_only_fields = ['id', 'message_id', 'sent_at', 'delivered_at', 'read_at', 
                           'created_at', 'updated_at']


class NotificationTemplateSerializer(serializers.ModelSerializer):
    """Serializer for NotificationTemplate model"""
    
    class Meta:
        model = NotificationTemplate
        fields = ['id', 'name', 'notification_type', 'category', 'subject', 'template', 
                  'is_active', 'created_at', 'updated_at']
        read_only_fields = ['id', 'created_at', 'updated_at']


class NotificationPreferenceSerializer(serializers.ModelSerializer):
    """Serializer for NotificationPreference model"""
    
    class Meta:
        model = NotificationPreference
        fields = ['id', 'user', 'email_enabled', 'whatsapp_enabled', 'sms_enabled', 
                  'in_app_enabled', 'invoice_notifications', 'payment_notifications', 
                  'service_notifications', 'account_notifications', 'system_notifications', 
                  'marketing_notifications', 'created_at', 'updated_at']
        read_only_fields = ['id', 'created_at', 'updated_at']
