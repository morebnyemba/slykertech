from rest_framework import serializers
from .models import ChatSession, ChatMessage
from accounts.serializers import UserSerializer


class ChatMessageSerializer(serializers.ModelSerializer):
    """Serializer for chat messages"""
    
    class Meta:
        model = ChatMessage
        fields = ['id', 'session', 'sender_type', 'message', 'created_at']
        read_only_fields = ['id', 'created_at']


class ChatSessionSerializer(serializers.ModelSerializer):
    """Serializer for chat sessions"""
    
    user = UserSerializer(read_only=True)
    messages = ChatMessageSerializer(many=True, read_only=True)
    message_count = serializers.SerializerMethodField()
    last_message = serializers.SerializerMethodField()
    
    class Meta:
        model = ChatSession
        fields = ['id', 'user', 'session_id', 'visitor_name', 'department', 'source', 'status', 'messages', 
                  'message_count', 'last_message', 'created_at', 'closed_at', 'last_active_at']
        read_only_fields = ['id', 'session_id', 'created_at']
    
    def get_message_count(self, obj):
        return obj.messages.count()
    
    def get_last_message(self, obj):
        last_msg = obj.messages.order_by('-created_at').first()
        if last_msg:
            return {
                'message': last_msg.message[:100],
                'sender_type': last_msg.sender_type,
                'created_at': last_msg.created_at
            }
        return None


class ChatSessionListSerializer(serializers.ModelSerializer):
    """Lightweight serializer for session lists"""
    
    user = UserSerializer(read_only=True)
    message_count = serializers.SerializerMethodField()
    last_message_at = serializers.SerializerMethodField()
    
    class Meta:
        model = ChatSession
        fields = ['id', 'user', 'session_id', 'visitor_name', 'department', 'source', 'status', 'message_count', 
                  'last_message_at', 'created_at', 'closed_at', 'last_active_at']
    
    def get_message_count(self, obj):
        return obj.messages.count()
    
    def get_last_message_at(self, obj):
        last_msg = obj.messages.order_by('-created_at').first()
        return last_msg.created_at if last_msg else None
