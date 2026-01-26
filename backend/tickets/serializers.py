from rest_framework import serializers
from .models import Ticket, TicketReply
from accounts.serializers import UserSerializer


class TicketReplySerializer(serializers.ModelSerializer):
    """Serializer for ticket replies"""
    
    user = UserSerializer(read_only=True)
    
    class Meta:
        model = TicketReply
        fields = ['id', 'ticket', 'user', 'message', 'is_staff_reply', 
                  'is_internal', 'attachments', 'created_at', 'updated_at']
        read_only_fields = ['id', 'user', 'is_staff_reply', 'created_at', 'updated_at']


class TicketReplyCreateSerializer(serializers.ModelSerializer):
    """Serializer for creating ticket replies"""
    
    class Meta:
        model = TicketReply
        fields = ['ticket', 'message', 'is_internal', 'attachments']


class TicketSerializer(serializers.ModelSerializer):
    """Serializer for tickets with replies"""
    
    user = UserSerializer(read_only=True)
    assigned_to = UserSerializer(read_only=True)
    replies = TicketReplySerializer(many=True, read_only=True)
    reply_count = serializers.SerializerMethodField()
    
    class Meta:
        model = Ticket
        fields = ['id', 'user', 'assigned_to', 'subject', 'description', 
                  'department', 'status', 'priority', 'metadata', 
                  'replies', 'reply_count', 'created_at', 'updated_at']
        read_only_fields = ['id', 'user', 'created_at', 'updated_at']
    
    def get_reply_count(self, obj):
        return obj.replies.count()


class TicketListSerializer(serializers.ModelSerializer):
    """Lightweight serializer for ticket lists"""
    
    user = UserSerializer(read_only=True)
    assigned_to = UserSerializer(read_only=True)
    reply_count = serializers.SerializerMethodField()
    last_reply_at = serializers.SerializerMethodField()
    
    class Meta:
        model = Ticket
        fields = ['id', 'user', 'assigned_to', 'subject', 'department', 
                  'status', 'priority', 'reply_count', 'last_reply_at',
                  'created_at', 'updated_at']
    
    def get_reply_count(self, obj):
        return obj.replies.count()
    
    def get_last_reply_at(self, obj):
        last_reply = obj.replies.order_by('-created_at').first()
        return last_reply.created_at if last_reply else None


class TicketCreateSerializer(serializers.ModelSerializer):
    """Serializer for creating tickets"""
    
    class Meta:
        model = Ticket
        fields = ['subject', 'description', 'department', 'priority', 'metadata']


class TicketUpdateSerializer(serializers.ModelSerializer):
    """Serializer for updating tickets (admin)"""
    
    assigned_to_id = serializers.IntegerField(required=False, allow_null=True)
    
    class Meta:
        model = Ticket
        fields = ['status', 'priority', 'assigned_to_id', 'department']
    
    def update(self, instance, validated_data):
        assigned_to_id = validated_data.pop('assigned_to_id', None)
        if assigned_to_id is not None:
            from django.contrib.auth import get_user_model
            User = get_user_model()
            try:
                instance.assigned_to = User.objects.get(id=assigned_to_id)
            except User.DoesNotExist:
                pass
        return super().update(instance, validated_data)
