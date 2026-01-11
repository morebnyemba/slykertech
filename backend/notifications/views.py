from rest_framework import viewsets, permissions, status
from rest_framework.decorators import action
from rest_framework.response import Response
from .models import Notification, WhatsAppMessage, NotificationTemplate, NotificationPreference
from .serializers import (
    NotificationSerializer, WhatsAppMessageSerializer,
    NotificationTemplateSerializer, NotificationPreferenceSerializer
)
from .whatsapp_service import WhatsAppService


class NotificationViewSet(viewsets.ModelViewSet):
    """ViewSet for Notification model"""
    
    queryset = Notification.objects.all()
    serializer_class = NotificationSerializer
    permission_classes = [permissions.IsAuthenticated]
    
    def get_queryset(self):
        """Filter notifications for current user"""
        user = self.request.user
        if user.is_superuser or user.user_type == 'admin':
            return Notification.objects.all()
        return Notification.objects.filter(user=user)
    
    @action(detail=True, methods=['post'])
    def mark_read(self, request, pk=None):
        """Mark notification as read"""
        notification = self.get_object()
        notification.mark_as_read()
        return Response(
            {"message": "Notification marked as read."},
            status=status.HTTP_200_OK
        )
    
    @action(detail=False, methods=['post'])
    def mark_all_read(self, request):
        """Mark all notifications as read for current user"""
        notifications = Notification.objects.filter(user=request.user, status__in=['sent', 'delivered'])
        for notification in notifications:
            notification.mark_as_read()
        
        return Response(
            {"message": f"Marked {notifications.count()} notifications as read."},
            status=status.HTTP_200_OK
        )
    
    @action(detail=False, methods=['get'])
    def unread_count(self, request):
        """Get count of unread notifications"""
        count = Notification.objects.filter(
            user=request.user,
            status__in=['sent', 'delivered']
        ).count()
        
        return Response({"unread_count": count})


class WhatsAppMessageViewSet(viewsets.ModelViewSet):
    """ViewSet for WhatsAppMessage model"""
    
    queryset = WhatsAppMessage.objects.all()
    serializer_class = WhatsAppMessageSerializer
    permission_classes = [permissions.IsAuthenticated]
    
    def get_queryset(self):
        """Filter WhatsApp messages based on user"""
        user = self.request.user
        if user.is_superuser or user.user_type == 'admin':
            return WhatsAppMessage.objects.all()
        return WhatsAppMessage.objects.filter(notification__user=user)
    
    @action(detail=False, methods=['post'])
    def send_message(self, request):
        """Send a WhatsApp message"""
        recipient = request.data.get('recipient')
        message = request.data.get('message')
        
        if not recipient or not message:
            return Response(
                {"error": "recipient and message are required"},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Send via WhatsApp service
        whatsapp_service = WhatsAppService()
        result = whatsapp_service.send_message(recipient, message)
        
        if result.get('success'):
            # Create WhatsApp message record
            whatsapp_msg = WhatsAppMessage.objects.create(
                recipient=recipient,
                message=message,
                status='sent',
                message_id=result.get('message_id')
            )
            
            serializer = self.get_serializer(whatsapp_msg)
            return Response(serializer.data, status=status.HTTP_201_CREATED)
        else:
            return Response(
                {"error": result.get('error')},
                status=status.HTTP_400_BAD_REQUEST
            )


class NotificationTemplateViewSet(viewsets.ModelViewSet):
    """ViewSet for NotificationTemplate model"""
    
    queryset = NotificationTemplate.objects.all()
    serializer_class = NotificationTemplateSerializer
    permission_classes = [permissions.IsAuthenticated]
    
    def get_queryset(self):
        """Admin/staff can see all templates"""
        user = self.request.user
        if user.is_superuser or user.user_type in ['admin', 'staff']:
            return NotificationTemplate.objects.all()
        return NotificationTemplate.objects.filter(is_active=True)


class NotificationPreferenceViewSet(viewsets.ModelViewSet):
    """ViewSet for NotificationPreference model"""
    
    queryset = NotificationPreference.objects.all()
    serializer_class = NotificationPreferenceSerializer
    permission_classes = [permissions.IsAuthenticated]
    
    def get_queryset(self):
        """Filter preferences for current user"""
        user = self.request.user
        if user.is_superuser or user.user_type == 'admin':
            return NotificationPreference.objects.all()
        return NotificationPreference.objects.filter(user=user)
    
    @action(detail=False, methods=['get'])
    def my_preferences(self, request):
        """Get current user's notification preferences"""
        try:
            preference = NotificationPreference.objects.get(user=request.user)
            serializer = self.get_serializer(preference)
            return Response(serializer.data)
        except NotificationPreference.DoesNotExist:
            # Create default preferences
            preference = NotificationPreference.objects.create(user=request.user)
            serializer = self.get_serializer(preference)
            return Response(serializer.data, status=status.HTTP_201_CREATED)

