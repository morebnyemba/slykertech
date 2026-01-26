from rest_framework import viewsets, permissions, status
from rest_framework.decorators import action
from rest_framework.response import Response
from django.utils import timezone
from .models import ChatSession, ChatMessage
from .serializers import (
    ChatSessionSerializer, ChatSessionListSerializer, ChatMessageSerializer
)


class IsStaffUser(permissions.BasePermission):
    """Permission class for staff users"""
    
    def has_permission(self, request, view):
        return request.user and request.user.is_authenticated and (
            request.user.is_staff or request.user.is_superuser or
            getattr(request.user, 'user_type', '') in ['admin', 'staff']
        )


class ChatSessionViewSet(viewsets.ModelViewSet):
    """ViewSet for managing chat sessions (admin)"""
    
    queryset = ChatSession.objects.all()
    serializer_class = ChatSessionSerializer
    permission_classes = [IsStaffUser]
    
    def get_queryset(self):
        queryset = ChatSession.objects.all().order_by('-created_at')
        
        # Apply filters
        status_filter = self.request.query_params.get('status')
        if status_filter:
            queryset = queryset.filter(status=status_filter)
        
        return queryset
    
    def get_serializer_class(self):
        if self.action == 'list':
            return ChatSessionListSerializer
        return ChatSessionSerializer
    
    @action(detail=False, methods=['get'])
    def active(self, request):
        """Get all active chat sessions"""
        sessions = ChatSession.objects.filter(status='active').order_by('-created_at')
        serializer = ChatSessionListSerializer(sessions, many=True)
        return Response(serializer.data)
    
    @action(detail=False, methods=['get'])
    def stats(self, request):
        """Get chat statistics"""
        now = timezone.now()
        today_start = now.replace(hour=0, minute=0, second=0, microsecond=0)
        
        stats = {
            'total_sessions': ChatSession.objects.count(),
            'active_sessions': ChatSession.objects.filter(status='active').count(),
            'today_sessions': ChatSession.objects.filter(created_at__gte=today_start).count(),
            'total_messages': ChatMessage.objects.count(),
            'agent_messages': ChatMessage.objects.filter(sender_type='agent').count(),
            'ai_messages': ChatMessage.objects.filter(sender_type='ai').count(),
        }
        return Response(stats)
    
    @action(detail=True, methods=['post'])
    def close_session(self, request, pk=None):
        """Close a chat session"""
        session = self.get_object()
        session.status = 'closed'
        session.closed_at = timezone.now()
        session.save()
        return Response({'message': 'Session closed'})
    
    @action(detail=True, methods=['post'])
    def send_message(self, request, pk=None):
        """Send a message as agent"""
        session = self.get_object()
        message = request.data.get('message')
        
        if not message:
            return Response({'error': 'Message is required'}, status=status.HTTP_400_BAD_REQUEST)
        
        chat_message = ChatMessage.objects.create(
            session=session,
            sender_type='agent',
            message=message
        )
        
        serializer = ChatMessageSerializer(chat_message)
        return Response(serializer.data, status=status.HTTP_201_CREATED)


class ChatMessageViewSet(viewsets.ReadOnlyModelViewSet):
    """ViewSet for viewing chat messages (admin)"""
    
    queryset = ChatMessage.objects.all()
    serializer_class = ChatMessageSerializer
    permission_classes = [IsStaffUser]
    
    def get_queryset(self):
        session_id = self.request.query_params.get('session')
        queryset = ChatMessage.objects.all()
        
        if session_id:
            queryset = queryset.filter(session_id=session_id)
        
        return queryset.order_by('created_at')
