from rest_framework import viewsets, permissions, status
from rest_framework.decorators import action
from rest_framework.response import Response
from django.db.models import Count, Q
from django.utils import timezone
from .models import Ticket, TicketReply
from .serializers import (
    TicketSerializer, TicketListSerializer, TicketCreateSerializer,
    TicketUpdateSerializer, TicketReplySerializer, TicketReplyCreateSerializer
)


class IsStaffOrOwner(permissions.BasePermission):
    """Permission to allow staff or ticket owner"""
    
    def has_permission(self, request, view):
        return request.user and request.user.is_authenticated
    
    def has_object_permission(self, request, view, obj):
        # Staff can access all tickets
        if request.user.is_staff or request.user.is_superuser:
            return True
        if getattr(request.user, 'user_type', '') == 'admin':
            return True
        # Owner can access their tickets
        return obj.user == request.user


class TicketViewSet(viewsets.ModelViewSet):
    """ViewSet for managing support tickets"""
    
    queryset = Ticket.objects.all()
    permission_classes = [IsStaffOrOwner]
    
    def get_queryset(self):
        user = self.request.user
        queryset = Ticket.objects.all()
        
        # Staff can see all tickets
        if user.is_staff or user.is_superuser or getattr(user, 'user_type', '') in ['admin', 'staff']:
            pass
        else:
            # Clients can only see their own tickets
            queryset = queryset.filter(user=user)
        
        # Apply filters
        status_filter = self.request.query_params.get('status')
        priority_filter = self.request.query_params.get('priority')
        department_filter = self.request.query_params.get('department')
        assigned_to_me = self.request.query_params.get('assigned_to_me')
        
        if status_filter:
            queryset = queryset.filter(status=status_filter)
        if priority_filter:
            queryset = queryset.filter(priority=priority_filter)
        if department_filter:
            queryset = queryset.filter(department=department_filter)
        if assigned_to_me == 'true':
            queryset = queryset.filter(assigned_to=user)
        
        return queryset.order_by('-created_at')
    
    def get_serializer_class(self):
        if self.action == 'list':
            return TicketListSerializer
        if self.action == 'create':
            return TicketCreateSerializer
        if self.action in ['update', 'partial_update']:
            return TicketUpdateSerializer
        return TicketSerializer
    
    def perform_create(self, serializer):
        serializer.save(user=self.request.user)
    
    @action(detail=False, methods=['get'])
    def stats(self, request):
        """Get ticket statistics for admin dashboard"""
        user = request.user
        
        if not (user.is_staff or user.is_superuser or getattr(user, 'user_type', '') in ['admin', 'staff']):
            return Response({'error': 'Not authorized'}, status=status.HTTP_403_FORBIDDEN)
        
        stats = {
            'total': Ticket.objects.count(),
            'open': Ticket.objects.filter(status='open').count(),
            'in_progress': Ticket.objects.filter(status='in_progress').count(),
            'pending': Ticket.objects.filter(status='pending').count(),
            'resolved': Ticket.objects.filter(status='resolved').count(),
            'closed': Ticket.objects.filter(status='closed').count(),
            'by_priority': {
                'critical': Ticket.objects.filter(priority='critical', status__in=['open', 'in_progress']).count(),
                'high': Ticket.objects.filter(priority='high', status__in=['open', 'in_progress']).count(),
                'medium': Ticket.objects.filter(priority='medium', status__in=['open', 'in_progress']).count(),
                'low': Ticket.objects.filter(priority='low', status__in=['open', 'in_progress']).count(),
            },
            'assigned_to_me': Ticket.objects.filter(assigned_to=user).exclude(status__in=['resolved', 'closed']).count(),
            'unassigned': Ticket.objects.filter(assigned_to__isnull=True, status='open').count(),
        }
        
        return Response(stats)
    
    @action(detail=True, methods=['post'])
    def assign_to_me(self, request, pk=None):
        """Assign ticket to current user"""
        ticket = self.get_object()
        ticket.assigned_to = request.user
        if ticket.status == 'open':
            ticket.status = 'in_progress'
        ticket.save()
        return Response({'message': 'Ticket assigned to you', 'status': ticket.status})
    
    @action(detail=True, methods=['post'])
    def close(self, request, pk=None):
        """Close a ticket"""
        ticket = self.get_object()
        ticket.status = 'closed'
        ticket.save()
        return Response({'message': 'Ticket closed'})
    
    @action(detail=True, methods=['post'])
    def reopen(self, request, pk=None):
        """Reopen a ticket"""
        ticket = self.get_object()
        ticket.status = 'open'
        ticket.save()
        return Response({'message': 'Ticket reopened'})


class TicketReplyViewSet(viewsets.ModelViewSet):
    """ViewSet for ticket replies"""
    
    queryset = TicketReply.objects.all()
    permission_classes = [IsStaffOrOwner]
    
    def get_queryset(self):
        user = self.request.user
        ticket_id = self.request.query_params.get('ticket')
        
        queryset = TicketReply.objects.all()
        
        if ticket_id:
            queryset = queryset.filter(ticket_id=ticket_id)
        
        # Non-staff users shouldn't see internal notes
        if not (user.is_staff or user.is_superuser or getattr(user, 'user_type', '') in ['admin', 'staff']):
            queryset = queryset.filter(is_internal=False)
        
        return queryset.order_by('created_at')
    
    def get_serializer_class(self):
        if self.action == 'create':
            return TicketReplyCreateSerializer
        return TicketReplySerializer
    
    def perform_create(self, serializer):
        user = self.request.user
        is_staff = user.is_staff or user.is_superuser or getattr(user, 'user_type', '') in ['admin', 'staff']
        
        reply = serializer.save(user=user, is_staff_reply=is_staff)
        
        # Update ticket status and timestamp
        ticket = reply.ticket
        if is_staff and ticket.status == 'open':
            ticket.status = 'in_progress'
        ticket.save()
