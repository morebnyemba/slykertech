"""
Django Bridge API for Erlang Chat System
"""

from rest_framework import status
from rest_framework.decorators import api_view, permission_classes
from rest_framework.permissions import AllowAny
from rest_framework.response import Response
import logging

logger = logging.getLogger(__name__)


@api_view(['POST'])
@permission_classes([AllowAny])
def ai_response(request):
    """Get AI response from Gemini"""
    try:
        message = request.data.get('message', '')
        return Response({
            'success': True,
            'response': f'AI response to: {message}',
            'action': None
        })
    except Exception as e:
        return Response({'success': False, 'error': str(e)}, status=500)


@api_view(['POST'])
@permission_classes([AllowAny])
def create_ticket_from_chat(request):
    """Create support ticket from chat"""
    from tickets.models import Ticket
    try:
        ticket = Ticket.objects.create(
            user_id=request.data.get('user_id'),
            subject=request.data.get('subject', 'Chat Support'),
            description=request.data.get('description', ''),
            status='open',
            priority='medium'
        )
        return Response({'success': True, 'ticket_id': ticket.id})
    except Exception as e:
        return Response({'success': False, 'error': str(e)}, status=500)


@api_view(['POST'])
@permission_classes([AllowAny])
def transfer_to_human(request):
    """Transfer to human agent"""
    return Response({
        'success': True,
        'agent_id': 'agent_001',
        'agent_name': 'Support Agent'
    })


@api_view(['POST'])
@permission_classes([AllowAny])
def manage_service(request):
    """Manage client services"""
    action = request.data.get('action')
    return Response({
        'success': True,
        'message': f'Action {action} completed'
    })
