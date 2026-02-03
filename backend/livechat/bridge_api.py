"""
Django Bridge API for Erlang Chat System
"""

from rest_framework.decorators import api_view, permission_classes
from rest_framework.permissions import AllowAny
from rest_framework.response import Response
from django.utils import timezone
import logging
import json
import uuid

from .gemini_client import generate_gemini_response, get_persona
from .models import ChatSession, ChatMessage
from .database_queries import execute_database_query

logger = logging.getLogger(__name__)


@api_view(['POST'])
@permission_classes([AllowAny])
def ai_response(request):
    """Get AI response from Gemini"""
    try:
        message = request.data.get('message', '').strip()
        visitor_name = request.data.get('visitor_name', 'Guest')
        department = request.data.get('department', 'support')
        session_id = request.data.get('session_id') or str(uuid.uuid4())
        user_id = request.data.get('user_id')

        if not message:
            return Response({
                'type': 'system',
                'message': 'Please enter a message to continue.',
                'success': False,
            }, status=400)

        session_defaults = {
            'visitor_name': visitor_name,
            'department': department,
            'source': 'web',
            'status': 'active',
            'last_active_at': timezone.now(),
        }
        if user_id:
            session_defaults['user_id'] = user_id

        session, _created = ChatSession.objects.get_or_create(
            session_id=session_id,
            defaults=session_defaults,
        )

        session.visitor_name = visitor_name
        session.department = department
        session.last_active_at = timezone.now()
        session.save(update_fields=['visitor_name', 'department', 'last_active_at'])

        ChatMessage.objects.create(
            session=session,
            sender_type='user',
            message=message,
        )

        persona = get_persona(department)
        try:
            raw_response = generate_gemini_response(message, department, visitor_name)
        except Exception as gemini_error:
            logger.exception("Gemini response failed")
            raw_response = (
                "Thanks for reaching out. A support agent will respond shortly. "
                "If this is urgent, please include your order or account details."
            )

        response_payload = None
        try:
            response_payload = json.loads(raw_response)
        except json.JSONDecodeError:
            response_payload = None

        reply_text = None
        action_name = None
        action_payload = None

        if isinstance(response_payload, dict):
            reply_text = response_payload.get('reply') or response_payload.get('message')
            action_name = response_payload.get('action')
            action_payload = response_payload.get('action_payload') or {}
        else:
            reply_text = raw_response

        action_result = None
        if action_name in {'create_ticket', 'transfer_human', 'manage_service', 'request_callback', 'query_database'}:
            action_result = _handle_action(action_name, action_payload, session, visitor_name)

        ChatMessage.objects.create(
            session=session,
            sender_type='ai',
            message=reply_text or '',
        )

        return Response({
            'type': 'message',
            'message': reply_text or 'Thanks for reaching out. How can we help further?',
            'sender': persona['name'],
            'persona': department,
            'action': action_name,
            'action_result': action_result,
            'session_id': session.session_id,
            'success': True,
        })
    except Exception as e:
        logger.exception("AI response error")
        return Response({
            'type': 'system',
            'message': 'We are having trouble responding right now. Please try again shortly.',
            'success': False,
            'error': str(e)
        }, status=500)


def _handle_action(action_name, payload, session, visitor_name):
    payload = payload or {}
    try:
        if action_name == 'create_ticket':
            return create_ticket_from_chat_payload(payload, session, visitor_name)
        if action_name == 'transfer_human':
            return transfer_to_human_payload(payload)
        if action_name == 'manage_service':
            return manage_service_payload(payload)
        if action_name == 'request_callback':
            return request_callback_payload(payload, session, visitor_name)
        if action_name == 'query_database':
            return query_database_payload(payload, session, visitor_name)
    except Exception as error:
        logger.exception("Action handling failed")
        return {'success': False, 'error': str(error)}
    return None


@api_view(['POST'])
@permission_classes([AllowAny])
def create_ticket_from_chat(request):
    """Create support ticket from chat"""
    from tickets.models import Ticket
    try:
        result = create_ticket_from_chat_payload(request.data, None, request.data.get('visitor_name', 'Guest'))
        return Response(result)
    except Exception as e:
        return Response({'success': False, 'error': str(e)}, status=500)


@api_view(['POST'])
@permission_classes([AllowAny])
def transfer_to_human(request):
    """Transfer to human agent"""
    return Response(transfer_to_human_payload(request.data))


@api_view(['POST'])
@permission_classes([AllowAny])
def manage_service(request):
    """Manage client services"""
    return Response(manage_service_payload(request.data))


@api_view(['POST'])
@permission_classes([AllowAny])
def request_callback(request):
    """Request a callback from support"""
    return Response(request_callback_payload(request.data, None, request.data.get('visitor_name', 'Guest')))


def create_ticket_from_chat_payload(payload, session, visitor_name):
    from tickets.models import Ticket
    description = payload.get('description') or payload.get('message') or ''
    subject = payload.get('subject') or 'Chat Support'
    user_id = payload.get('user_id') or (session.user_id if session else None)
    ticket = Ticket.objects.create(
        user_id=user_id,
        subject=subject,
        description=description,
        status='open',
        priority=payload.get('priority', 'medium')
    )
    return {
        'success': True,
        'ticket_id': ticket.id,
        'message': f'Ticket {ticket.id} created for {visitor_name}.'
    }


def transfer_to_human_payload(payload):
    return {
        'success': True,
        'agent_id': 'agent_001',
        'agent_name': payload.get('agent_name', 'Support Agent')
    }


def manage_service_payload(payload):
    action = payload.get('action')
    return {
        'success': True,
        'message': f'Action {action} completed'
    }


def request_callback_payload(payload, session, visitor_name):
    phone = payload.get('phone') or ''
    preferred_time = payload.get('preferred_time') or 'anytime'
    return {
        'success': True,
        'message': f'Callback requested by {visitor_name}. We will call {phone} ({preferred_time}).'
    }


def query_database_payload(payload, session, visitor_name):
    """Execute a whitelisted database query"""
    if not session:
        return {
            'success': False,
            'message': 'Database queries require an active session.'
        }
    
    query_name = payload.get('query')
    query_params = payload.get('params') or {}
    
    if not query_name:
        return {
            'success': False,
            'message': 'Query name is required.'
        }
    
    result = execute_database_query(
        query_name,
        session.session_id,
        visitor_name,
        params=query_params
    )
    
    return {
        'success': result.get('status') == 'success',
        'query': query_name,
        'data': result,
        'message': result.get('message', 'Query executed.')
    }

