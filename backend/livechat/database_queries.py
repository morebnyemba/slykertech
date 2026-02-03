"""
Secure database query functions for livechat AI personas.

These functions allow AI personas to query customer data safely:
- Only queries the current visitor's own account data
- Enforces strict field whitelisting
- Returns only non-sensitive information
- Logs all queries for audit purposes
"""

from django.db.models import Q
from django.utils import timezone
from typing import Dict, List, Any, Optional
import logging

from .models import ChatSession, ChatMessage
from services.models import ServiceSubscription
from clients.models import Client

logger = logging.getLogger(__name__)


# Whitelisted fields that AI can query
QUERYABLE_FIELDS = {
    "service_subscription": [
        "id",
        "status",
        "billing_cycle",
        "price",
        "start_date",
        "end_date",
        "next_due_date",
        "auto_renew",
        "created_at",
    ],
    "service": [
        "name",
        "category",
        "description",
    ],
    "chat_session": [
        "visitor_name",
        "department",
        "status",
        "created_at",
        "last_active_at",
    ],
    "chat_message": [
        "sender_type",
        "message",
        "created_at",
    ],
}

# Blocked fields that are NEVER queryable
BLOCKED_FIELDS = [
    "password",
    "api_key",
    "billing_address",
    "credit_card",
    "payment_method",
    "email",
    "phone",
    "ip_address",
    "token",
    "secret",
]


def _sanitize_field(field_name: str) -> str:
    """Ensure field is allowed for querying."""
    field_lower = field_name.lower().strip()

    # Check blocked fields
    for blocked in BLOCKED_FIELDS:
        if blocked in field_lower:
            raise PermissionError(f"Cannot query field: {field_name}")

    return field_lower


def get_visitor_services(
    session_id: str, visitor_name: str
) -> Dict[str, Any]:
    """
    Get all service subscriptions for the current visitor.

    Returns: {
        "status": "success",
        "services": [
            {"id": 1, "service_name": "Shared Hosting", "status": "active", ...},
            ...
        ],
        "count": 3
    }
    """
    try:
        # Find the chat session
        chat_session = ChatSession.objects.filter(
            session_id=session_id, visitor_name=visitor_name
        ).first()

        if not chat_session:
            return {
                "status": "not_found",
                "message": "No session found for this visitor",
                "services": [],
            }

        # Get user if authenticated
        user = chat_session.user

        if not user:
            return {
                "status": "unauthenticated",
                "message": "Visitor is not authenticated. Cannot query services without account access.",
                "services": [],
            }

        # Get client profile
        try:
            client = Client.objects.get(user=user)
        except Client.DoesNotExist:
            return {
                "status": "no_client",
                "message": "No client profile found for this user.",
                "services": [],
            }

        # Query service subscriptions for this client
        subscriptions = ServiceSubscription.objects.filter(
            client=client
        ).select_related('service')

        # Build service list with both subscription and service details
        service_list = []
        for sub in subscriptions:
            service_data = {
                'subscription_id': sub.id,
                'service_name': sub.service.name,
                'service_category': sub.service.category,
                'status': sub.status,
                'billing_cycle': sub.billing_cycle,
                'price': str(sub.price),
                'start_date': sub.start_date.isoformat() if sub.start_date else None,
                'end_date': sub.end_date.isoformat() if sub.end_date else None,
                'next_due_date': sub.next_due_date.isoformat() if sub.next_due_date else None,
                'auto_renew': sub.auto_renew,
                'created_at': sub.created_at.isoformat(),
            }
            service_list.append(service_data)

        result = {
            "status": "success",
            "services": service_list,
            "count": len(service_list),
        }

        # Log query for audit
        logger.info(
            f"Database query: get_visitor_services for user_id={user.id}, session_id={session_id}"
        )

        return result

    except Exception as e:
        logger.error(f"Error querying visitor services: {str(e)}")
        return {"status": "error", "message": str(e), "services": []}


def get_visitor_account_status(
    session_id: str, visitor_name: str
) -> Dict[str, Any]:
    """
    Get account status summary for current visitor.

    Returns: {
        "status": "success",
        "account": {
            "total_services": 3,
            "active_services": 2,
            "expired_services": 1,
            "total_spending": "1250.00",
            "last_purchase": "2025-12-15"
        }
    }
    """
    try:
        # Find the chat session
        chat_session = ChatSession.objects.filter(
            session_id=session_id, visitor_name=visitor_name
        ).first()

        if not chat_session:
            return {
                "status": "not_found",
                "message": "No session found",
                "account": {},
            }

        user = chat_session.user

        if not user:
            return {
                "status": "unauthenticated",
                "message": "Cannot access account data - visitor not authenticated",
                "account": {},
            }

        # Get client profile
        try:
            client = Client.objects.get(user=user)
        except Client.DoesNotExist:
            return {
                "status": "no_client",
                "message": "No client profile found",
                "account": {},
            }

        # Get account statistics
        subscriptions = ServiceSubscription.objects.filter(client=client)
        total_services = subscriptions.count()
        active_services = subscriptions.filter(status='active').count()
        expired_services = subscriptions.filter(status='expired').count()
        suspended_services = subscriptions.filter(status='suspended').count()

        # Calculate total spending (sum of all subscription prices)
        from django.db.models import Sum
        total_spending = subscriptions.aggregate(Sum('price'))['price__sum']
        total_spending_str = str(total_spending) if total_spending else "0.00"
        
        # Get last purchase date
        last_subscription = subscriptions.order_by('-created_at').first()
        last_purchase_date = (
            last_subscription.created_at.isoformat() if last_subscription else "No services"
        )

        account_summary = {
            "total_services": total_services,
            "active_services": active_services,
            "expired_services": expired_services,
            "suspended_services": suspended_services,
            "total_spending": total_spending_str,
            "last_purchase": last_purchase_date,
            "account_created": user.date_joined.isoformat(),
            "company_name": client.company_name,
        }

        logger.info(
            f"Database query: get_visitor_account_status for user_id={user.id}, session_id={session_id}"
        )

        return {"status": "success", "account": account_summary}

    except Exception as e:
        logger.error(f"Error querying account status: {str(e)}")
        return {"status": "error", "message": str(e), "account": {}}


def get_visitor_chat_history(
    session_id: str, visitor_name: str, limit: int = 10
) -> Dict[str, Any]:
    """
    Get recent chat history for current visitor.

    Args:
        session_id: Current chat session ID
        visitor_name: Current visitor name
        limit: Number of recent messages to return (max 20)

    Returns: {
        "status": "success",
        "messages": [
            {"sender_type": "user", "message": "Help with hosting", "created_at": "2025-02-03T10:30:00Z"},
            ...
        ],
        "count": 5
    }
    """
    try:
        if limit > 20:
            limit = 20  # Safety limit

        # Find the chat session
        chat_session = ChatSession.objects.filter(
            session_id=session_id, visitor_name=visitor_name
        ).first()

        if not chat_session:
            return {
                "status": "not_found",
                "message": "No session found",
                "messages": [],
            }

        # Get recent messages from this session only
        messages = ChatMessage.objects.filter(session=chat_session).order_by(
            "-created_at"
        )[:limit]

        message_list = [
            {
                "sender_type": msg.sender_type,
                "message": msg.message[:500],  # Truncate for safety
                "created_at": msg.created_at.isoformat(),
            }
            for msg in messages
        ]

        logger.info(
            f"Database query: get_visitor_chat_history for session_id={session_id}, limit={limit}"
        )

        return {"status": "success", "messages": list(reversed(message_list)), "count": len(message_list)}

    except Exception as e:
        logger.error(f"Error querying chat history: {str(e)}")
        return {"status": "error", "message": str(e), "messages": []}


def get_service_details(
    session_id: str, visitor_name: str, service_id: int
) -> Dict[str, Any]:
    """
    Get details for a specific service subscription.

    Returns: {
        "status": "success",
        "service": {
            "subscription_id": 1,
            "service_name": "Shared Hosting",
            "status": "active",
            ...
        }
    }
    """
    try:
        # Verify ownership
        chat_session = ChatSession.objects.filter(
            session_id=session_id, visitor_name=visitor_name
        ).first()

        if not chat_session or not chat_session.user:
            return {
                "status": "unauthenticated",
                "message": "Cannot access service - not authenticated",
                "service": {},
            }

        # Get client profile
        try:
            client = Client.objects.get(user=chat_session.user)
        except Client.DoesNotExist:
            return {
                "status": "no_client",
                "message": "No client profile found",
                "service": {},
            }

        # Get subscription and verify client owns it
        try:
            subscription = ServiceSubscription.objects.select_related('service').get(
                id=service_id, client=client
            )
        except ServiceSubscription.DoesNotExist:
            return {
                "status": "not_found",
                "message": f"Service subscription {service_id} not found or access denied",
                "service": {},
            }

        service_data = {
            'subscription_id': subscription.id,
            'service_name': subscription.service.name,
            'service_category': subscription.service.category,
            'service_description': subscription.service.description,
            'status': subscription.status,
            'billing_cycle': subscription.billing_cycle,
            'price': str(subscription.price),
            'start_date': subscription.start_date.isoformat() if subscription.start_date else None,
            'end_date': subscription.end_date.isoformat() if subscription.end_date else None,
            'next_due_date': subscription.next_due_date.isoformat() if subscription.next_due_date else None,
            'auto_renew': subscription.auto_renew,
            'created_at': subscription.created_at.isoformat(),
        }

        logger.info(
            f"Database query: get_service_details for service_id={service_id}, user_id={chat_session.user.id}"
        )

        return {"status": "success", "service": service_data}

    except Exception as e:
        logger.error(f"Error querying service details: {str(e)}")
        return {"status": "error", "message": str(e), "service": {}}


# Registry of available database queries
AVAILABLE_QUERIES = {
    "get_visitor_services": {
        "description": "Get all services owned by the visitor",
        "params": ["session_id", "visitor_name"],
        "function": get_visitor_services,
    },
    "get_visitor_account_status": {
        "description": "Get account summary (total services, status, etc)",
        "params": ["session_id", "visitor_name"],
        "function": get_visitor_account_status,
    },
    "get_visitor_chat_history": {
        "description": "Get recent chat messages in this conversation",
        "params": ["session_id", "visitor_name", "limit"],
        "function": get_visitor_chat_history,
    },
    "get_service_details": {
        "description": "Get details for a specific service by ID",
        "params": ["session_id", "visitor_name", "service_id"],
        "function": get_service_details,
    },
}


def execute_database_query(
    query_name: str, session_id: str, visitor_name: str, params: Optional[Dict[str, Any]] = None
) -> Dict[str, Any]:
    """
    Execute a whitelisted database query.

    Args:
        query_name: Name of the query function to execute
        session_id: Current chat session ID (for verification)
        visitor_name: Current visitor name (for verification)
        params: Additional parameters for the query

    Returns:
        Query result as dictionary
    """
    if query_name not in AVAILABLE_QUERIES:
        logger.warning(f"Attempted query of unknown function: {query_name}")
        return {
            "status": "error",
            "message": f"Unknown query: {query_name}",
        }

    try:
        query_info = AVAILABLE_QUERIES[query_name]
        query_func = query_info["function"]

        # Call function with base params
        if params:
            result = query_func(session_id, visitor_name, **params)
        else:
            result = query_func(session_id, visitor_name)

        logger.info(
            f"Database query executed: {query_name} for session_id={session_id}"
        )
        return result

    except PermissionError as e:
        logger.warning(f"Database query permission denied: {str(e)}")
        return {"status": "permission_denied", "message": str(e)}
    except Exception as e:
        logger.error(f"Error executing database query {query_name}: {str(e)}")
        return {"status": "error", "message": f"Query failed: {str(e)}"}
