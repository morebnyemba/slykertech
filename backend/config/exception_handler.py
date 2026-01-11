"""
Custom exception handler for DRF
Provides consistent error responses across the API
"""
from rest_framework.views import exception_handler
from rest_framework.response import Response
from rest_framework import status
import logging

logger = logging.getLogger(__name__)


def custom_exception_handler(exc, context):
    """
    Custom exception handler that provides consistent error responses
    """
    # Call REST framework's default exception handler first
    response = exception_handler(exc, context)
    
    if response is not None:
        # Log the error
        logger.error(
            f"API Error: {exc.__class__.__name__} - {str(exc)}",
            extra={
                'view': context.get('view').__class__.__name__,
                'request': context.get('request').path if context.get('request') else None
            }
        )
        
        # Customize the response data
        custom_response_data = {
            'error': True,
            'status_code': response.status_code,
            'message': get_error_message(response.data),
            'details': response.data if isinstance(response.data, dict) else {}
        }
        
        response.data = custom_response_data
    else:
        # Handle unexpected errors
        logger.exception(
            f"Unexpected error: {exc.__class__.__name__} - {str(exc)}",
            extra={
                'view': context.get('view').__class__.__name__ if context.get('view') else None,
                'request': context.get('request').path if context.get('request') else None
            }
        )
        
        response = Response(
            {
                'error': True,
                'status_code': status.HTTP_500_INTERNAL_SERVER_ERROR,
                'message': 'An unexpected error occurred. Please try again later.',
                'details': {}
            },
            status=status.HTTP_500_INTERNAL_SERVER_ERROR
        )
    
    return response


def get_error_message(data):
    """
    Extract a user-friendly error message from error data
    """
    if isinstance(data, dict):
        # Check for common error fields
        if 'detail' in data:
            return str(data['detail'])
        elif 'non_field_errors' in data:
            return str(data['non_field_errors'][0]) if data['non_field_errors'] else 'Validation error'
        elif len(data) > 0:
            # Return the first error message
            first_key = list(data.keys())[0]
            first_value = data[first_key]
            if isinstance(first_value, list) and len(first_value) > 0:
                return f"{first_key}: {first_value[0]}"
            return str(first_value)
    elif isinstance(data, list) and len(data) > 0:
        return str(data[0])
    
    return 'An error occurred'
