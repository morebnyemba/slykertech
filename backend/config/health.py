"""
Health check endpoints for monitoring and deployment verification
"""
from django.http import JsonResponse
from django.conf import settings
from django.db import connection
from django.core.cache import cache
import sys


def health_check(request):
    """
    Basic health check endpoint
    Returns 200 if the application is running
    """
    return JsonResponse({
        'status': 'healthy',
        'service': 'slykertech-api',
        'debug': settings.DEBUG,
        'python_version': f"{sys.version_info.major}.{sys.version_info.minor}.{sys.version_info.micro}"
    })


def readiness_check(request):
    """
    Readiness check - verifies all critical services are available
    """
    checks = {
        'database': False,
        'migrations': False,
    }
    
    # Database check
    try:
        connection.ensure_connection()
        checks['database'] = True
    except Exception as e:
        return JsonResponse({
            'status': 'not_ready',
            'checks': checks,
            'error': f'Database error: {str(e)}'
        }, status=503)
    
    # Check if migrations are up to date
    try:
        from django.db.migrations.executor import MigrationExecutor
        executor = MigrationExecutor(connection)
        plan = executor.migration_plan(executor.loader.graph.leaf_nodes())
        checks['migrations'] = len(plan) == 0
    except Exception as e:
        return JsonResponse({
            'status': 'not_ready',
            'checks': checks,
            'error': f'Migration check error: {str(e)}'
        }, status=503)
    
    if all(checks.values()):
        return JsonResponse({
            'status': 'ready',
            'checks': checks
        })
    else:
        return JsonResponse({
            'status': 'not_ready',
            'checks': checks
        }, status=503)
