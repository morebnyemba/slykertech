"""
WebSocket routing configuration for Django Channels
"""

from django.urls import path
from services.consumers import DashboardAnalyticsConsumer
from services.dns_consumers import DNSPanelConsumer
from livechat.consumers import LiveChatConsumer

websocket_urlpatterns = [
    # WebSocket routes for real-time updates
    path('ws/analytics/', DashboardAnalyticsConsumer.as_asgi()),
    path('ws/dns/', DNSPanelConsumer.as_asgi()),
    path('ws/chat/<str:room_name>/', LiveChatConsumer.as_asgi()),
]
