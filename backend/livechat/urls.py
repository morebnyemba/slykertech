from django.urls import path, include
from rest_framework.routers import DefaultRouter
from . import bridge_api
from .views import ChatSessionViewSet, ChatMessageViewSet

router = DefaultRouter()
router.register(r'sessions', ChatSessionViewSet, basename='chat-session')
router.register(r'messages', ChatMessageViewSet, basename='chat-message')

urlpatterns = [
    path('bridge/ai_response/', bridge_api.ai_response, name='ai_response'),
    path('bridge/create_ticket/', bridge_api.create_ticket_from_chat, name='create_ticket'),
    path('bridge/transfer_human/', bridge_api.transfer_to_human, name='transfer_human'),
    path('bridge/manage_service/', bridge_api.manage_service, name='manage_service'),
    path('bridge/request_callback/', bridge_api.request_callback, name='request_callback'),
    path('', include(router.urls)),
]
