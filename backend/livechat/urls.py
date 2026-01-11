from django.urls import path
from . import bridge_api

urlpatterns = [
    path('bridge/ai_response/', bridge_api.ai_response, name='ai_response'),
    path('bridge/create_ticket/', bridge_api.create_ticket_from_chat, name='create_ticket'),
    path('bridge/transfer_human/', bridge_api.transfer_to_human, name='transfer_human'),
    path('bridge/manage_service/', bridge_api.manage_service, name='manage_service'),
]
