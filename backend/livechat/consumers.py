"""
WebSocket consumers for live chat functionality
"""

import json
from channels.generic.websocket import AsyncWebsocketConsumer
from channels.db import database_sync_to_async


class LiveChatConsumer(AsyncWebsocketConsumer):
    """
    WebSocket consumer for live chat
    Handles real-time messaging between clients and support staff
    """
    
    async def connect(self):
        """Handle WebSocket connection"""
        self.user = self.scope.get('user')
        self.room_name = self.scope['url_route']['kwargs'].get('room_name', 'general')
        self.room_group_name = f'chat_{self.room_name}'
        
        # Check authentication for management department
        if self.room_name == 'management':
            if not self.user or not self.user.is_authenticated:
                await self.close(code=4001)
                return
            
            # Check if user is staff
            if not await self.is_staff_user():
                await self.close(code=4003)
                return
        
        # Join room group
        await self.channel_layer.group_add(
            self.room_group_name,
            self.channel_name
        )
        
        await self.accept()
        
        # Send welcome message
        await self.send(text_data=json.dumps({
            'type': 'connection_established',
            'message': 'Connected to live chat',
            'room': self.room_name
        }))
    
    async def disconnect(self, close_code):
        """Handle WebSocket disconnection"""
        # Leave room group
        if hasattr(self, 'room_group_name'):
            await self.channel_layer.group_discard(
                self.room_group_name,
                self.channel_name
            )
    
    async def receive(self, text_data):
        """Receive message from WebSocket"""
        try:
            data = json.loads(text_data)
            message_type = data.get('type', 'chat_message')
            
            if message_type == 'chat_message':
                message = data.get('message', '')
                sender = data.get('sender', 'Anonymous')
                
                # Broadcast message to room group
                await self.channel_layer.group_send(
                    self.room_group_name,
                    {
                        'type': 'chat_message',
                        'message': message,
                        'sender': sender,
                        'timestamp': data.get('timestamp')
                    }
                )
            
            elif message_type == 'typing':
                # Broadcast typing indicator
                await self.channel_layer.group_send(
                    self.room_group_name,
                    {
                        'type': 'typing_indicator',
                        'sender': data.get('sender', 'Anonymous'),
                        'is_typing': data.get('is_typing', False)
                    }
                )
        
        except json.JSONDecodeError:
            await self.send(text_data=json.dumps({
                'type': 'error',
                'message': 'Invalid JSON format'
            }))
    
    async def chat_message(self, event):
        """Send chat message to WebSocket"""
        await self.send(text_data=json.dumps({
            'type': 'chat_message',
            'message': event['message'],
            'sender': event['sender'],
            'timestamp': event.get('timestamp')
        }))
    
    async def typing_indicator(self, event):
        """Send typing indicator to WebSocket"""
        await self.send(text_data=json.dumps({
            'type': 'typing',
            'sender': event['sender'],
            'is_typing': event['is_typing']
        }))
    
    @database_sync_to_async
    def is_staff_user(self):
        """Check if user is staff member"""
        if not self.user or not self.user.is_authenticated:
            return False
        
        # Check if user is staff or admin
        return self.user.is_staff or self.user.is_superuser or getattr(self.user, 'user_type', None) == 'admin'
