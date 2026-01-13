"""
WebSocket consumers for real-time analytics and dashboard updates
"""

import json
from channels.generic.websocket import AsyncWebsocketConsumer
from channels.db import database_sync_to_async


class DashboardAnalyticsConsumer(AsyncWebsocketConsumer):
    """
    WebSocket consumer for real-time dashboard analytics
    """
    
    async def connect(self):
        """Accept WebSocket connection"""
        self.user = self.scope["user"]
        
        if not self.user.is_authenticated:
            await self.close()
            return
            
        self.room_group_name = f"analytics_{self.user.id}"
        
        # Join room group
        await self.channel_layer.group_add(
            self.room_group_name,
            self.channel_name
        )
        
        await self.accept()
        
        # Send initial data
        await self.send_analytics_update()
    
    async def disconnect(self, close_code):
        """Leave room group"""
        if hasattr(self, 'room_group_name'):
            await self.channel_layer.group_discard(
                self.room_group_name,
                self.channel_name
            )
    
    async def receive(self, text_data):
        """Receive message from WebSocket"""
        data = json.loads(text_data)
        message_type = data.get('type')
        
        if message_type == 'request_update':
            await self.send_analytics_update()
    
    async def send_analytics_update(self):
        """Send analytics data to WebSocket"""
        analytics_data = await self.get_analytics_data()
        
        await self.send(text_data=json.dumps({
            'type': 'analytics_update',
            'data': analytics_data
        }))
    
    async def analytics_message(self, event):
        """Receive message from room group"""
        await self.send(text_data=json.dumps({
            'type': 'analytics_update',
            'data': event['data']
        }))
    
    @database_sync_to_async
    def get_analytics_data(self):
        """Get analytics data from database"""
        from services.models import Subscription
        from billing.models import Invoice
        
        # Get user-specific analytics
        subscriptions = Subscription.objects.filter(
            client__user=self.user
        ).count()
        
        invoices = Invoice.objects.filter(
            client__user=self.user
        )
        
        total_invoices = invoices.count()
        paid_invoices = invoices.filter(status='paid').count()
        pending_invoices = invoices.filter(status='pending').count()
        
        return {
            'subscriptions': subscriptions,
            'invoices': {
                'total': total_invoices,
                'paid': paid_invoices,
                'pending': pending_invoices,
            }
        }
