"""
WebSocket consumer for DNS panel real-time updates
"""

import json
from channels.generic.websocket import AsyncWebsocketConsumer
from channels.db import database_sync_to_async


class DNSPanelConsumer(AsyncWebsocketConsumer):
    """
    WebSocket consumer for real-time DNS records updates
    """
    
    async def connect(self):
        """Accept WebSocket connection"""
        self.user = self.scope["user"]
        
        if not self.user.is_authenticated:
            await self.close()
            return
            
        self.room_group_name = f"dns_{self.user.id}"
        
        # Join room group
        await self.channel_layer.group_add(
            self.room_group_name,
            self.channel_name
        )
        
        await self.accept()
        
        # Send initial DNS records
        await self.send_dns_records()
    
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
        
        if message_type == 'request_records':
            await self.send_dns_records()
        elif message_type == 'create_record':
            await self.create_dns_record(data.get('record'))
        elif message_type == 'update_record':
            await self.update_dns_record(data.get('id'), data.get('record'))
        elif message_type == 'delete_record':
            await self.delete_dns_record(data.get('id'))
    
    async def send_dns_records(self):
        """Send DNS records to WebSocket"""
        records = await self.get_dns_records()
        
        await self.send(text_data=json.dumps({
            'type': 'dns_records',
            'data': records
        }))
    
    async def dns_update(self, event):
        """Receive DNS update from room group"""
        await self.send(text_data=json.dumps({
            'type': 'dns_update',
            'data': event['data']
        }))
    
    @database_sync_to_async
    def get_dns_records(self):
        """Get DNS records from database"""
        from services.models import DNSRecord
        
        # Get user's DNS records
        records = DNSRecord.objects.filter(
            service__client__user=self.user
        ).values(
            'id', 'type', 'name', 'content', 
            'ttl', 'priority', 'service__name'
        )
        
        return list(records)
    
    @database_sync_to_async
    def create_dns_record(self, record_data):
        """Create a new DNS record"""
        from services.models import DNSRecord, Service
        
        try:
            service = Service.objects.get(
                id=record_data.get('service_id'),
                client__user=self.user
            )
            
            record = DNSRecord.objects.create(
                service=service,
                type=record_data.get('type'),
                name=record_data.get('name'),
                content=record_data.get('content'),
                ttl=record_data.get('ttl', 3600),
                priority=record_data.get('priority', 0)
            )
            
            return {
                'success': True,
                'record': {
                    'id': record.id,
                    'type': record.type,
                    'name': record.name,
                    'content': record.content,
                    'ttl': record.ttl,
                    'priority': record.priority
                }
            }
        except Exception as e:
            return {
                'success': False,
                'error': str(e)
            }
    
    @database_sync_to_async
    def update_dns_record(self, record_id, record_data):
        """Update an existing DNS record"""
        from services.models import DNSRecord
        
        try:
            record = DNSRecord.objects.get(
                id=record_id,
                service__client__user=self.user
            )
            
            if 'type' in record_data:
                record.type = record_data['type']
            if 'name' in record_data:
                record.name = record_data['name']
            if 'content' in record_data:
                record.content = record_data['content']
            if 'ttl' in record_data:
                record.ttl = record_data['ttl']
            if 'priority' in record_data:
                record.priority = record_data['priority']
            
            record.save()
            
            return {
                'success': True,
                'record': {
                    'id': record.id,
                    'type': record.type,
                    'name': record.name,
                    'content': record.content,
                    'ttl': record.ttl,
                    'priority': record.priority
                }
            }
        except Exception as e:
            return {
                'success': False,
                'error': str(e)
            }
    
    @database_sync_to_async
    def delete_dns_record(self, record_id):
        """Delete a DNS record"""
        from services.models import DNSRecord
        
        try:
            record = DNSRecord.objects.get(
                id=record_id,
                service__client__user=self.user
            )
            record.delete()
            
            return {
                'success': True,
                'id': record_id
            }
        except Exception as e:
            return {
                'success': False,
                'error': str(e)
            }
