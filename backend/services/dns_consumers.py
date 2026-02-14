"""
WebSocket consumer for DNS panel real-time updates
"""

import json
import logging
from channels.generic.websocket import AsyncWebsocketConsumer
from channels.db import database_sync_to_async

logger = logging.getLogger(__name__)

VALID_RECORD_TYPES = {'A', 'AAAA', 'CNAME', 'MX', 'TXT', 'NS', 'SRV', 'CAA'}
MIN_TTL = 60
MAX_TTL = 86400


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
        try:
            data = json.loads(text_data)
        except json.JSONDecodeError:
            await self.send_error('Invalid JSON payload')
            return

        message_type = data.get('type')
        
        if message_type == 'request_records':
            await self.send_dns_records()
        elif message_type == 'create_record':
            record_data = data.get('record')
            if not isinstance(record_data, dict):
                await self.send_error('Record data must be an object')
                return
            result = await self.create_dns_record(record_data)
            await self.send(text_data=json.dumps({
                'type': 'record_created',
                'data': result
            }))
            if result.get('success'):
                await self.broadcast_dns_update('created', result.get('record'))
        elif message_type == 'update_record':
            record_id = data.get('id')
            record_data = data.get('record')
            if record_id is None:
                await self.send_error('Record id is required')
                return
            if not isinstance(record_data, dict):
                await self.send_error('Record data must be an object')
                return
            result = await self.update_dns_record(record_id, record_data)
            await self.send(text_data=json.dumps({
                'type': 'record_updated',
                'data': result
            }))
            if result.get('success'):
                await self.broadcast_dns_update('updated', result.get('record'))
        elif message_type == 'delete_record':
            record_id = data.get('id')
            if record_id is None:
                await self.send_error('Record id is required')
                return
            result = await self.delete_dns_record(record_id)
            await self.send(text_data=json.dumps({
                'type': 'record_deleted',
                'data': result
            }))
            if result.get('success'):
                await self.broadcast_dns_update('deleted', {'id': record_id})
        else:
            await self.send_error(f'Unknown message type: {message_type}')

    async def send_error(self, message):
        """Send an error message to the client"""
        await self.send(text_data=json.dumps({
            'type': 'error',
            'data': {'message': message}
        }))

    async def broadcast_dns_update(self, action, record_data):
        """Broadcast DNS change to all connections in the user's group"""
        await self.channel_layer.group_send(
            self.room_group_name,
            {
                'type': 'dns_update',
                'data': {'action': action, 'record': record_data}
            }
        )
    
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
        
        # Get user's DNS records via subscription -> client -> user
        records = DNSRecord.objects.filter(
            subscription__client__user=self.user
        ).values(
            'id', 'record_type', 'name', 'content', 
            'ttl', 'priority', 'domain',
            'is_active', 'subscription__service__name'
        )
        
        return list(records)
    
    @database_sync_to_async
    def create_dns_record(self, record_data):
        """Create a new DNS record"""
        from services.models import DNSRecord, ServiceSubscription

        # Validate required fields
        required = ['subscription_id', 'record_type', 'name', 'content', 'domain']
        missing = [f for f in required if not record_data.get(f)]
        if missing:
            return {'success': False, 'error': f'Missing required fields: {", ".join(missing)}'}

        record_type = record_data.get('record_type', '').upper()
        if record_type not in VALID_RECORD_TYPES:
            return {'success': False, 'error': f'Invalid record type. Must be one of: {", ".join(sorted(VALID_RECORD_TYPES))}'}

        if record_type == 'MX' and not record_data.get('priority'):
            return {'success': False, 'error': 'Priority is required for MX records'}

        ttl = record_data.get('ttl', 3600)
        try:
            ttl = int(ttl)
        except (TypeError, ValueError):
            return {'success': False, 'error': 'TTL must be an integer'}
        if not (MIN_TTL <= ttl <= MAX_TTL):
            return {'success': False, 'error': f'TTL must be between {MIN_TTL} and {MAX_TTL}'}

        try:
            subscription = ServiceSubscription.objects.get(
                id=record_data.get('subscription_id'),
                client__user=self.user
            )
            
            record = DNSRecord.objects.create(
                subscription=subscription,
                domain=record_data.get('domain'),
                record_type=record_type,
                name=record_data.get('name'),
                content=record_data.get('content'),
                ttl=ttl,
                priority=record_data.get('priority')
            )
            
            return {
                'success': True,
                'record': {
                    'id': record.id,
                    'record_type': record.record_type,
                    'name': record.name,
                    'content': record.content,
                    'ttl': record.ttl,
                    'priority': record.priority,
                    'domain': record.domain,
                    'is_active': record.is_active,
                }
            }
        except ServiceSubscription.DoesNotExist:
            return {'success': False, 'error': 'Subscription not found or access denied'}
        except Exception as e:
            logger.exception("Error creating DNS record")
            return {'success': False, 'error': str(e)}
    
    @database_sync_to_async
    def update_dns_record(self, record_id, record_data):
        """Update an existing DNS record"""
        from services.models import DNSRecord
        
        try:
            record = DNSRecord.objects.get(
                id=record_id,
                subscription__client__user=self.user
            )
            
            if 'record_type' in record_data:
                new_type = record_data['record_type'].upper()
                if new_type not in VALID_RECORD_TYPES:
                    return {'success': False, 'error': f'Invalid record type. Must be one of: {", ".join(sorted(VALID_RECORD_TYPES))}'}
                record.record_type = new_type
            if 'name' in record_data:
                record.name = record_data['name']
            if 'content' in record_data:
                record.content = record_data['content']
            if 'ttl' in record_data:
                try:
                    ttl = int(record_data['ttl'])
                except (TypeError, ValueError):
                    return {'success': False, 'error': 'TTL must be an integer'}
                if not (MIN_TTL <= ttl <= MAX_TTL):
                    return {'success': False, 'error': f'TTL must be between {MIN_TTL} and {MAX_TTL}'}
                record.ttl = ttl
            if 'priority' in record_data:
                record.priority = record_data['priority']
            if 'domain' in record_data:
                record.domain = record_data['domain']

            # Validate MX priority
            if record.record_type == 'MX' and record.priority is None:
                return {'success': False, 'error': 'Priority is required for MX records'}
            
            record.save()
            
            return {
                'success': True,
                'record': {
                    'id': record.id,
                    'record_type': record.record_type,
                    'name': record.name,
                    'content': record.content,
                    'ttl': record.ttl,
                    'priority': record.priority,
                    'domain': record.domain,
                    'is_active': record.is_active,
                }
            }
        except DNSRecord.DoesNotExist:
            return {'success': False, 'error': 'DNS record not found or access denied'}
        except Exception as e:
            logger.exception("Error updating DNS record")
            return {'success': False, 'error': str(e)}
    
    @database_sync_to_async
    def delete_dns_record(self, record_id):
        """Delete a DNS record"""
        from services.models import DNSRecord
        
        try:
            record = DNSRecord.objects.get(
                id=record_id,
                subscription__client__user=self.user
            )
            record.delete()
            
            return {
                'success': True,
                'id': record_id
            }
        except DNSRecord.DoesNotExist:
            return {'success': False, 'error': 'DNS record not found or access denied'}
        except Exception as e:
            logger.exception("Error deleting DNS record")
            return {'success': False, 'error': str(e)}
