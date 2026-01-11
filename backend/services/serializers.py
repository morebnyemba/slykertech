from rest_framework import serializers
from .models import (Service, ServiceSubscription, DNSRecord,
                    ProjectTracker, ProjectMilestone, ProjectTask, ProjectComment)
from clients.serializers import ClientSerializer
from accounts.serializers import UserSerializer


class ServiceSerializer(serializers.ModelSerializer):
    """Serializer for Service model"""
    
    class Meta:
        model = Service
        fields = ['id', 'name', 'category', 'description', 'features', 'base_price', 
                  'is_active', 'created_at', 'updated_at']
        read_only_fields = ['id', 'created_at', 'updated_at']


class DNSRecordSerializer(serializers.ModelSerializer):
    """Serializer for DNSRecord model"""
    
    class Meta:
        model = DNSRecord
        fields = ['id', 'domain', 'record_type', 'name', 'content', 'ttl', 'priority', 
                  'is_active', 'external_id', 'created_at', 'updated_at']
        read_only_fields = ['id', 'external_id', 'created_at', 'updated_at']


class ServiceSubscriptionSerializer(serializers.ModelSerializer):
    """Serializer for ServiceSubscription model"""
    
    client = ClientSerializer(read_only=True)
    service = ServiceSerializer(read_only=True)
    dns_records = DNSRecordSerializer(many=True, read_only=True)
    
    class Meta:
        model = ServiceSubscription
        fields = ['id', 'client', 'service', 'status', 'billing_cycle', 'price', 
                  'start_date', 'end_date', 'auto_renew', 'notes', 'metadata', 
                  'dns_records', 'created_at', 'updated_at']
        read_only_fields = ['id', 'created_at', 'updated_at']


class ServiceSubscriptionCreateSerializer(serializers.ModelSerializer):
    """Serializer for creating ServiceSubscription"""
    
    class Meta:
        model = ServiceSubscription
        fields = ['client', 'service', 'status', 'billing_cycle', 'price', 
                  'start_date', 'end_date', 'auto_renew', 'notes', 'metadata']


class ProjectTaskSerializer(serializers.ModelSerializer):
    """Serializer for ProjectTask model"""
    
    assigned_to = UserSerializer(read_only=True)
    
    class Meta:
        model = ProjectTask
        fields = ['id', 'milestone', 'project', 'title', 'description', 'status', 
                  'assigned_to', 'estimated_hours', 'actual_hours', 'due_date', 
                  'completed_date', 'order', 'created_at', 'updated_at']
        read_only_fields = ['id', 'created_at', 'updated_at']


class ProjectMilestoneSerializer(serializers.ModelSerializer):
    """Serializer for ProjectMilestone model"""
    
    tasks = ProjectTaskSerializer(many=True, read_only=True)
    
    class Meta:
        model = ProjectMilestone
        fields = ['id', 'project', 'title', 'description', 'status', 'due_date', 
                  'completed_date', 'order', 'tasks', 'created_at', 'updated_at']
        read_only_fields = ['id', 'created_at', 'updated_at']


class ProjectCommentSerializer(serializers.ModelSerializer):
    """Serializer for ProjectComment model"""
    
    user = UserSerializer(read_only=True)
    
    class Meta:
        model = ProjectComment
        fields = ['id', 'project', 'user', 'comment', 'is_internal', 'attachments', 
                  'created_at', 'updated_at']
        read_only_fields = ['id', 'user', 'created_at', 'updated_at']


class ProjectTrackerSerializer(serializers.ModelSerializer):
    """Serializer for ProjectTracker model"""
    
    subscription = ServiceSubscriptionSerializer(read_only=True)
    assigned_to = UserSerializer(read_only=True)
    milestones = ProjectMilestoneSerializer(many=True, read_only=True)
    tasks = ProjectTaskSerializer(many=True, read_only=True)
    comments = ProjectCommentSerializer(many=True, read_only=True)
    
    class Meta:
        model = ProjectTracker
        fields = ['id', 'subscription', 'title', 'description', 'status', 'priority', 
                  'progress_percentage', 'estimated_hours', 'actual_hours', 'start_date', 
                  'estimated_completion_date', 'actual_completion_date', 'assigned_to', 
                  'metadata', 'milestones', 'tasks', 'comments', 'created_at', 'updated_at']
        read_only_fields = ['id', 'created_at', 'updated_at']


class ProjectTrackerCreateSerializer(serializers.ModelSerializer):
    """Serializer for creating ProjectTracker"""
    
    class Meta:
        model = ProjectTracker
        fields = ['subscription', 'title', 'description', 'status', 'priority', 
                  'estimated_hours', 'start_date', 'estimated_completion_date', 
                  'assigned_to', 'metadata']

