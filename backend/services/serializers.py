from rest_framework import serializers
from .models import (Service, ServiceSubscription, DNSRecord,
                    ProjectTracker, ProjectMilestone, ProjectTask, ProjectComment,
                    HostingProduct, DomainProduct, ServiceAddon, DomainRegistration)
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


class HostingProductSerializer(serializers.ModelSerializer):
    """Serializer for HostingProduct model"""
    
    class Meta:
        model = HostingProduct
        fields = [
            'id', 'name', 'slug', 'description', 'hosting_type',
            'disk_space', 'bandwidth', 'email_accounts', 'databases', 
            'ftp_accounts', 'subdomains', 'addon_domains', 'parked_domains',
            'ssl_certificate', 'dedicated_ip', 'cpanel_access', 'ssh_access', 
            'cron_jobs', 'backups_included',
            'monthly_price', 'quarterly_price', 'semi_annual_price', 
            'annual_price', 'biennial_price', 'triennial_price',
            'setup_fee_monthly', 'setup_fee_quarterly', 'setup_fee_annual',
            'is_featured', 'sort_order', 'is_active', 
            'created_at', 'updated_at'
        ]
        read_only_fields = ['id', 'created_at', 'updated_at']


class DomainProductSerializer(serializers.ModelSerializer):
    """Serializer for DomainProduct model"""
    
    class Meta:
        model = DomainProduct
        fields = [
            'id', 'tld', 'description',
            'registration_price_1yr', 'registration_price_2yr', 
            'registration_price_3yr', 'registration_price_5yr', 'registration_price_10yr',
            'renewal_price', 'transfer_price', 'redemption_price',
            'whois_privacy_price', 'auto_renew_default', 'epp_code_required',
            'grace_period_days', 'redemption_period_days',
            'min_registration_years', 'max_registration_years',
            'is_active', 'is_featured', 'sort_order',
            'created_at', 'updated_at'
        ]
        read_only_fields = ['id', 'created_at', 'updated_at']


class ServiceAddonSerializer(serializers.ModelSerializer):
    """Serializer for ServiceAddon model"""
    
    compatible_products = HostingProductSerializer(many=True, read_only=True)
    
    class Meta:
        model = ServiceAddon
        fields = [
            'id', 'name', 'description', 'addon_type', 'billing_type',
            'monthly_price', 'quarterly_price', 'annual_price', 'one_time_price',
            'quantity', 'is_active', 'requires_hosting', 'compatible_products',
            'created_at', 'updated_at'
        ]
        read_only_fields = ['id', 'created_at', 'updated_at']


class DomainRegistrationSerializer(serializers.ModelSerializer):
    """Serializer for DomainRegistration model"""
    
    client = ClientSerializer(read_only=True)
    domain_product = DomainProductSerializer(read_only=True)
    
    class Meta:
        model = DomainRegistration
        fields = [
            'id', 'client', 'domain_name', 'domain_product',
            'registration_date', 'expiry_date', 'registration_years',
            'status', 'auto_renew', 'whois_privacy', 'whois_privacy_expiry',
            'epp_code', 'is_transfer',
            'nameserver1', 'nameserver2', 'nameserver3', 'nameserver4', 'nameserver5',
            'notes', 'created_at', 'updated_at'
        ]
        read_only_fields = ['id', 'created_at', 'updated_at']


