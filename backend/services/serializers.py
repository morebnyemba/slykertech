from rest_framework import serializers
from .models import (Service, ServiceSubscription, DNSRecord,
                    ProjectTracker, ProjectMilestone, ProjectTask, ProjectComment,
                    HostingProduct, DomainProduct, ServiceAddon, DomainRegistration,
                    DomainTransferRequest, ProvisioningFailure)
from clients.serializers import ClientSerializer
from accounts.serializers import UserSerializer


class ServiceSerializer(serializers.ModelSerializer):
    """Serializer for Service model"""
    
    recommended_addon_details = serializers.SerializerMethodField(read_only=True)
    
    class Meta:
        model = Service
        fields = ['id', 'name', 'category', 'description', 'features', 'base_price', 
                  'payment_type', 'pricing_options', 'service_metadata', 
                  'requires_provisioning', 'provisioning_type',
                  'recommended_addons', 'recommended_addon_details',
                  'is_active', 'created_at', 'updated_at']
        read_only_fields = ['id', 'recommended_addon_details', 'created_at', 'updated_at']
    
    def get_recommended_addon_details(self, obj):
        """Get a list of recommended addon services for this service"""
        addons = obj.recommended_addons.filter(is_active=True)
        return [{'id': addon.id, 'name': addon.name, 'category': addon.category, 
                 'description': addon.description, 'base_price': addon.base_price} for addon in addons]


class DNSRecordSerializer(serializers.ModelSerializer):
    """Serializer for DNSRecord model"""
    
    class Meta:
        model = DNSRecord
        fields = ['id', 'subscription', 'domain', 'record_type', 'name', 'content', 'ttl', 'priority', 
                  'is_active', 'external_id', 'created_at', 'updated_at']
        read_only_fields = ['id', 'external_id', 'created_at', 'updated_at']

    def validate_ttl(self, value):
        if value < 60 or value > 86400:
            raise serializers.ValidationError("TTL must be between 60 and 86400 seconds.")
        return value

    def validate(self, data):
        record_type = data.get('record_type', getattr(self.instance, 'record_type', None))
        priority = data.get('priority', getattr(self.instance, 'priority', None))
        if record_type == 'MX' and priority is None:
            raise serializers.ValidationError({'priority': 'Priority is required for MX records.'})
        return data


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


class DomainTransferRequestSerializer(serializers.ModelSerializer):
    """Serializer for DomainTransferRequest model"""
    
    client = ClientSerializer(read_only=True)
    
    class Meta:
        model = DomainTransferRequest
        fields = [
            'id', 'client', 'domain_name', 'contact_email', 'contact_name', 
            'contact_phone', 'epp_code', 'current_registrar', 'admin_email',
            'owns_domain', 'status', 'status_message', 'update_nameservers',
            'nameserver1', 'nameserver2', 'whois_privacy', 'auto_renew',
            'created_at', 'updated_at', 'completed_at'
        ]
        read_only_fields = ['id', 'status', 'status_message', 'created_at', 
                          'updated_at', 'completed_at']


class DomainTransferRequestCreateSerializer(serializers.ModelSerializer):
    """Serializer for creating DomainTransferRequest"""
    
    class Meta:
        model = DomainTransferRequest
        fields = [
            'domain_name', 'contact_email', 'contact_name', 'contact_phone',
            'epp_code', 'current_registrar', 'admin_email', 'owns_domain',
            'update_nameservers', 'nameserver1', 'nameserver2', 
            'whois_privacy', 'auto_renew'
        ]
    
    def validate_domain_name(self, value):
        """Validate domain name format"""
        if not value or '.' not in value:
            raise serializers.ValidationError("Please enter a valid domain name (e.g., example.com)")
        return value.lower().strip()
    
    def validate_owns_domain(self, value):
        """Ensure user confirms domain ownership"""
        if not value:
            raise serializers.ValidationError("You must confirm that you own this domain")
        return value
    
    def validate(self, data):
        """Cross-field validation"""
        # EPP code is recommended but not required initially
        # It can be provided later
        return data


class ProvisioningFailureSerializer(serializers.ModelSerializer):
    """Serializer for ProvisioningFailure model"""
    
    subscription = ServiceSubscriptionSerializer(read_only=True)
    resolved_by = UserSerializer(read_only=True)
    
    class Meta:
        model = ProvisioningFailure
        fields = [
            'id', 'subscription', 'error_message', 'error_details',
            'status', 'provisioning_data', 'admin_notes', 
            'resolved_by', 'resolved_at', 'admin_notified', 
            'notification_sent_at', 'created_at', 'updated_at'
        ]
        read_only_fields = ['id', 'subscription', 'error_message', 'error_details',
                          'provisioning_data', 'resolved_by', 'resolved_at',
                          'admin_notified', 'notification_sent_at', 
                          'created_at', 'updated_at']


class ProvisioningFailureUpdateSerializer(serializers.ModelSerializer):
    """Serializer for updating ProvisioningFailure (admin use)"""
    
    class Meta:
        model = ProvisioningFailure
        fields = ['status', 'admin_notes']
    
    def update(self, instance, validated_data):
        # If marking as resolved, set resolved_by and resolved_at
        if validated_data.get('status') == 'resolved':
            from django.utils import timezone
            instance.resolved_by = self.context['request'].user
            instance.resolved_at = timezone.now()
        
        return super().update(instance, validated_data)


class ManualProvisioningSerializer(serializers.Serializer):
    """Serializer for manual provisioning completion"""
    
    notes = serializers.CharField(required=False, allow_blank=True)
    provisioned_username = serializers.CharField(required=False, allow_blank=True)
    provisioned_domain = serializers.CharField(required=False, allow_blank=True)
    provisioned_password = serializers.CharField(required=False, allow_blank=True, 
                                                  write_only=True)
    additional_data = serializers.JSONField(required=False, default=dict)

