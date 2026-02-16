from rest_framework import serializers
from .models import (Service, ServiceSubscription, DNSRecord,
                    ProjectPackage, PackageFreeService,
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


class PackageFreeServiceSerializer(serializers.ModelSerializer):
    """Serializer for PackageFreeService model"""
    
    service_name = serializers.CharField(source='service.name', read_only=True)
    service_category = serializers.CharField(source='service.category', read_only=True)
    
    class Meta:
        model = PackageFreeService
        fields = ['id', 'package', 'service', 'service_name', 'service_category',
                  'duration_value', 'duration_unit', 'description',
                  'created_at', 'updated_at']
        read_only_fields = ['id', 'created_at', 'updated_at']


class ProjectPackageSerializer(serializers.ModelSerializer):
    """Serializer for ProjectPackage model"""
    
    free_services = PackageFreeServiceSerializer(many=True, read_only=True)
    
    class Meta:
        model = ProjectPackage
        fields = ['id', 'name', 'slug', 'description', 'project_type', 'deliverables',
                  'estimated_duration_days', 'max_revisions', 'base_price',
                  'is_featured', 'sort_order', 'is_active', 'free_services',
                  'created_at', 'updated_at']
        read_only_fields = ['id', 'created_at', 'updated_at']


class ProjectTaskSerializer(serializers.ModelSerializer):
    """Serializer for ProjectTask model"""
    
    assigned_to = UserSerializer(read_only=True)
    
    class Meta:
        model = ProjectTask
        fields = ['id', 'milestone', 'project', 'title', 'description', 'status', 'priority',
                  'assigned_to', 'depends_on', 'estimated_hours', 'actual_hours', 'due_date', 
                  'completed_date', 'order', 'created_at', 'updated_at']
        read_only_fields = ['id', 'created_at', 'updated_at']
    
    def validate(self, attrs):
        """Validate depends_on is within the same project and not self-referencing"""
        depends_on = attrs.get('depends_on')
        project = attrs.get('project', getattr(self.instance, 'project', None))
        
        if depends_on is not None:
            # Prevent self-dependency
            if self.instance and depends_on.pk == self.instance.pk:
                raise serializers.ValidationError({
                    'depends_on': 'A task cannot depend on itself.'
                })
            # Prevent cross-project dependency
            if project is not None:
                project_id = project.pk if hasattr(project, 'pk') else project
                if depends_on.project_id != project_id:
                    raise serializers.ValidationError({
                        'depends_on': 'A task can only depend on another task within the same project.'
                    })
        
        return attrs


class ProjectMilestoneSerializer(serializers.ModelSerializer):
    """Serializer for ProjectMilestone model - billing fields are read-only for non-admin users"""
    
    tasks = ProjectTaskSerializer(many=True, read_only=True)
    
    class Meta:
        model = ProjectMilestone
        fields = ['id', 'project', 'title', 'description', 'status', 'deliverables',
                  'is_billable', 'amount', 'payment_status', 'due_date', 
                  'completed_date', 'order', 'tasks', 'created_at', 'updated_at']
        read_only_fields = ['id', 'is_billable', 'amount', 'payment_status', 'created_at', 'updated_at']
    
    def validate(self, attrs):
        """Validate billing field consistency"""
        is_billable = attrs.get('is_billable', getattr(self.instance, 'is_billable', False))
        amount = attrs.get('amount', getattr(self.instance, 'amount', None))
        payment_status = attrs.get('payment_status', getattr(self.instance, 'payment_status', 'not_applicable'))
        
        if is_billable:
            if amount is None:
                raise serializers.ValidationError({
                    'amount': 'Amount is required for billable milestones.'
                })
            if payment_status == 'not_applicable':
                raise serializers.ValidationError({
                    'payment_status': 'Payment status cannot be "Not Applicable" for billable milestones.'
                })
        else:
            if payment_status != 'not_applicable':
                raise serializers.ValidationError({
                    'payment_status': 'Payment status must be "Not Applicable" for non-billable milestones.'
                })
        
        return attrs


class ProjectMilestoneAdminSerializer(ProjectMilestoneSerializer):
    """Serializer for ProjectMilestone model - admin users can modify billing fields"""
    
    class Meta(ProjectMilestoneSerializer.Meta):
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
    project_package = ProjectPackageSerializer(read_only=True)
    client = ClientSerializer(read_only=True)
    assigned_to = UserSerializer(read_only=True)
    milestones = ProjectMilestoneSerializer(many=True, read_only=True)
    tasks = ProjectTaskSerializer(many=True, read_only=True)
    comments = ProjectCommentSerializer(many=True, read_only=True)
    
    class Meta:
        model = ProjectTracker
        fields = ['id', 'subscription', 'project_package', 'client', 'title', 'description', 
                  'project_type', 'status', 'priority', 
                  'progress_percentage', 'estimated_hours', 'actual_hours', 
                  'budget', 'amount_spent', 'start_date', 
                  'estimated_completion_date', 'actual_completion_date', 'assigned_to', 
                  'metadata', 'milestones', 'tasks', 'comments', 'created_at', 'updated_at']
        read_only_fields = ['id', 'created_at', 'updated_at']


class ProjectTrackerCreateSerializer(serializers.ModelSerializer):
    """Serializer for creating ProjectTracker"""
    
    class Meta:
        model = ProjectTracker
        fields = ['id', 'subscription', 'project_package', 'client', 'title', 'description', 
                  'project_type', 'status', 'priority', 
                  'estimated_hours', 'budget', 'start_date', 'estimated_completion_date', 
                  'assigned_to', 'metadata']
        # Prevent clients from arbitrarily assigning ProjectTracker to any client
        read_only_fields = ['id', 'client']

    def validate(self, attrs):
        """
        Enforce that non-admin users can only create project trackers for
        subscriptions they own, and ensure any selected project package is active.
        """
        request = self.context.get('request')
        subscription = attrs.get('subscription')
        project_package = attrs.get('project_package')

        # Validate ownership of the subscription for non-admin users
        if request is not None and subscription is not None:
            user = getattr(request, 'user', None)
            if user is not None and not (user.is_staff or user.is_superuser):
                subscription_client = getattr(subscription, 'client', None)
                if subscription_client is not None:
                    user_client = getattr(user, 'client_profile', None)
                    if user_client is not None and subscription_client.pk != user_client.pk:
                        raise serializers.ValidationError({
                            'subscription': 'You do not have permission to create a project for this subscription.'
                        })

        # Ensure the selected project package is active
        if project_package is not None and not project_package.is_active:
            raise serializers.ValidationError({
                'project_package': 'The selected project package is not active.'
            })

        return attrs

    def create(self, validated_data):
        """
        Derive client from the associated subscription to avoid mismatched or
        forged client relationships.
        """
        subscription = validated_data.get('subscription')
        if subscription is not None:
            validated_data['client'] = subscription.client
        return super().create(validated_data)


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

