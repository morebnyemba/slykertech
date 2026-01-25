from django.contrib import admin
from .models import (Service, ServiceSubscription, DNSRecord, 
                    ProjectTracker, ProjectMilestone, ProjectTask, ProjectComment,
                    HostingProduct, DomainProduct, ServiceAddon, DomainRegistration,
                    DomainTransferRequest)


@admin.register(Service)
class ServiceAdmin(admin.ModelAdmin):
    list_display = ('name', 'category', 'base_price', 'is_addon', 'parent_service', 'is_active', 'created_at')
    list_filter = ('category', 'is_active', 'is_addon', 'created_at')
    search_fields = ('name', 'description')
    readonly_fields = ('created_at', 'updated_at')
    raw_id_fields = ('parent_service',)
    
    fieldsets = (
        ('Basic Information', {
            'fields': ('name', 'category', 'description', 'features', 'base_price')
        }),
        ('Payment & Pricing', {
            'fields': ('payment_type', 'pricing_options', 'service_metadata')
        }),
        ('Provisioning', {
            'fields': ('requires_provisioning', 'provisioning_type')
        }),
        ('Service Addon Configuration', {
            'fields': ('is_addon', 'parent_service'),
            'description': 'Configure this service as an addon to another service'
        }),
        ('Status', {
            'fields': ('is_active',)
        }),
        ('Timestamps', {
            'fields': ('created_at', 'updated_at'),
            'classes': ('collapse',)
        }),
    )


class DNSRecordInline(admin.TabularInline):
    model = DNSRecord
    extra = 1


@admin.register(ServiceSubscription)
class ServiceSubscriptionAdmin(admin.ModelAdmin):
    list_display = ('client', 'service', 'status', 'billing_cycle', 'start_date', 'end_date', 'price')
    list_filter = ('status', 'billing_cycle', 'auto_renew', 'created_at')
    search_fields = ('client__company_name', 'service__name')
    readonly_fields = ('created_at', 'updated_at')
    inlines = [DNSRecordInline]
    
    fieldsets = (
        ('Subscription Details', {
            'fields': ('client', 'service', 'status', 'billing_cycle', 'price')
        }),
        ('Dates', {
            'fields': ('start_date', 'end_date', 'auto_renew')
        }),
        ('Additional Information', {
            'fields': ('notes', 'metadata')
        }),
        ('Timestamps', {
            'fields': ('created_at', 'updated_at'),
            'classes': ('collapse',)
        }),
    )


@admin.register(DNSRecord)
class DNSRecordAdmin(admin.ModelAdmin):
    list_display = ('domain', 'record_type', 'name', 'content', 'ttl', 'is_active')
    list_filter = ('record_type', 'is_active', 'created_at')
    search_fields = ('domain', 'name', 'content')
    readonly_fields = ('created_at', 'updated_at')


class ProjectMilestoneInline(admin.TabularInline):
    model = ProjectMilestone
    extra = 1
    fields = ('title', 'status', 'due_date', 'order')


class ProjectTaskInline(admin.TabularInline):
    model = ProjectTask
    extra = 1
    fields = ('title', 'status', 'assigned_to', 'due_date')


@admin.register(ProjectTracker)
class ProjectTrackerAdmin(admin.ModelAdmin):
    list_display = ('title', 'subscription', 'status', 'priority', 'progress_percentage', 
                   'assigned_to', 'estimated_completion_date')
    list_filter = ('status', 'priority', 'created_at')
    search_fields = ('title', 'description', 'subscription__client__company_name')
    readonly_fields = ('created_at', 'updated_at')
    inlines = [ProjectMilestoneInline, ProjectTaskInline]
    
    fieldsets = (
        ('Project Information', {
            'fields': ('subscription', 'title', 'description', 'status', 'priority')
        }),
        ('Progress Tracking', {
            'fields': ('progress_percentage', 'estimated_hours', 'actual_hours')
        }),
        ('Dates', {
            'fields': ('start_date', 'estimated_completion_date', 'actual_completion_date')
        }),
        ('Assignment', {
            'fields': ('assigned_to',)
        }),
        ('Additional Data', {
            'fields': ('metadata',)
        }),
        ('Timestamps', {
            'fields': ('created_at', 'updated_at'),
            'classes': ('collapse',)
        }),
    )


@admin.register(ProjectMilestone)
class ProjectMilestoneAdmin(admin.ModelAdmin):
    list_display = ('title', 'project', 'status', 'due_date', 'order')
    list_filter = ('status', 'created_at')
    search_fields = ('title', 'project__title')
    readonly_fields = ('created_at', 'updated_at')


@admin.register(ProjectTask)
class ProjectTaskAdmin(admin.ModelAdmin):
    list_display = ('title', 'project', 'milestone', 'status', 'assigned_to', 'due_date')
    list_filter = ('status', 'created_at')
    search_fields = ('title', 'project__title')
    readonly_fields = ('created_at', 'updated_at')


@admin.register(ProjectComment)
class ProjectCommentAdmin(admin.ModelAdmin):
    list_display = ('project', 'user', 'is_internal', 'created_at')
    list_filter = ('is_internal', 'created_at')
    search_fields = ('project__title', 'user__email', 'comment')
    readonly_fields = ('created_at', 'updated_at')


@admin.register(HostingProduct)
class HostingProductAdmin(admin.ModelAdmin):
    """Admin for managing hosting products/packages with TLD pricing"""
    list_display = ('name', 'hosting_type', 'monthly_price', 'annual_price', 
                   'is_featured', 'is_active', 'sort_order')
    list_filter = ('hosting_type', 'is_active', 'is_featured', 'created_at')
    search_fields = ('name', 'description', 'slug')
    readonly_fields = ('created_at', 'updated_at')
    prepopulated_fields = {'slug': ('name',)}
    list_editable = ('sort_order', 'is_featured', 'is_active')
    
    fieldsets = (
        ('Basic Information', {
            'fields': ('name', 'slug', 'description', 'hosting_type')
        }),
        ('Resource Limits', {
            'fields': ('disk_space', 'bandwidth', 'email_accounts', 'databases', 
                      'ftp_accounts', 'subdomains', 'addon_domains', 'parked_domains')
        }),
        ('Features', {
            'fields': ('ssl_certificate', 'dedicated_ip', 'cpanel_access', 
                      'ssh_access', 'cron_jobs', 'backups_included')
        }),
        ('Pricing', {
            'fields': ('monthly_price', 'quarterly_price', 'semi_annual_price', 
                      'annual_price', 'biennial_price', 'triennial_price'),
            'description': 'Set prices for different billing cycles'
        }),
        ('Setup Fees', {
            'fields': ('setup_fee_monthly', 'setup_fee_quarterly', 'setup_fee_annual'),
            'classes': ('collapse',)
        }),
        ('Server Configuration', {
            'fields': ('server_id', 'cpanel_package_name'),
            'classes': ('collapse',)
        }),
        ('Display Settings', {
            'fields': ('is_featured', 'sort_order', 'is_active')
        }),
        ('Timestamps', {
            'fields': ('created_at', 'updated_at'),
            'classes': ('collapse',)
        }),
    )


@admin.register(DomainProduct)
class DomainProductAdmin(admin.ModelAdmin):
    """Admin for managing domain TLD pricing - allows setting prices per TLD"""
    list_display = ('tld', 'registration_price_1yr', 'renewal_price', 'transfer_price',
                   'is_featured', 'is_active', 'sort_order')
    list_filter = ('is_active', 'is_featured', 'epp_code_required', 'created_at')
    search_fields = ('tld', 'description')
    readonly_fields = ('created_at', 'updated_at')
    list_editable = ('registration_price_1yr', 'renewal_price', 'transfer_price', 
                    'sort_order', 'is_featured', 'is_active')
    ordering = ('sort_order', 'tld')
    
    fieldsets = (
        ('TLD Information', {
            'fields': ('tld', 'description')
        }),
        ('Registration Pricing', {
            'fields': ('registration_price_1yr', 'registration_price_2yr', 
                      'registration_price_3yr', 'registration_price_5yr', 
                      'registration_price_10yr'),
            'description': 'Set prices for different registration periods'
        }),
        ('Renewal & Transfer Pricing', {
            'fields': ('renewal_price', 'transfer_price', 'redemption_price'),
            'description': 'Annual renewal, transfer-in, and redemption fees'
        }),
        ('Add-on Pricing', {
            'fields': ('whois_privacy_price',),
            'description': 'WHOIS/ID privacy protection fee'
        }),
        ('Domain Settings', {
            'fields': ('auto_renew_default', 'epp_code_required', 
                      'grace_period_days', 'redemption_period_days',
                      'min_registration_years', 'max_registration_years')
        }),
        ('Display Settings', {
            'fields': ('is_active', 'is_featured', 'sort_order')
        }),
        ('Timestamps', {
            'fields': ('created_at', 'updated_at'),
            'classes': ('collapse',)
        }),
    )


@admin.register(ServiceAddon)
class ServiceAddonAdmin(admin.ModelAdmin):
    """Admin for managing service add-ons"""
    list_display = ('name', 'addon_type', 'billing_type', 'monthly_price', 
                   'is_active', 'requires_hosting')
    list_filter = ('addon_type', 'billing_type', 'is_active', 'requires_hosting')
    search_fields = ('name', 'description')
    readonly_fields = ('created_at', 'updated_at')
    filter_horizontal = ('compatible_products',)
    
    fieldsets = (
        ('Basic Information', {
            'fields': ('name', 'description', 'addon_type', 'billing_type')
        }),
        ('Pricing', {
            'fields': ('monthly_price', 'quarterly_price', 'annual_price', 'one_time_price')
        }),
        ('Configuration', {
            'fields': ('quantity', 'is_active', 'requires_hosting', 'compatible_products')
        }),
        ('Timestamps', {
            'fields': ('created_at', 'updated_at'),
            'classes': ('collapse',)
        }),
    )


@admin.register(DomainRegistration)
class DomainRegistrationAdmin(admin.ModelAdmin):
    """Admin for managing domain registrations"""
    list_display = ('domain_name', 'client', 'status', 'registration_date', 
                   'expiry_date', 'auto_renew', 'whois_privacy')
    list_filter = ('status', 'auto_renew', 'whois_privacy', 'is_transfer', 'created_at')
    search_fields = ('domain_name', 'client__company_name', 'client__user__email')
    readonly_fields = ('created_at', 'updated_at')
    raw_id_fields = ('client', 'domain_product')
    date_hierarchy = 'expiry_date'
    
    fieldsets = (
        ('Domain Information', {
            'fields': ('client', 'domain_name', 'domain_product', 'status')
        }),
        ('Registration Details', {
            'fields': ('registration_date', 'expiry_date', 'registration_years', 'is_transfer')
        }),
        ('Settings', {
            'fields': ('auto_renew', 'whois_privacy', 'whois_privacy_expiry')
        }),
        ('Transfer Information', {
            'fields': ('epp_code',),
            'classes': ('collapse',),
            'description': 'EPP/Authorization code for domain transfers'
        }),
        ('Nameservers', {
            'fields': ('nameserver1', 'nameserver2', 'nameserver3', 
                      'nameserver4', 'nameserver5')
        }),
        ('Registrar Details', {
            'fields': ('registrar_id', 'notes'),
            'classes': ('collapse',)
        }),
        ('Timestamps', {
            'fields': ('created_at', 'updated_at'),
            'classes': ('collapse',)
        }),
    )


@admin.register(DomainTransferRequest)
class DomainTransferRequestAdmin(admin.ModelAdmin):
    """Admin for managing domain transfer requests"""
    list_display = ('domain_name', 'contact_email', 'status', 'owns_domain', 
                   'current_registrar', 'created_at')
    list_filter = ('status', 'owns_domain', 'whois_privacy', 'auto_renew', 'created_at')
    search_fields = ('domain_name', 'contact_email', 'contact_name', 'current_registrar')
    readonly_fields = ('created_at', 'updated_at', 'completed_at')
    raw_id_fields = ('client',)
    date_hierarchy = 'created_at'
    
    fieldsets = (
        ('Domain Information', {
            'fields': ('domain_name', 'current_registrar', 'status', 'status_message')
        }),
        ('Contact Information', {
            'fields': ('client', 'contact_name', 'contact_email', 'contact_phone')
        }),
        ('Transfer Credentials', {
            'fields': ('epp_code', 'admin_email', 'owns_domain'),
            'description': 'EPP/Authorization code and ownership verification'
        }),
        ('Nameserver Settings', {
            'fields': ('update_nameservers', 'nameserver1', 'nameserver2'),
            'classes': ('collapse',)
        }),
        ('Additional Options', {
            'fields': ('whois_privacy', 'auto_renew')
        }),
        ('Admin Notes', {
            'fields': ('admin_notes',),
            'classes': ('collapse',)
        }),
        ('Timestamps', {
            'fields': ('created_at', 'updated_at', 'completed_at'),
            'classes': ('collapse',)
        }),
    )
    
    actions = ['mark_in_progress', 'mark_completed', 'mark_cancelled']
    
    def mark_in_progress(self, request, queryset):
        queryset.update(status='in_progress')
        self.message_user(request, f"{queryset.count()} transfer(s) marked as in progress.")
    mark_in_progress.short_description = "Mark selected as In Progress"
    
    def mark_completed(self, request, queryset):
        from django.utils import timezone
        queryset.update(status='completed', completed_at=timezone.now())
        self.message_user(request, f"{queryset.count()} transfer(s) marked as completed.")
    mark_completed.short_description = "Mark selected as Completed"
    
    def mark_cancelled(self, request, queryset):
        queryset.update(status='cancelled')
        self.message_user(request, f"{queryset.count()} transfer(s) marked as cancelled.")
    mark_cancelled.short_description = "Mark selected as Cancelled"

