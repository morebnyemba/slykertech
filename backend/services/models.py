from django.db import models
from django.conf import settings
from django.utils.translation import gettext_lazy as _


class Service(models.Model):
    """Services offered by the company"""
    
    SERVICE_CATEGORY_CHOICES = [
        ('hosting', 'Web Hosting'),
        ('domain', 'Domain Services'),
        ('development', 'Development'),
        ('design', 'Design'),
        ('marketing', 'Marketing'),
        ('maintenance', 'Maintenance'),
        ('cloud', 'Cloud Services'),
        ('other', 'Other'),
    ]
    
    PAYMENT_TYPE_CHOICES = [
        ('one_time', 'One Time'),
        ('recurring', 'Recurring'),
        ('both', 'Both Options Available'),
    ]
    
    name = models.CharField(max_length=255)
    category = models.CharField(max_length=50, choices=SERVICE_CATEGORY_CHOICES)
    description = models.TextField()
    features = models.JSONField(default=list, blank=True)
    base_price = models.DecimalField(max_digits=10, decimal_places=2, null=True, blank=True,
                                     help_text="Selling price to clients")
    cost_price = models.DecimalField(max_digits=10, decimal_places=2, null=True, blank=True,
                                     help_text="Cost price (e.g., cPanel license, domain registry cost)")
    payment_type = models.CharField(max_length=20, choices=PAYMENT_TYPE_CHOICES, default='one_time',
                                    help_text="Payment structure for this service")
    pricing_options = models.JSONField(default=dict, blank=True,
                                      help_text="Different pricing tiers/options for this service")
    
    # Polymorphic metadata field for service-specific attributes
    # For Hosting: {types: ['shared', 'vps', 'dedicated'], regions: ['US', 'EU'], os_options: ['Ubuntu', 'CentOS']}
    # For Development: {types: ['web', 'mobile', 'desktop', 'hybrid'], requires_brief: true}
    # For Domains: {actions: ['registration', 'transfer'], requires_epp_for_transfer: true, tlds: ['.com', '.net']}
    service_metadata = models.JSONField(default=dict, blank=True,
                                       help_text="Service-specific configuration and options")
    
    requires_provisioning = models.BooleanField(default=False,
                                               help_text="Requires automatic provisioning (e.g., cPanel account)")
    provisioning_type = models.CharField(max_length=50, blank=True, null=True,
                                        help_text="Type of provisioning needed: cpanel, directadmin, domain, etc.")
    
    # Many-to-many relationship to allow selecting services as recommended addons
    # E.g., Domain Registration can show Web Hosting or Web Dev as addon options
    recommended_addons = models.ManyToManyField('self', symmetrical=False, blank=True,
                                                related_name='addon_of_services',
                                                help_text="Services to recommend as addons when purchasing this service")
    
    is_active = models.BooleanField(default=True)
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        verbose_name = _('service')
        verbose_name_plural = _('services')
        ordering = ['name']
    
    def __str__(self):
        return self.name


class ServiceSubscription(models.Model):
    """Client subscriptions to services"""
    
    STATUS_CHOICES = [
        ('active', 'Active'),
        ('pending', 'Pending'),
        ('provisioning', 'Provisioning'),
        ('suspended', 'Suspended'),
        ('cancelled', 'Cancelled'),
        ('expired', 'Expired'),
        ('terminated', 'Terminated'),
    ]
    
    BILLING_CYCLE_CHOICES = [
        ('monthly', 'Monthly'),
        ('quarterly', 'Quarterly'),
        ('semi_annual', 'Semi-Annual'),
        ('annual', 'Annual'),
        ('biennial', 'Biennial (2 years)'),
        ('triennial', 'Triennial (3 years)'),
        ('one_time', 'One Time'),
    ]
    
    client = models.ForeignKey('clients.Client', on_delete=models.CASCADE, related_name='subscriptions')
    service = models.ForeignKey(Service, on_delete=models.CASCADE, related_name='subscriptions')
    
    # WHMCS-style: Link to specific hosting product if applicable
    hosting_product = models.ForeignKey('services.HostingProduct', on_delete=models.SET_NULL, 
                                       null=True, blank=True, related_name='subscriptions',
                                       help_text="Hosting package if service is web hosting")
    
    status = models.CharField(max_length=20, choices=STATUS_CHOICES, default='pending')
    billing_cycle = models.CharField(max_length=20, choices=BILLING_CYCLE_CHOICES, default='monthly')
    price = models.DecimalField(max_digits=10, decimal_places=2)
    start_date = models.DateField()
    end_date = models.DateField(null=True, blank=True)
    
    # WHMCS-style renewal management
    auto_renew = models.BooleanField(default=True)
    next_due_date = models.DateField(null=True, blank=True, help_text="Next invoice due date")
    next_invoice_date = models.DateField(null=True, blank=True, help_text="When to generate next invoice")
    
    # WHMCS-style renewal reminders
    renewal_reminder_30d = models.BooleanField(default=False)
    renewal_reminder_14d = models.BooleanField(default=False)
    renewal_reminder_7d = models.BooleanField(default=False)
    renewal_reminder_1d = models.BooleanField(default=False)
    
    # WHMCS-style suspension management
    grace_period_days = models.IntegerField(default=14, help_text="Days after due date before suspension")
    suspension_date = models.DateField(null=True, blank=True, help_text="Date service was suspended")
    termination_date = models.DateField(null=True, blank=True, help_text="Date service was terminated")
    
    notes = models.TextField(blank=True, null=True)
    metadata = models.JSONField(default=dict, blank=True, help_text="Additional service-specific data")
    provisioning_completed = models.BooleanField(default=False, help_text="Has automated provisioning completed?")
    provisioning_error = models.TextField(blank=True, null=True, help_text="Error message if provisioning failed")
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        verbose_name = _('service subscription')
        verbose_name_plural = _('service subscriptions')
        ordering = ['-created_at']
    
    def __str__(self):
        return f"{self.client.company_name} - {self.service.name}"


class DNSRecord(models.Model):
    """DNS records management for clients"""
    
    RECORD_TYPE_CHOICES = [
        ('A', 'A'),
        ('AAAA', 'AAAA'),
        ('CNAME', 'CNAME'),
        ('MX', 'MX'),
        ('TXT', 'TXT'),
        ('NS', 'NS'),
        ('SRV', 'SRV'),
        ('CAA', 'CAA'),
    ]
    
    subscription = models.ForeignKey(ServiceSubscription, on_delete=models.CASCADE, related_name='dns_records')
    domain = models.CharField(max_length=255)
    record_type = models.CharField(max_length=10, choices=RECORD_TYPE_CHOICES)
    name = models.CharField(max_length=255)
    content = models.TextField()
    ttl = models.IntegerField(default=3600)
    priority = models.IntegerField(null=True, blank=True)
    is_active = models.BooleanField(default=True)
    external_id = models.CharField(max_length=255, blank=True, null=True, help_text="ID from external DNS provider")
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        verbose_name = _('DNS record')
        verbose_name_plural = _('DNS records')
        ordering = ['domain', 'record_type', 'name']
    
    def __str__(self):
        return f"{self.domain} - {self.record_type} - {self.name}"


class ProjectPackage(models.Model):
    """Predefined project packages/tiers (e.g., Basic Website, E-commerce, Custom App)"""
    
    PROJECT_TYPE_CHOICES = [
        ('web_development', 'Web Development'),
        ('mobile_app', 'Mobile App Development'),
        ('ecommerce', 'E-Commerce'),
        ('seo', 'SEO & Marketing'),
        ('design', 'Graphic Design'),
        ('branding', 'Branding'),
        ('maintenance', 'Maintenance & Support'),
        ('custom', 'Custom Project'),
    ]
    
    name = models.CharField(max_length=255, help_text="e.g., Basic Website, E-commerce Store")
    slug = models.SlugField(unique=True)
    description = models.TextField()
    project_type = models.CharField(max_length=30, choices=PROJECT_TYPE_CHOICES, default='web_development')
    
    # Deliverables included in this package
    deliverables = models.JSONField(default=list, blank=True,
                                   help_text="List of deliverables included (e.g., ['5-page website', 'Contact form', 'SEO setup'])")
    
    # Scope and timeline
    estimated_duration_days = models.IntegerField(default=30, help_text="Estimated project duration in days")
    max_revisions = models.IntegerField(default=3, help_text="Maximum number of revisions included")
    
    # Pricing
    base_price = models.DecimalField(max_digits=10, decimal_places=2, help_text="Base package price")
    
    # Display settings
    is_featured = models.BooleanField(default=False)
    sort_order = models.IntegerField(default=0)
    is_active = models.BooleanField(default=True)
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        verbose_name = _('project package')
        verbose_name_plural = _('project packages')
        ordering = ['sort_order', 'name']
    
    def __str__(self):
        return self.name


class ProjectTracker(models.Model):
    """Track progress for projects like web development, SEO, design, etc."""
    
    STATUS_CHOICES = [
        ('not_started', 'Not Started'),
        ('planning', 'Planning'),
        ('in_progress', 'In Progress'),
        ('review', 'Under Review'),
        ('completed', 'Completed'),
        ('on_hold', 'On Hold'),
        ('cancelled', 'Cancelled'),
    ]
    
    PRIORITY_CHOICES = [
        ('low', 'Low'),
        ('medium', 'Medium'),
        ('high', 'High'),
        ('urgent', 'Urgent'),
    ]
    
    PROJECT_TYPE_CHOICES = [
        ('web_development', 'Web Development'),
        ('mobile_app', 'Mobile App Development'),
        ('ecommerce', 'E-Commerce'),
        ('seo', 'SEO & Marketing'),
        ('design', 'Graphic Design'),
        ('branding', 'Branding'),
        ('maintenance', 'Maintenance & Support'),
        ('custom', 'Custom Project'),
    ]
    
    subscription = models.ForeignKey(ServiceSubscription, on_delete=models.CASCADE, related_name='project_trackers')
    project_package = models.ForeignKey(ProjectPackage, on_delete=models.SET_NULL,
                                       null=True, blank=True, related_name='projects',
                                       help_text="Package/tier this project is based on")
    client = models.ForeignKey('clients.Client', on_delete=models.CASCADE,
                              related_name='projects', null=True, blank=True,
                              help_text="Direct client reference for easier querying")
    title = models.CharField(max_length=255)
    description = models.TextField(blank=True, null=True)
    project_type = models.CharField(max_length=30, choices=PROJECT_TYPE_CHOICES, 
                                   default='web_development', help_text="Type/category of project")
    status = models.CharField(max_length=20, choices=STATUS_CHOICES, default='not_started')
    priority = models.CharField(max_length=20, choices=PRIORITY_CHOICES, default='medium')
    
    # Progress tracking
    progress_percentage = models.IntegerField(default=0, help_text="Progress from 0 to 100")
    estimated_hours = models.DecimalField(max_digits=6, decimal_places=2, null=True, blank=True)
    actual_hours = models.DecimalField(max_digits=6, decimal_places=2, default=0)
    
    # Budget tracking
    budget = models.DecimalField(max_digits=10, decimal_places=2, null=True, blank=True,
                                help_text="Total project budget")
    amount_spent = models.DecimalField(max_digits=10, decimal_places=2, default=0,
                                      help_text="Amount spent so far")
    
    # Dates
    start_date = models.DateField(null=True, blank=True)
    estimated_completion_date = models.DateField(null=True, blank=True)
    actual_completion_date = models.DateField(null=True, blank=True)
    
    # Team assignment
    assigned_to = models.ForeignKey(settings.AUTH_USER_MODEL, on_delete=models.SET_NULL, 
                                   null=True, blank=True, related_name='assigned_projects')
    
    # Additional metadata
    metadata = models.JSONField(default=dict, blank=True, help_text="Additional project-specific data")
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        verbose_name = _('project tracker')
        verbose_name_plural = _('project trackers')
        ordering = ['-created_at']
    
    def __str__(self):
        return f"{self.title} - {self.get_status_display()}"
    
    def update_progress(self, percentage):
        """Update progress percentage"""
        if 0 <= percentage <= 100:
            self.progress_percentage = percentage
            if percentage == 100 and not self.actual_completion_date:
                from django.utils import timezone
                self.actual_completion_date = timezone.now().date()
                self.status = 'completed'
            self.save()


class ProjectMilestone(models.Model):
    """Milestones for project tracking"""
    
    STATUS_CHOICES = [
        ('pending', 'Pending'),
        ('in_progress', 'In Progress'),
        ('completed', 'Completed'),
        ('skipped', 'Skipped'),
    ]
    
    project = models.ForeignKey(ProjectTracker, on_delete=models.CASCADE, related_name='milestones')
    title = models.CharField(max_length=255)
    description = models.TextField(blank=True, null=True)
    status = models.CharField(max_length=20, choices=STATUS_CHOICES, default='pending')
    
    # Deliverables for this milestone
    deliverables = models.JSONField(default=list, blank=True,
                                   help_text="List of deliverables for this milestone")
    
    # Payment milestone tracking
    is_billable = models.BooleanField(default=False, help_text="Whether this milestone triggers a payment")
    amount = models.DecimalField(max_digits=10, decimal_places=2, null=True, blank=True,
                                help_text="Payment amount due upon completion of this milestone")
    payment_status = models.CharField(max_length=20, choices=[
        ('not_applicable', 'Not Applicable'),
        ('pending', 'Payment Pending'),
        ('invoiced', 'Invoiced'),
        ('paid', 'Paid'),
    ], default='not_applicable', help_text="Payment status for billable milestones")
    
    due_date = models.DateField(null=True, blank=True)
    completed_date = models.DateField(null=True, blank=True)
    
    order = models.IntegerField(default=0, help_text="Display order")
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        verbose_name = _('project milestone')
        verbose_name_plural = _('project milestones')
        ordering = ['project', 'order', 'due_date']
    
    def __str__(self):
        return f"{self.project.title} - {self.title}"


class ProjectTask(models.Model):
    """Tasks within project milestones"""
    
    STATUS_CHOICES = [
        ('todo', 'To Do'),
        ('in_progress', 'In Progress'),
        ('completed', 'Completed'),
        ('blocked', 'Blocked'),
    ]
    
    PRIORITY_CHOICES = [
        ('low', 'Low'),
        ('medium', 'Medium'),
        ('high', 'High'),
        ('urgent', 'Urgent'),
    ]
    
    milestone = models.ForeignKey(ProjectMilestone, on_delete=models.CASCADE, related_name='tasks', 
                                 null=True, blank=True)
    project = models.ForeignKey(ProjectTracker, on_delete=models.CASCADE, related_name='tasks')
    
    title = models.CharField(max_length=255)
    description = models.TextField(blank=True, null=True)
    status = models.CharField(max_length=20, choices=STATUS_CHOICES, default='todo')
    priority = models.CharField(max_length=20, choices=PRIORITY_CHOICES, default='medium',
                               help_text="Task priority level")
    
    assigned_to = models.ForeignKey(settings.AUTH_USER_MODEL, on_delete=models.SET_NULL, 
                                   null=True, blank=True, related_name='assigned_tasks')
    
    # Dependencies
    depends_on = models.ForeignKey('self', on_delete=models.SET_NULL,
                                  null=True, blank=True, related_name='dependents',
                                  help_text="Task that must be completed before this one")
    
    estimated_hours = models.DecimalField(max_digits=5, decimal_places=2, null=True, blank=True)
    actual_hours = models.DecimalField(max_digits=5, decimal_places=2, default=0)
    
    due_date = models.DateField(null=True, blank=True)
    completed_date = models.DateField(null=True, blank=True)
    
    order = models.IntegerField(default=0)
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        verbose_name = _('project task')
        verbose_name_plural = _('project tasks')
        ordering = ['project', 'milestone', 'order']
    
    def __str__(self):
        return f"{self.title} - {self.get_status_display()}"


class ProjectComment(models.Model):
    """Comments and updates on projects"""
    
    project = models.ForeignKey(ProjectTracker, on_delete=models.CASCADE, related_name='comments')
    user = models.ForeignKey(settings.AUTH_USER_MODEL, on_delete=models.CASCADE)
    
    comment = models.TextField()
    is_internal = models.BooleanField(default=False, help_text="Internal note not visible to client")
    
    # Attachments
    attachments = models.JSONField(default=list, blank=True, help_text="List of attachment URLs")
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        verbose_name = _('project comment')
        verbose_name_plural = _('project comments')
        ordering = ['-created_at']
    
    def __str__(self):
        return f"Comment on {self.project.title} by {self.user.email}"


class ProvisioningFailure(models.Model):
    """Track failed provisioning attempts for admin review and manual intervention"""
    
    STATUS_CHOICES = [
        ('pending', 'Pending Review'),
        ('in_progress', 'In Progress'),
        ('resolved', 'Resolved'),
        ('dismissed', 'Dismissed'),
    ]
    
    subscription = models.ForeignKey(ServiceSubscription, on_delete=models.CASCADE, related_name='provisioning_failures')
    error_message = models.TextField(help_text="Error message from provisioning attempt")
    error_details = models.JSONField(default=dict, blank=True, help_text="Additional error details")
    
    status = models.CharField(max_length=20, choices=STATUS_CHOICES, default='pending')
    
    # Manual provisioning data for admin to copy/paste
    provisioning_data = models.JSONField(default=dict, blank=True,
                                        help_text="Data needed for manual provisioning (username, domain, etc.)")
    
    # Admin notes and resolution details
    admin_notes = models.TextField(blank=True, null=True)
    resolved_by = models.ForeignKey(settings.AUTH_USER_MODEL, on_delete=models.SET_NULL, 
                                   null=True, blank=True, related_name='resolved_failures')
    resolved_at = models.DateTimeField(null=True, blank=True)
    
    # Notification tracking
    admin_notified = models.BooleanField(default=False, help_text="Whether admin has been notified")
    notification_sent_at = models.DateTimeField(null=True, blank=True)
    
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        verbose_name = _('provisioning failure')
        verbose_name_plural = _('provisioning failures')
        ordering = ['-created_at']
    
    def __str__(self):
        return f"Failure: {self.subscription} - {self.status}"
    
    def mark_resolved(self, user, notes=None):
        """Mark this failure as resolved"""
        from django.utils import timezone
        self.status = 'resolved'
        self.resolved_by = user
        self.resolved_at = timezone.now()
        if notes:
            self.admin_notes = notes
        self.save()


# Import WHMCS models to make them available in the services app
from .whmcs_models import (
    HostingProduct,
    DomainProduct,
    ServiceAddon,
    DomainRegistration,
    DomainTransferRequest,
)

__all__ = [
    'Service',
    'ServiceSubscription',
    'DNSRecord',
    'ProjectPackage',
    'ProjectTracker',
    'ProjectTask',
    'ProjectMilestone',
    'ProjectComment',
    'ProvisioningFailure',
    'HostingProduct',
    'DomainProduct',
    'ServiceAddon',
    'DomainRegistration',
    'DomainTransferRequest',
]


