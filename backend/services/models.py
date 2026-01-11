from django.db import models
from django.conf import settings
from django.utils.translation import gettext_lazy as _


class Service(models.Model):
    """Services offered by the company"""
    
    SERVICE_CATEGORY_CHOICES = [
        ('hosting', 'Web Hosting'),
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
    base_price = models.DecimalField(max_digits=10, decimal_places=2, null=True, blank=True)
    payment_type = models.CharField(max_length=20, choices=PAYMENT_TYPE_CHOICES, default='one_time',
                                    help_text="Payment structure for this service")
    pricing_options = models.JSONField(default=dict, blank=True,
                                      help_text="Different pricing tiers/options for this service")
    requires_provisioning = models.BooleanField(default=False,
                                               help_text="Requires automatic provisioning (e.g., cPanel account)")
    provisioning_type = models.CharField(max_length=50, blank=True, null=True,
                                        help_text="Type of provisioning needed: cpanel, directadmin, domain, etc.")
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
    ]
    
    BILLING_CYCLE_CHOICES = [
        ('monthly', 'Monthly'),
        ('quarterly', 'Quarterly'),
        ('semi_annual', 'Semi-Annual'),
        ('annual', 'Annual'),
        ('one_time', 'One Time'),
    ]
    
    client = models.ForeignKey('clients.Client', on_delete=models.CASCADE, related_name='subscriptions')
    service = models.ForeignKey(Service, on_delete=models.CASCADE, related_name='subscriptions')
    status = models.CharField(max_length=20, choices=STATUS_CHOICES, default='pending')
    billing_cycle = models.CharField(max_length=20, choices=BILLING_CYCLE_CHOICES, default='monthly')
    price = models.DecimalField(max_digits=10, decimal_places=2)
    start_date = models.DateField()
    end_date = models.DateField(null=True, blank=True)
    auto_renew = models.BooleanField(default=True)
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
    
    subscription = models.ForeignKey(ServiceSubscription, on_delete=models.CASCADE, related_name='project_trackers')
    title = models.CharField(max_length=255)
    description = models.TextField(blank=True, null=True)
    status = models.CharField(max_length=20, choices=STATUS_CHOICES, default='not_started')
    priority = models.CharField(max_length=20, choices=PRIORITY_CHOICES, default='medium')
    
    # Progress tracking
    progress_percentage = models.IntegerField(default=0, help_text="Progress from 0 to 100")
    estimated_hours = models.DecimalField(max_digits=6, decimal_places=2, null=True, blank=True)
    actual_hours = models.DecimalField(max_digits=6, decimal_places=2, default=0)
    
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
    
    milestone = models.ForeignKey(ProjectMilestone, on_delete=models.CASCADE, related_name='tasks', 
                                 null=True, blank=True)
    project = models.ForeignKey(ProjectTracker, on_delete=models.CASCADE, related_name='tasks')
    
    title = models.CharField(max_length=255)
    description = models.TextField(blank=True, null=True)
    status = models.CharField(max_length=20, choices=STATUS_CHOICES, default='todo')
    
    assigned_to = models.ForeignKey(settings.AUTH_USER_MODEL, on_delete=models.SET_NULL, 
                                   null=True, blank=True, related_name='assigned_tasks')
    
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


