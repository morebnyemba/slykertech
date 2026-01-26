from django.db import models
from django.conf import settings
from django.contrib.auth import get_user_model

User = get_user_model()


class Ticket(models.Model):
    """Support ticket model"""
    
    STATUS_CHOICES = [
        ('open', 'Open'),
        ('in_progress', 'In Progress'),
        ('pending', 'Pending'),
        ('resolved', 'Resolved'),
        ('closed', 'Closed')
    ]
    
    PRIORITY_CHOICES = [
        ('low', 'Low'),
        ('medium', 'Medium'),
        ('high', 'High'),
        ('critical', 'Critical')
    ]
    
    DEPARTMENT_CHOICES = [
        ('support', 'Technical Support'),
        ('billing', 'Billing'),
        ('sales', 'Sales'),
        ('general', 'General Inquiry'),
    ]
    
    user = models.ForeignKey(User, on_delete=models.CASCADE, related_name='tickets')
    assigned_to = models.ForeignKey(
        User, on_delete=models.SET_NULL, null=True, blank=True, 
        related_name='assigned_tickets'
    )
    subject = models.CharField(max_length=255)
    description = models.TextField()
    department = models.CharField(max_length=20, choices=DEPARTMENT_CHOICES, default='support')
    status = models.CharField(max_length=20, choices=STATUS_CHOICES, default='open')
    priority = models.CharField(max_length=20, choices=PRIORITY_CHOICES, default='medium')
    metadata = models.JSONField(default=dict, blank=True)
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        ordering = ['-created_at']
    
    def __str__(self):
        return f"#{self.id} - {self.subject}"


class TicketReply(models.Model):
    """Replies to support tickets"""
    
    ticket = models.ForeignKey(Ticket, on_delete=models.CASCADE, related_name='replies')
    user = models.ForeignKey(settings.AUTH_USER_MODEL, on_delete=models.CASCADE)
    message = models.TextField()
    is_staff_reply = models.BooleanField(default=False)
    is_internal = models.BooleanField(default=False, help_text="Internal note not visible to client")
    attachments = models.JSONField(default=list, blank=True)
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)
    
    class Meta:
        ordering = ['created_at']
        verbose_name_plural = 'Ticket replies'
    
    def __str__(self):
        return f"Reply to #{self.ticket.id} by {self.user.email}"
