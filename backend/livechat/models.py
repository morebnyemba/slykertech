from django.db import models
from django.contrib.auth import get_user_model

User = get_user_model()


class ChatSession(models.Model):
    user = models.ForeignKey(User, on_delete=models.CASCADE)
    session_id = models.CharField(max_length=100, unique=True)
    status = models.CharField(max_length=20, choices=[
        ('active', 'Active'),
        ('closed', 'Closed'),
        ('transferred', 'Transferred')
    ], default='active')
    created_at = models.DateTimeField(auto_now_add=True)
    closed_at = models.DateTimeField(null=True, blank=True)


class ChatMessage(models.Model):
    session = models.ForeignKey(ChatSession, on_delete=models.CASCADE, related_name='messages')
    sender_type = models.CharField(max_length=10, choices=[
        ('user', 'User'),
        ('ai', 'AI'),
        ('agent', 'Agent')
    ])
    message = models.TextField()
    created_at = models.DateTimeField(auto_now_add=True)
