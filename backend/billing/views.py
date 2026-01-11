from rest_framework import viewsets, permissions, status
from rest_framework.decorators import action
from rest_framework.response import Response
from .models import Invoice, InvoiceItem, Payment, BillingProfile
from .serializers import (
    InvoiceSerializer, InvoiceCreateSerializer, InvoiceItemSerializer,
    PaymentSerializer, BillingProfileSerializer
)


class InvoiceViewSet(viewsets.ModelViewSet):
    """ViewSet for Invoice model"""
    
    queryset = Invoice.objects.all()
    serializer_class = InvoiceSerializer
    permission_classes = [permissions.IsAuthenticated]
    
    def get_queryset(self):
        """Filter invoices based on user"""
        user = self.request.user
        if user.is_superuser or user.user_type == 'admin':
            return Invoice.objects.all()
        # Clients can only see their own invoices
        return Invoice.objects.filter(client__user=user)
    
    def get_serializer_class(self):
        if self.action == 'create':
            return InvoiceCreateSerializer
        return InvoiceSerializer
    
    @action(detail=True, methods=['post'])
    def send(self, request, pk=None):
        """Send invoice to client"""
        invoice = self.get_object()
        invoice.status = 'sent'
        invoice.save()
        
        # TODO: Send notification to client
        from notifications.models import Notification
        Notification.objects.create(
            user=invoice.client.user,
            notification_type='email',
            category='invoice',
            title=f'New Invoice #{invoice.invoice_number}',
            message=f'You have a new invoice for ${invoice.total}. Due date: {invoice.due_date}',
        )
        
        return Response(
            {"message": "Invoice sent successfully."},
            status=status.HTTP_200_OK
        )
    
    @action(detail=True, methods=['post'])
    def mark_paid(self, request, pk=None):
        """Mark invoice as paid"""
        invoice = self.get_object()
        invoice.status = 'paid'
        from django.utils import timezone
        invoice.paid_date = timezone.now().date()
        invoice.save()
        
        return Response(
            {"message": "Invoice marked as paid."},
            status=status.HTTP_200_OK
        )


class PaymentViewSet(viewsets.ModelViewSet):
    """ViewSet for Payment model"""
    
    queryset = Payment.objects.all()
    serializer_class = PaymentSerializer
    permission_classes = [permissions.IsAuthenticated]
    
    def get_queryset(self):
        """Filter payments based on user"""
        user = self.request.user
        if user.is_superuser or user.user_type == 'admin':
            return Payment.objects.all()
        # Clients can only see their own payments
        return Payment.objects.filter(invoice__client__user=user)


class BillingProfileViewSet(viewsets.ModelViewSet):
    """ViewSet for BillingProfile model"""
    
    queryset = BillingProfile.objects.all()
    serializer_class = BillingProfileSerializer
    permission_classes = [permissions.IsAuthenticated]
    
    def get_queryset(self):
        """Filter billing profiles based on user"""
        user = self.request.user
        if user.is_superuser or user.user_type == 'admin':
            return BillingProfile.objects.all()
        # Clients can only see their own billing profile
        return BillingProfile.objects.filter(client__user=user)

