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
    
    @action(detail=True, methods=['post'])
    def paynow_payment(self, request, pk=None):
        """
        Initiate Paynow payment for invoice
        
        Request body:
        {
            "email": "customer@example.com",
            "mobile_payment": false,
            "phone": "263771234567",  // Required if mobile_payment=true
            "method": "ecocash"  // ecocash or onemoney, required if mobile_payment=true
        }
        """
        invoice = self.get_object()
        
        email = request.data.get('email')
        mobile_payment = request.data.get('mobile_payment', False)
        phone = request.data.get('phone')
        method = request.data.get('method', 'ecocash')
        
        if not email and not mobile_payment:
            return Response(
                {"error": "Email is required for web payment"},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        try:
            from .paynow_service import PaynowService
            paynow = PaynowService()
            
            result = paynow.process_invoice_payment(
                invoice=invoice,
                email=email or '',
                mobile_payment=mobile_payment,
                phone=phone,
                method=method
            )
            
            if result['success']:
                # Create a pending payment record
                payment = Payment.objects.create(
                    invoice=invoice,
                    payment_method='paynow' if not mobile_payment else method,
                    amount=invoice.total,
                    status='pending',
                    transaction_id=result.get('reference'),
                    metadata={
                        'poll_url': result.get('poll_url'),
                        'redirect_url': result.get('redirect_url'),
                        'paynow_data': result.get('data', {})
                    }
                )
                
                return Response({
                    "message": "Payment initiated successfully",
                    "payment_id": payment.id,
                    "redirect_url": result.get('redirect_url'),
                    "poll_url": result.get('poll_url'),
                    "instructions": result.get('instructions')
                }, status=status.HTTP_200_OK)
            else:
                return Response(
                    {"error": result.get('error', 'Payment initiation failed')},
                    status=status.HTTP_400_BAD_REQUEST
                )
                
        except ImportError:
            return Response(
                {"error": "Paynow SDK not installed. Install with: pip install paynow"},
                status=status.HTTP_503_SERVICE_UNAVAILABLE
            )
        except Exception as e:
            return Response(
                {"error": str(e)},
                status=status.HTTP_500_INTERNAL_SERVER_ERROR
            )
    
    @action(detail=True, methods=['post'])
    def express_checkout(self, request, pk=None):
        """
        Initiate Paynow Express Checkout for invoice (payment processed in-app, no redirect)
        
        Request body:
        {
            "method": "vmc",  // vmc, ecocash, onemoney, innbucks, zimswitch
            "email": "customer@example.com",
            "token": "CARD_TOKEN",  // Required for vmc (Visa/Mastercard)
            "phone": "263771234567",  // Required for ecocash/onemoney
            "account_number": "12345"  // Required for innbucks
        }
        """
        invoice = self.get_object()
        
        method = request.data.get('method', '').lower()
        email = request.data.get('email')
        token = request.data.get('token')
        phone = request.data.get('phone')
        account_number = request.data.get('account_number')
        
        if not method:
            return Response(
                {"error": "Payment method is required"},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        if not email:
            return Response(
                {"error": "Email is required"},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        try:
            from .paynow_service import PaynowService
            paynow = PaynowService()
            
            reference = f"INV-{invoice.invoice_number}"
            description = f"Payment for Invoice {invoice.invoice_number}"
            
            result = paynow.create_express_checkout(
                method=method,
                amount=invoice.total,
                reference=reference,
                email=email,
                description=description,
                token=token,
                phone=phone,
                account_number=account_number
            )
            
            if result['success']:
                # Create a payment record
                payment = Payment.objects.create(
                    invoice=invoice,
                    payment_method=method if method in ['ecocash', 'onemoney'] else 'paynow',
                    amount=invoice.total,
                    status='pending',
                    transaction_id=result.get('reference'),
                    metadata={
                        'poll_url': result.get('poll_url'),
                        'payment_method': result.get('payment_method'),
                        'paynow_data': result.get('data', {})
                    }
                )
                
                return Response({
                    "message": "Express checkout initiated successfully",
                    "payment_id": payment.id,
                    "poll_url": result.get('poll_url'),
                    "instructions": result.get('instructions'),
                    "payment_method": result.get('payment_method')
                }, status=status.HTTP_200_OK)
            else:
                return Response(
                    {"error": result.get('error', 'Express checkout failed')},
                    status=status.HTTP_400_BAD_REQUEST
                )
                
        except ImportError:
            return Response(
                {"error": "Paynow SDK not installed. Install with: pip install paynow"},
                status=status.HTTP_503_SERVICE_UNAVAILABLE
            )
        except Exception as e:
            return Response(
                {"error": str(e)},
                status=status.HTTP_500_INTERNAL_SERVER_ERROR
            )
    
    @action(detail=False, methods=['post'])
    def tokenize_card(self, request):
        """
        Tokenize a credit/debit card for express checkout
        
        Request body:
        {
            "card_number": "4111111111111111",
            "cardholder_name": "John Doe",
            "expiry_month": "12",
            "expiry_year": "2025",
            "cvv": "123"
        }
        """
        card_number = request.data.get('card_number', '').replace(' ', '')
        cardholder_name = request.data.get('cardholder_name', '')
        expiry_month = request.data.get('expiry_month', '')
        expiry_year = request.data.get('expiry_year', '')
        cvv = request.data.get('cvv', '')
        
        # Validate required fields
        if not all([card_number, cardholder_name, expiry_month, expiry_year, cvv]):
            return Response(
                {"error": "All card details are required"},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Validate card number (basic format check)
        if not card_number.isdigit() or len(card_number) < 13 or len(card_number) > 19:
            return Response(
                {"error": "Invalid card number format"},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Validate expiry month
        try:
            month = int(expiry_month)
            if month < 1 or month > 12:
                return Response(
                    {"error": "Invalid expiry month (must be 1-12)"},
                    status=status.HTTP_400_BAD_REQUEST
                )
        except ValueError:
            return Response(
                {"error": "Expiry month must be a number"},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Validate expiry year
        try:
            year = int(expiry_year)
            from datetime import datetime
            current_year = datetime.now().year
            if year < current_year or year > current_year + 20:
                return Response(
                    {"error": f"Invalid expiry year (must be {current_year}-{current_year+20})"},
                    status=status.HTTP_400_BAD_REQUEST
                )
        except ValueError:
            return Response(
                {"error": "Expiry year must be a number"},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Validate CVV
        if not cvv.isdigit() or len(cvv) < 3 or len(cvv) > 4:
            return Response(
                {"error": "Invalid CVV (must be 3 or 4 digits)"},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        try:
            from .paynow_service import PaynowService
            paynow = PaynowService()
            
            result = paynow.tokenize_card(
                card_number=card_number,
                cardholder_name=cardholder_name,
                expiry_month=expiry_month,
                expiry_year=expiry_year,
                cvv=cvv
            )
            
            if result['success']:
                return Response({
                    "message": "Card tokenized successfully",
                    "token": result.get('token'),
                    "card_last4": result.get('card_last4'),
                    "expiry": result.get('expiry')
                }, status=status.HTTP_200_OK)
            else:
                return Response(
                    {"error": result.get('error', 'Card tokenization failed')},
                    status=status.HTTP_400_BAD_REQUEST
                )
                
        except ImportError:
            return Response(
                {"error": "Paynow SDK not installed. Install with: pip install paynow"},
                status=status.HTTP_503_SERVICE_UNAVAILABLE
            )
        except Exception as e:
            return Response(
                {"error": str(e)},
                status=status.HTTP_500_INTERNAL_SERVER_ERROR
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
    
    @action(detail=True, methods=['post'])
    def check_paynow_status(self, request, pk=None):
        """
        Check Paynow payment status
        """
        payment = self.get_object()
        
        if payment.payment_method not in ['paynow', 'ecocash', 'onemoney']:
            return Response(
                {"error": "This is not a Paynow payment"},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        poll_url = payment.metadata.get('poll_url')
        if not poll_url:
            return Response(
                {"error": "No poll URL found for this payment"},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        try:
            from .paynow_service import PaynowService
            paynow = PaynowService()
            
            result = paynow.check_payment_status(poll_url)
            
            if result['success']:
                # Update payment status
                if result.get('paid'):
                    payment.status = 'completed'
                    payment.save()
                
                return Response({
                    "payment_id": payment.id,
                    "paid": result.get('paid'),
                    "status": result.get('status'),
                    "amount": result.get('amount'),
                    "reference": result.get('reference')
                }, status=status.HTTP_200_OK)
            else:
                return Response(
                    {"error": result.get('error', 'Status check failed')},
                    status=status.HTTP_400_BAD_REQUEST
                )
                
        except ImportError:
            return Response(
                {"error": "Paynow SDK not installed"},
                status=status.HTTP_503_SERVICE_UNAVAILABLE
            )
        except Exception as e:
            return Response(
                {"error": str(e)},
                status=status.HTTP_500_INTERNAL_SERVER_ERROR
            )


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


