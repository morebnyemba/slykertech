from rest_framework import viewsets, permissions, status
from rest_framework.decorators import action
from rest_framework.response import Response
from .models import Invoice, InvoiceItem, Payment, BillingProfile, Cart, CartItem
from .serializers import (
    InvoiceSerializer, InvoiceCreateSerializer,
    PaymentSerializer, BillingProfileSerializer,
    CartSerializer, CartItemSerializer
)


class CartViewSet(viewsets.ModelViewSet):
    """ViewSet for shopping cart"""
    
    queryset = Cart.objects.all()
    serializer_class = CartSerializer
    permission_classes = [permissions.AllowAny]  # Allow non-authenticated users
    
    def get_queryset(self):
        """Filter carts based on user or session"""
        user = self.request.user
        session_id = self.request.session.session_key
        
        if user.is_authenticated:
            # Get or create user's active cart
            if hasattr(user, 'client'):
                return Cart.objects.filter(client=user.client, status='active')
            return Cart.objects.none()
        elif session_id:
            # Get session cart
            return Cart.objects.filter(session_id=session_id, status='active')
        return Cart.objects.none()
    
    @action(detail=False, methods=['get'])
    def current(self, request):
        """Get current user's or session's active cart"""
        user = request.user
        session_id = request.session.session_key
        
        cart = None
        if user.is_authenticated and hasattr(user, 'client'):
            cart, _ = Cart.objects.get_or_create(
                client=user.client,
                status='active',
                defaults={'session_id': session_id}
            )
        elif session_id:
            cart, _ = Cart.objects.get_or_create(
                session_id=session_id,
                status='active'
            )
        else:
            # Create session if doesn't exist
            if not request.session.session_key:
                request.session.create()
            session_id = request.session.session_key
            cart, _ = Cart.objects.get_or_create(
                session_id=session_id,
                status='active'
            )
        
        serializer = self.get_serializer(cart)
        return Response(serializer.data)
    
    @action(detail=True, methods=['post'])
    def add_item(self, request, pk=None):
        """Add item to cart"""
        cart = self.get_object()
        serializer = CartItemSerializer(data=request.data, context={'request': request})
        
        if serializer.is_valid():
            serializer.save(cart=cart)
            # Return updated cart
            cart_serializer = self.get_serializer(cart)
            return Response(cart_serializer.data, status=status.HTTP_201_CREATED)
        
        # Format errors for better frontend consumption
        error_messages = []
        for field, messages in serializer.errors.items():
            if isinstance(messages, list):
                for msg in messages:
                    error_messages.append(f"{field}: {msg}" if field != 'non_field_errors' else str(msg))
            else:
                error_messages.append(f"{field}: {messages}" if field != 'non_field_errors' else str(messages))
        
        return Response({
            'error': '. '.join(error_messages) if error_messages else 'Validation failed',
            'errors': serializer.errors
        }, status=status.HTTP_400_BAD_REQUEST)
    
    @action(detail=True, methods=['delete'])
    def remove_item(self, request, pk=None):
        """Remove item from cart"""
        cart = self.get_object()
        item_id = request.data.get('item_id')
        
        try:
            item = cart.items.get(id=item_id)
            item.delete()
            # Return updated cart
            cart_serializer = self.get_serializer(cart)
            return Response(cart_serializer.data)
        except CartItem.DoesNotExist:
            return Response(
                {"error": "Item not found in cart"},
                status=status.HTTP_404_NOT_FOUND
            )
    
    @action(detail=True, methods=['post'])
    def clear(self, request, pk=None):
        """Clear all items from cart"""
        cart = self.get_object()
        cart.items.all().delete()
        cart_serializer = self.get_serializer(cart)
        return Response(cart_serializer.data)
    
    @action(detail=True, methods=['post'])
    def checkout(self, request, pk=None):
        """
        Convert cart to invoice and create payment
        Requires authenticated user
        
        Request body:
        {
            "billing_info": {
                "full_name": "John Doe",
                "email": "john@example.com",
                "phone": "+263771234567",
                "address": "123 Main St",
                "city": "Harare",
                "country": "Zimbabwe"
            },
            "payment_method": "paynow"  // paynow, bank, crypto
        }
        """
        cart = self.get_object()
        user = request.user
        
        # Require authentication for checkout
        if not user.is_authenticated:
            return Response(
                {"error": "Authentication required for checkout"},
                status=status.HTTP_401_UNAUTHORIZED
            )
        
        # Check if user has a client profile
        if not hasattr(user, 'client'):
            return Response(
                {"error": "Client profile required. Please complete your profile."},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        # Check if cart has items
        if cart.items.count() == 0:
            return Response(
                {"error": "Cart is empty"},
                status=status.HTTP_400_BAD_REQUEST
            )
        
        billing_info = request.data.get('billing_info', {})
        payment_method = request.data.get('payment_method', 'paynow')
        
        from django.utils import timezone
        from datetime import timedelta
        from .models import Invoice, InvoiceItem
        
        # Create invoice
        invoice = Invoice.objects.create(
            client=user.client,
            issue_date=timezone.now().date(),
            due_date=timezone.now().date() + timedelta(days=7),
            notes=f"Order from cart #{cart.id}",
            subtotal=cart.get_total(),
        )
        
        # Create invoice items from cart items
        for cart_item in cart.items.all():
            # Build description with metadata
            description = cart_item.service.name
            if cart_item.service_metadata:
                if 'hosting_product_name' in cart_item.service_metadata:
                    description = cart_item.service_metadata['hosting_product_name']
                if 'domain_name' in cart_item.service_metadata:
                    description = f"Domain: {cart_item.service_metadata['domain_name']}"
                if 'region' in cart_item.service_metadata:
                    description += f" ({cart_item.service_metadata['region']})"
                if 'billing_cycle' in cart_item.service_metadata or cart_item.billing_cycle:
                    description += f" - {cart_item.billing_cycle}"
            
            InvoiceItem.objects.create(
                invoice=invoice,
                description=description,
                quantity=cart_item.quantity,
                unit_price=cart_item.unit_price,
            )
        
        # Update cart status
        cart.status = 'checkout'
        cart.save()
        
        # If payment method is paynow, initiate payment
        payment_data = {
            "invoice_id": invoice.id,
            "invoice_number": invoice.invoice_number,
            "total": float(invoice.total),
            "status": "pending",
            "message": "Invoice created successfully"
        }
        
        if payment_method == 'paynow' and billing_info.get('email'):
            try:
                from .paynow_service import PaynowService
                paynow = PaynowService()
                
                result = paynow.process_invoice_payment(
                    invoice=invoice,
                    email=billing_info.get('email', ''),
                    mobile_payment=False,
                )
                
                if result['success']:
                    # Create a pending payment record
                    payment = Payment.objects.create(
                        invoice=invoice,
                        payment_method='paynow',
                        amount=invoice.total,
                        status='pending',
                        transaction_id=result.get('reference'),
                        metadata={
                            'poll_url': result.get('poll_url'),
                            'redirect_url': result.get('redirect_url'),
                            'billing_info': billing_info,
                        }
                    )
                    
                    payment_data.update({
                        "payment_id": payment.id,
                        "redirect_url": result.get('redirect_url'),
                        "poll_url": result.get('poll_url'),
                        "payment_initiated": True,
                    })
            except ImportError:
                # Paynow SDK not installed, just create invoice
                payment_data["payment_initiated"] = False
                payment_data["message"] = "Invoice created. Paynow payment can be initiated later."
            except Exception as e:
                payment_data["payment_initiated"] = False
                payment_data["payment_error"] = str(e)
        
        # Mark cart as completed
        cart.status = 'completed'
        cart.save()
        
        return Response(payment_data, status=status.HTTP_201_CREATED)


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
    
    @action(detail=False, methods=['get'])
    def stats(self, request):
        """Get invoice statistics for admin dashboard"""
        user = request.user
        
        if not (user.is_staff or user.is_superuser or getattr(user, 'user_type', '') == 'admin'):
            return Response({'error': 'Not authorized'}, status=status.HTTP_403_FORBIDDEN)
        
        from django.db.models import Sum, Count
        from django.utils import timezone
        from datetime import timedelta
        
        now = timezone.now()
        today = now.date()
        this_month_start = today.replace(day=1)
        last_month_start = (this_month_start - timedelta(days=1)).replace(day=1)
        
        # Basic counts
        total = Invoice.objects.count()
        draft = Invoice.objects.filter(status='draft').count()
        sent = Invoice.objects.filter(status='sent').count()
        paid = Invoice.objects.filter(status='paid').count()
        overdue = Invoice.objects.filter(status='overdue').count()
        cancelled = Invoice.objects.filter(status='cancelled').count()
        
        # Revenue calculations
        total_revenue = Invoice.objects.filter(status='paid').aggregate(Sum('total'))['total__sum'] or 0
        this_month_revenue = Invoice.objects.filter(
            status='paid', 
            paid_date__gte=this_month_start
        ).aggregate(Sum('total'))['total__sum'] or 0
        last_month_revenue = Invoice.objects.filter(
            status='paid', 
            paid_date__gte=last_month_start,
            paid_date__lt=this_month_start
        ).aggregate(Sum('total'))['total__sum'] or 0
        
        # Outstanding amounts
        outstanding = Invoice.objects.filter(status__in=['sent', 'overdue']).aggregate(Sum('total'))['total__sum'] or 0
        
        # Recent invoices count
        recent_count = Invoice.objects.filter(created_at__gte=now - timedelta(days=7)).count()
        
        stats = {
            'total': total,
            'by_status': {
                'draft': draft,
                'sent': sent,
                'paid': paid,
                'overdue': overdue,
                'cancelled': cancelled,
            },
            'revenue': {
                'total': float(total_revenue),
                'this_month': float(this_month_revenue),
                'last_month': float(last_month_revenue),
                'growth_percent': ((this_month_revenue - last_month_revenue) / last_month_revenue * 100) if last_month_revenue > 0 else 0,
            },
            'outstanding': float(outstanding),
            'recent_count': recent_count,
        }
        
        return Response(stats)
    
    @action(detail=True, methods=['post'])
    def cancel(self, request, pk=None):
        """Cancel an invoice"""
        invoice = self.get_object()
        invoice.status = 'cancelled'
        invoice.save()
        
        return Response({"message": "Invoice cancelled."}, status=status.HTTP_200_OK)
    
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
    
    @action(detail=False, methods=['get'])
    def stats(self, request):
        """Get payment statistics for admin dashboard"""
        user = request.user
        
        if not (user.is_staff or user.is_superuser or getattr(user, 'user_type', '') == 'admin'):
            return Response({'error': 'Not authorized'}, status=status.HTTP_403_FORBIDDEN)
        
        from django.db.models import Sum, Count
        from django.utils import timezone
        from datetime import timedelta
        
        now = timezone.now()
        today = now.date()
        this_month_start = today.replace(day=1)
        last_30_days = now - timedelta(days=30)
        
        # Basic counts
        total = Payment.objects.count()
        completed = Payment.objects.filter(status='completed').count()
        pending = Payment.objects.filter(status='pending').count()
        failed = Payment.objects.filter(status='failed').count()
        refunded = Payment.objects.filter(status='refunded').count()
        
        # Revenue by payment method
        payment_methods = Payment.objects.filter(status='completed').values('payment_method').annotate(
            count=Count('id'),
            total=Sum('amount')
        )
        
        by_method = {}
        for pm in payment_methods:
            by_method[pm['payment_method']] = {
                'count': pm['count'],
                'total': float(pm['total'] or 0)
            }
        
        # Total completed amounts
        total_completed = Payment.objects.filter(status='completed').aggregate(Sum('amount'))['amount__sum'] or 0
        recent_completed = Payment.objects.filter(
            status='completed',
            payment_date__gte=last_30_days
        ).aggregate(Sum('amount'))['amount__sum'] or 0
        
        stats = {
            'total': total,
            'by_status': {
                'completed': completed,
                'pending': pending,
                'failed': failed,
                'refunded': refunded,
            },
            'by_method': by_method,
            'total_completed': float(total_completed),
            'last_30_days': float(recent_completed),
        }
        
        return Response(stats)
    
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


