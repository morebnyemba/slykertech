'use client';

import { useEffect, useState } from 'react';
import { useRouter } from 'next/navigation';
import Link from 'next/link';
import { FaCheckCircle, FaLock, FaUserPlus, FaSignInAlt } from 'react-icons/fa';
import { useCartStore } from '@/lib/stores/cart-store';
import { useAuthStore } from '@/lib/stores/auth-store';

export default function CheckoutPage() {
  const router = useRouter();
  const { cart, fetchCart, getTotal, getItemCount } = useCartStore();
  const { isAuthenticated, user, token } = useAuthStore();

  const [billingInfo, setBillingInfo] = useState({
    fullName: '',
    email: '',
    phone: '',
    address: '',
    city: '',
    country: '',
  });

  const [paymentMethod, setPaymentMethod] = useState('paynow');
  const [isProcessing, setIsProcessing] = useState(false);

  useEffect(() => {
    fetchCart(token || undefined);
  }, [fetchCart, token]);

  useEffect(() => {
    // Pre-fill form if user is authenticated
    if (isAuthenticated && user) {
      setBillingInfo({
        fullName: `${user.first_name} ${user.last_name}`.trim(),
        email: user.email,
        phone: user.mobile_number || '',
        address: '',
        city: '',
        country: '',
      });
    }
  }, [isAuthenticated, user]);

  const total = getTotal();
  const itemCount = getItemCount();

  useEffect(() => {
    // Redirect to cart if empty
    if (cart && itemCount === 0) {
      router.push('/cart');
    }
  }, [cart, itemCount, router]);

  // If user is not authenticated, show login/signup prompt
  if (!isAuthenticated) {
    return (
      <div className="min-h-screen py-12">
        <div className="max-w-2xl mx-auto px-4">
          <div className="bg-white dark:bg-gray-800 rounded-xl shadow-lg p-8 text-center">
            <div className="w-16 h-16 bg-blue-100 dark:bg-blue-900 rounded-full flex items-center justify-center mx-auto mb-6">
              <FaLock className="text-blue-600 text-2xl" />
            </div>
            <h1 className="text-2xl font-bold text-gray-900 dark:text-white mb-4">
              Sign In Required
            </h1>
            <p className="text-gray-600 dark:text-gray-300 mb-8">
              Please sign in or create an account to proceed with checkout. Your cart items will be saved.
            </p>
            
            <div className="flex flex-col sm:flex-row gap-4 justify-center">
              <Link
                href="/login?redirect=/checkout"
                className="inline-flex items-center justify-center gap-2 px-6 py-3 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors font-medium"
              >
                <FaSignInAlt />
                Sign In
              </Link>
              <Link
                href="/signup?redirect=/checkout"
                className="inline-flex items-center justify-center gap-2 px-6 py-3 border border-blue-600 text-blue-600 rounded-lg hover:bg-blue-50 dark:hover:bg-blue-900/20 transition-colors font-medium"
              >
                <FaUserPlus />
                Create Account
              </Link>
            </div>

            <div className="mt-8 pt-6 border-t border-gray-200 dark:border-gray-700">
              <Link
                href="/cart"
                className="text-gray-600 dark:text-gray-400 hover:text-gray-900 dark:hover:text-white text-sm"
              >
                ← Back to Cart
              </Link>
            </div>
          </div>
        </div>
      </div>
    );
  }

  const handleInputChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    setBillingInfo({
      ...billingInfo,
      [e.target.name]: e.target.value,
    });
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    
    // Validation
    if (!billingInfo.fullName || !billingInfo.email || !billingInfo.phone) {
      alert('Please fill in all required fields');
      return;
    }

    if (!cart) {
      alert('No cart found');
      return;
    }

    setIsProcessing(true);

    try {
      const apiUrl = process.env.NEXT_PUBLIC_API_URL || 'https://api.slykertech.co.zw/api';
      
      const response = await fetch(`${apiUrl}/billing/carts/${cart.id}/checkout/`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          ...(token ? { 'Authorization': `Bearer ${token}` } : {}),
        },
        credentials: 'include',
        body: JSON.stringify({
          billing_info: {
            full_name: billingInfo.fullName,
            email: billingInfo.email,
            phone: billingInfo.phone,
            address: billingInfo.address,
            city: billingInfo.city,
            country: billingInfo.country,
          },
          payment_method: paymentMethod,
        }),
      });

      const data = await response.json();

      if (!response.ok) {
        throw new Error(data.error || 'Checkout failed');
      }

      // If payment was initiated with redirect URL (Paynow), redirect user
      if (data.redirect_url) {
        window.location.href = data.redirect_url;
        return;
      }

      // Otherwise, show success and redirect to dashboard
      alert(`Order placed successfully! Invoice #${data.invoice_number} has been created. You will receive a confirmation email shortly.`);
      
      // Refresh cart to get new empty cart
      fetchCart(token || undefined);
      
      router.push('/dashboard/invoices');
    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : 'Checkout failed';
      alert(`Error: ${errorMessage}`);
    } finally {
      setIsProcessing(false);
    }
  };

  if (!cart || itemCount === 0) {
    return (
      <div className="min-h-screen py-12 flex items-center justify-center">
        <div className="text-center">
          <p className="text-gray-600 dark:text-gray-400">Loading...</p>
        </div>
      </div>
    );
  }

  return (
    <div className="min-h-screen py-12">
      <div className="max-w-6xl mx-auto px-4">
        <h1 className="text-3xl font-bold text-gray-900 dark:text-white mb-8">
          Checkout
        </h1>

        <form onSubmit={handleSubmit}>
          <div className="grid lg:grid-cols-3 gap-8">
            {/* Billing Information */}
            <div className="lg:col-span-2 space-y-6">
              {/* Contact Information */}
              <div className="bg-white dark:bg-gray-800 rounded-lg shadow-md p-6">
                <h2 className="text-xl font-bold text-gray-900 dark:text-white mb-6">
                  Contact Information
                </h2>
                
                <div className="grid md:grid-cols-2 gap-4">
                  <div>
                    <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                      Full Name *
                    </label>
                    <input
                      type="text"
                      name="fullName"
                      value={billingInfo.fullName}
                      onChange={handleInputChange}
                      required
                      className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-900 text-gray-900 dark:text-white"
                    />
                  </div>
                  
                  <div>
                    <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                      Email *
                    </label>
                    <input
                      type="email"
                      name="email"
                      value={billingInfo.email}
                      onChange={handleInputChange}
                      required
                      className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-900 text-gray-900 dark:text-white"
                    />
                  </div>
                  
                  <div>
                    <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                      Phone *
                    </label>
                    <input
                      type="tel"
                      name="phone"
                      value={billingInfo.phone}
                      onChange={handleInputChange}
                      required
                      className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-900 text-gray-900 dark:text-white"
                    />
                  </div>
                  
                  <div>
                    <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                      Address
                    </label>
                    <input
                      type="text"
                      name="address"
                      value={billingInfo.address}
                      onChange={handleInputChange}
                      className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-900 text-gray-900 dark:text-white"
                    />
                  </div>
                  
                  <div>
                    <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                      City
                    </label>
                    <input
                      type="text"
                      name="city"
                      value={billingInfo.city}
                      onChange={handleInputChange}
                      className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-900 text-gray-900 dark:text-white"
                    />
                  </div>
                  
                  <div>
                    <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                      Country
                    </label>
                    <input
                      type="text"
                      name="country"
                      value={billingInfo.country}
                      onChange={handleInputChange}
                      className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-900 text-gray-900 dark:text-white"
                    />
                  </div>
                </div>
              </div>

              {/* Payment Method */}
              <div className="bg-white dark:bg-gray-800 rounded-lg shadow-md p-6">
                <h2 className="text-xl font-bold text-gray-900 dark:text-white mb-6">
                  Payment Method
                </h2>
                
                <div className="space-y-4">
                  <label className="flex items-center p-4 border border-gray-300 dark:border-gray-600 rounded-lg cursor-pointer hover:bg-gray-50 dark:hover:bg-gray-700">
                    <input
                      type="radio"
                      name="paymentMethod"
                      value="paynow"
                      checked={paymentMethod === 'paynow'}
                      onChange={(e) => setPaymentMethod(e.target.value)}
                      className="mr-4"
                    />
                    <div className="flex-1">
                      <div className="font-medium text-gray-900 dark:text-white">Paynow</div>
                      <div className="text-sm text-gray-600 dark:text-gray-400">
                        EcoCash, OneMoney, Visa/Mastercard
                      </div>
                    </div>
                  </label>

                  <label className="flex items-center p-4 border border-gray-300 dark:border-gray-600 rounded-lg cursor-pointer hover:bg-gray-50 dark:hover:bg-gray-700">
                    <input
                      type="radio"
                      name="paymentMethod"
                      value="bank"
                      checked={paymentMethod === 'bank'}
                      onChange={(e) => setPaymentMethod(e.target.value)}
                      className="mr-4"
                    />
                    <div className="flex-1">
                      <div className="font-medium text-gray-900 dark:text-white">Bank Transfer</div>
                      <div className="text-sm text-gray-600 dark:text-gray-400">
                        Direct bank transfer
                      </div>
                    </div>
                  </label>

                  <label className="flex items-center p-4 border border-gray-300 dark:border-gray-600 rounded-lg cursor-pointer hover:bg-gray-50 dark:hover:bg-gray-700">
                    <input
                      type="radio"
                      name="paymentMethod"
                      value="crypto"
                      checked={paymentMethod === 'crypto'}
                      onChange={(e) => setPaymentMethod(e.target.value)}
                      className="mr-4"
                    />
                    <div className="flex-1">
                      <div className="font-medium text-gray-900 dark:text-white">Cryptocurrency</div>
                      <div className="text-sm text-gray-600 dark:text-gray-400">
                        Bitcoin, Ethereum, USDT
                      </div>
                    </div>
                  </label>
                </div>
              </div>
            </div>

            {/* Order Summary */}
            <div className="lg:col-span-1">
              <div className="bg-white dark:bg-gray-800 rounded-lg shadow-md p-6 sticky top-24">
                <h2 className="text-xl font-bold text-gray-900 dark:text-white mb-6">
                  Order Summary
                </h2>

                <div className="space-y-4 mb-6">
                  {cart.items.map((item) => (
                    <div key={item.id} className="flex justify-between text-sm">
                      <span className="text-gray-600 dark:text-gray-400">
                        {item.service_name}
                      </span>
                      <span className="font-medium text-gray-900 dark:text-white">
                        ${item.total_price.toFixed(2)}
                      </span>
                    </div>
                  ))}
                </div>

                <div className="border-t border-gray-200 dark:border-gray-700 pt-4 space-y-3 mb-6">
                  <div className="flex justify-between text-gray-600 dark:text-gray-400">
                    <span>Subtotal</span>
                    <span>${total.toFixed(2)}</span>
                  </div>
                  <div className="flex justify-between text-gray-600 dark:text-gray-400">
                    <span>Tax</span>
                    <span>$0.00</span>
                  </div>
                  <div className="flex justify-between text-xl font-bold text-gray-900 dark:text-white">
                    <span>Total</span>
                    <span>${total.toFixed(2)}</span>
                  </div>
                </div>

                <button
                  type="submit"
                  disabled={isProcessing}
                  className="w-full bg-blue-600 text-white py-3 rounded-lg font-bold hover:bg-blue-700 transition-colors disabled:opacity-50 disabled:cursor-not-allowed flex items-center justify-center gap-2"
                >
                  {isProcessing ? (
                    'Processing...'
                  ) : (
                    <>
                      <FaLock />
                      Complete Order
                    </>
                  )}
                </button>

                <div className="mt-4 flex items-center justify-center gap-2 text-sm text-gray-600 dark:text-gray-400">
                  <FaCheckCircle className="text-green-500" />
                  <span>Secure checkout powered by SSL</span>
                </div>
              </div>
            </div>
          </div>
        </form>
      </div>
    </div>
  );
}
