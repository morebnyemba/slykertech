'use client';

import { useEffect, useState } from 'react';
import Link from 'next/link';
import { FaTrash, FaShoppingCart, FaTag, FaSpinner, FaTimes, FaGift } from 'react-icons/fa';
import { useCartStore } from '@/lib/stores/cart-store';
import { useAuthStore } from '@/lib/stores/auth-store';

export default function CartPage() {
  const { 
    cart, 
    fetchCart, 
    removeItem, 
    clearCart, 
    getTotal, 
    getItemCount, 
    getDiscountedTotal,
    applyCoupon,
    removeCoupon,
    couponLoading,
    couponError
  } = useCartStore();
  const { token } = useAuthStore();
  const [couponCode, setCouponCode] = useState('');
  const [showCouponInput, setShowCouponInput] = useState(false);

  useEffect(() => {
    fetchCart(token || undefined);
  }, [fetchCart, token]);

  const handleRemoveItem = async (itemId: number) => {
    const result = await removeItem(itemId, token || undefined);
    if (!result.success) {
      alert(`Failed to remove item: ${result.error}`);
    }
  };

  const handleClearCart = async () => {
    if (confirm('Are you sure you want to clear your cart?')) {
      const result = await clearCart(token || undefined);
      if (!result.success) {
        alert(`Failed to clear cart: ${result.error}`);
      }
    }
  };

  const handleApplyCoupon = async () => {
    if (!couponCode.trim()) return;
    
    const result = await applyCoupon(couponCode.trim().toUpperCase(), token || undefined);
    if (result.success) {
      setCouponCode('');
      setShowCouponInput(false);
    }
  };

  const handleRemoveCoupon = async () => {
    await removeCoupon(token || undefined);
  };

  const total = getTotal();
  const discountedTotal = getDiscountedTotal();
  const itemCount = getItemCount();
  const hasDiscount = cart?.discount_amount && cart.discount_amount > 0;

  if (!cart || itemCount === 0) {
    return (
      <div className="min-h-screen py-12">
        <div className="max-w-4xl mx-auto px-4 text-center">
          <FaShoppingCart className="w-24 h-24 text-gray-400 mx-auto mb-6" />
          <h1 className="text-3xl font-bold text-gray-900 dark:text-white mb-4">
            Your Cart is Empty
          </h1>
          <p className="text-gray-600 dark:text-gray-400 mb-8">
            Add some services to your cart to get started
          </p>
          <Link
            href="/services"
            className="inline-block bg-blue-600 text-white px-8 py-3 rounded-lg font-medium hover:bg-blue-700 transition-colors"
          >
            Browse Services
          </Link>
        </div>
      </div>
    );
  }

  return (
    <div className="min-h-screen py-12">
      <div className="max-w-6xl mx-auto px-4">
        <div className="flex justify-between items-center mb-8">
          <h1 className="text-3xl font-bold text-gray-900 dark:text-white">
            Shopping Cart ({itemCount} {itemCount === 1 ? 'item' : 'items'})
          </h1>
          <button
            onClick={handleClearCart}
            className="text-red-600 hover:text-red-700 font-medium"
          >
            Clear Cart
          </button>
        </div>

        <div className="grid lg:grid-cols-3 gap-8">
          {/* Cart Items */}
          <div className="lg:col-span-2 space-y-4">
            {cart.items.map((item) => (
              <div
                key={item.id}
                className="bg-white dark:bg-gray-800 rounded-lg shadow-md p-6"
              >
                <div className="flex justify-between items-start mb-4">
                  <div className="flex-1">
                    <h3 className="text-xl font-bold text-gray-900 dark:text-white mb-2">
                      {item.service_name}
                    </h3>
                    <div className="space-y-1 text-sm text-gray-600 dark:text-gray-400">
                      <p>Category: <span className="font-medium">{item.service_category}</span></p>
                      <p>Billing: <span className="font-medium">{item.billing_cycle}</span></p>
                      
                      {/* Service-specific metadata */}
                      {item.service_category === 'hosting' && (
                        <>
                          <p>Type: <span className="font-medium">{item.service_metadata.type}</span></p>
                          <p>Region: <span className="font-medium">{item.service_metadata.region}</span></p>
                          {item.service_metadata.os && (
                            <p>OS: <span className="font-medium">{item.service_metadata.os}</span></p>
                          )}
                          {item.service_metadata.ram && (
                            <p>RAM: <span className="font-medium">{item.service_metadata.ram}</span></p>
                          )}
                          {item.service_metadata.cpu && (
                            <p>CPU: <span className="font-medium">{item.service_metadata.cpu}</span></p>
                          )}
                        </>
                      )}
                      
                      {item.service_category === 'domain' && (
                        <>
                          <p>Action: <span className="font-medium">{item.service_metadata.action}</span></p>
                          <p>Domain: <span className="font-medium">{item.service_metadata.domain_name}</span></p>
                          {item.service_metadata.epp_code && (
                            <p>EPP Code: <span className="font-medium">***{item.service_metadata.epp_code.slice(-4)}</span></p>
                          )}
                        </>
                      )}
                      
                      {item.service_category === 'development' && (
                        <>
                          <p>Type: <span className="font-medium">{item.service_metadata.type}</span></p>
                          <p>Project: <span className="font-medium">{item.service_metadata.project_name}</span></p>
                          <p>Timeline: <span className="font-medium">{item.service_metadata.timeline}</span></p>
                        </>
                      )}
                    </div>
                  </div>
                  
                  <button
                    onClick={() => handleRemoveItem(item.id)}
                    className="text-red-600 hover:text-red-700 p-2"
                    aria-label="Remove item"
                  >
                    <FaTrash />
                  </button>
                </div>

                <div className="flex justify-between items-center border-t border-gray-200 dark:border-gray-700 pt-4">
                  <span className="text-gray-600 dark:text-gray-400">
                    Quantity: {item.quantity}
                  </span>
                  <span className="text-2xl font-bold text-blue-600">
                    ${item.total_price.toFixed(2)}
                  </span>
                </div>
              </div>
            ))}
          </div>

          {/* Order Summary */}
          <div className="lg:col-span-1">
            <div className="bg-white dark:bg-gray-800 rounded-lg shadow-md p-6 sticky top-24">
              <h2 className="text-xl font-bold text-gray-900 dark:text-white mb-6">
                Order Summary
              </h2>

              <div className="space-y-4 mb-6">
                <div className="flex justify-between text-gray-600 dark:text-gray-400">
                  <span>Subtotal</span>
                  <span>${total.toFixed(2)}</span>
                </div>

                {/* Coupon Section */}
                {hasDiscount ? (
                  <div className="bg-green-50 dark:bg-green-900/20 rounded-lg p-3">
                    <div className="flex items-center justify-between">
                      <div className="flex items-center gap-2 text-green-700 dark:text-green-400">
                        <FaTag />
                        <span className="font-medium">{cart?.applied_coupon}</span>
                      </div>
                      <button
                        onClick={handleRemoveCoupon}
                        disabled={couponLoading}
                        className="text-gray-400 hover:text-red-500 transition-colors"
                        title="Remove coupon"
                      >
                        {couponLoading ? <FaSpinner className="animate-spin" /> : <FaTimes />}
                      </button>
                    </div>
                    <p className="text-sm text-green-600 dark:text-green-400 mt-1">
                      {cart?.discount_description || `You save $${cart?.discount_amount?.toFixed(2)}`}
                    </p>
                  </div>
                ) : showCouponInput ? (
                  <div className="space-y-2">
                    <div className="flex gap-2">
                      <input
                        type="text"
                        value={couponCode}
                        onChange={(e) => setCouponCode(e.target.value.toUpperCase())}
                        placeholder="Enter coupon code"
                        className="flex-1 px-3 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-white text-sm font-mono"
                        onKeyDown={(e) => e.key === 'Enter' && handleApplyCoupon()}
                      />
                      <button
                        onClick={handleApplyCoupon}
                        disabled={couponLoading || !couponCode.trim()}
                        className="px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 disabled:opacity-50 transition-colors text-sm"
                      >
                        {couponLoading ? <FaSpinner className="animate-spin" /> : 'Apply'}
                      </button>
                    </div>
                    {couponError && (
                      <p className="text-sm text-red-500">{couponError}</p>
                    )}
                    <button
                      onClick={() => {
                        setShowCouponInput(false);
                        setCouponCode('');
                      }}
                      className="text-sm text-gray-500 hover:text-gray-700"
                    >
                      Cancel
                    </button>
                  </div>
                ) : (
                  <button
                    onClick={() => setShowCouponInput(true)}
                    className="flex items-center gap-2 text-blue-600 hover:text-blue-700 text-sm font-medium"
                  >
                    <FaGift /> Have a coupon code?
                  </button>
                )}

                {/* Discount Line */}
                {hasDiscount && (
                  <div className="flex justify-between text-green-600 dark:text-green-400">
                    <span>Discount</span>
                    <span>-${cart?.discount_amount?.toFixed(2)}</span>
                  </div>
                )}

                <div className="flex justify-between text-gray-600 dark:text-gray-400">
                  <span>Tax</span>
                  <span>$0.00</span>
                </div>
                <div className="border-t border-gray-200 dark:border-gray-700 pt-4">
                  <div className="flex justify-between text-xl font-bold text-gray-900 dark:text-white">
                    <span>Total</span>
                    <span>${discountedTotal.toFixed(2)}</span>
                  </div>
                  {hasDiscount && (
                    <p className="text-sm text-green-600 dark:text-green-400 text-right mt-1">
                      You save ${cart?.discount_amount?.toFixed(2)}!
                    </p>
                  )}
                </div>
              </div>

              <Link
                href="/checkout"
                className="block w-full bg-blue-600 text-white text-center py-3 rounded-lg font-bold hover:bg-blue-700 transition-colors mb-4"
              >
                Proceed to Checkout
              </Link>

              <Link
                href="/services"
                className="block w-full text-center text-blue-600 hover:text-blue-700 font-medium"
              >
                Continue Shopping
              </Link>

              {/* Promotions Link */}
              <Link
                href="/promotions"
                className="flex items-center justify-center gap-2 mt-4 text-sm text-gray-500 hover:text-blue-600"
              >
                <FaTag /> View all promotions
              </Link>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
