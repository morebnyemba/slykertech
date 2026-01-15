'use client';

import { useEffect } from 'react';
import Link from 'next/link';
import { FaTrash, FaShoppingCart } from 'react-icons/fa';
import { useCartStore } from '@/lib/stores/cart-store';
import { useAuthStore } from '@/lib/stores/auth-store';

export default function CartPage() {
  const { cart, fetchCart, removeItem, clearCart, getTotal, getItemCount } = useCartStore();
  const { token } = useAuthStore();

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

  const total = getTotal();
  const itemCount = getItemCount();

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
                <div className="flex justify-between text-gray-600 dark:text-gray-400">
                  <span>Tax</span>
                  <span>$0.00</span>
                </div>
                <div className="border-t border-gray-200 dark:border-gray-700 pt-4">
                  <div className="flex justify-between text-xl font-bold text-gray-900 dark:text-white">
                    <span>Total</span>
                    <span>${total.toFixed(2)}</span>
                  </div>
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
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
