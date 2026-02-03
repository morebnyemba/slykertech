/**
 * Cart Store using Zustand
 * Manages shopping cart state across the application
 */

import { create } from 'zustand';
import { persist, createJSONStorage } from 'zustand/middleware';

const getApiUrl = () => process.env.NEXT_PUBLIC_API_URL || 'https://api.slykertech.co.zw/api';

interface CartItem {
  id: number;
  service: number;
  service_name: string;
  service_category: string;
  service_metadata: Record<string, unknown>;
  quantity: number;
  unit_price: number;
  billing_cycle: string;
  total_price: number;
  created_at: string;
}

interface Cart {
  id: number;
  client?: number;
  session_id?: string;
  status: string;
  items: CartItem[];
  total: number;
  item_count: number;
  created_at: string;
  updated_at: string;
  // Promotion fields
  applied_coupon?: string;
  discount_amount?: number;
  discount_description?: string;
  subtotal?: number;
  final_total?: number;
}

interface CartState {
  cart: Cart | null;
  isLoading: boolean;
  error: string | null;
  couponLoading: boolean;
  couponError: string | null;
  fetchCart: (token?: string) => Promise<void>;
  addItem: (item: Partial<CartItem>, token?: string) => Promise<{ success: boolean; error?: string }>;
  removeItem: (itemId: number, token?: string) => Promise<{ success: boolean; error?: string }>;
  clearCart: (token?: string) => Promise<{ success: boolean; error?: string }>;
  applyCoupon: (code: string, token?: string) => Promise<{ success: boolean; error?: string; discount?: number }>;
  removeCoupon: (token?: string) => Promise<{ success: boolean; error?: string }>;
  getItemCount: () => number;
  getTotal: () => number;
  getDiscountedTotal: () => number;
}

export const useCartStore = create<CartState>()(
  persist(
    (set, get) => ({
      cart: null,
      isLoading: false,
      error: null,
      couponLoading: false,
      couponError: null,

      fetchCart: async (token?: string) => {
        set({ isLoading: true, error: null });
        try {
          const apiUrl = getApiUrl();
          const headers: Record<string, string> = {
            'Content-Type': 'application/json',
          };
          
          if (token) {
            headers['Authorization'] = `Bearer ${token}`;
          }

          const response = await fetch(`${apiUrl}/billing/carts/current/`, {
            headers,
            credentials: 'include',
          });

          // Silently handle 401 for unauthenticated users
          if (response.status === 401) {
            set({ cart: null, isLoading: false, error: null });
            return;
          }

          if (!response.ok) {
            throw new Error('Failed to fetch cart');
          }

          const cart = await response.json();
          set({ cart, isLoading: false });
        } catch (error) {
          // Don't show errors for network issues - just set loading to false
          console.error('Cart fetch error:', error);
          set({ 
            error: null, // Don't expose errors to UI for cart fetching
            isLoading: false 
          });
        }
      },

      addItem: async (item: Partial<CartItem>, token?: string) => {
        set({ isLoading: true, error: null });
        try {
          const apiUrl = getApiUrl();
          let { cart } = get();
          
          if (!cart) {
            // Fetch cart first
            await get().fetchCart(token);
            cart = get().cart;
            if (!cart) {
              throw new Error('Failed to get cart');
            }
          }

          const headers: Record<string, string> = {
            'Content-Type': 'application/json',
          };
          
          if (token) {
            headers['Authorization'] = `Bearer ${token}`;
          }

          const response = await fetch(`${apiUrl}/billing/carts/${cart.id}/add_item/`, {
            method: 'POST',
            headers,
            credentials: 'include',
            body: JSON.stringify(item),
          });

          if (!response.ok) {
            const errorData = await response.json();
            // Handle Django REST Framework validation errors
            let errorMessage = 'Failed to add item to cart';
            if (errorData.error) {
              errorMessage = errorData.error;
            } else if (errorData.detail) {
              errorMessage = errorData.detail;
            } else if (typeof errorData === 'object') {
              // Parse field-level validation errors
              const errors = Object.entries(errorData)
                .map(([field, messages]) => {
                  const msgArray = Array.isArray(messages) ? messages : [messages];
                  return `${field}: ${msgArray.join(', ')}`;
                })
                .join('. ');
              if (errors) errorMessage = errors;
            }
            throw new Error(errorMessage);
          }

          const updatedCart = await response.json();
          set({ cart: updatedCart, isLoading: false });
          return { success: true };
        } catch (error) {
          const errorMessage = error instanceof Error ? error.message : 'Failed to add item';
          set({ error: errorMessage, isLoading: false });
          return { success: false, error: errorMessage };
        }
      },

      removeItem: async (itemId: number, token?: string) => {
        set({ isLoading: true, error: null });
        try {
          const apiUrl = getApiUrl();
          const { cart } = get();
          
          if (!cart) {
            throw new Error('No cart available');
          }

          const headers: Record<string, string> = {
            'Content-Type': 'application/json',
          };
          
          if (token) {
            headers['Authorization'] = `Bearer ${token}`;
          }

          const response = await fetch(`${apiUrl}/billing/carts/${cart.id}/remove_item/`, {
            method: 'DELETE',
            headers,
            credentials: 'include',
            body: JSON.stringify({ item_id: itemId }),
          });

          if (!response.ok) {
            throw new Error('Failed to remove item from cart');
          }

          const updatedCart = await response.json();
          set({ cart: updatedCart, isLoading: false });
          return { success: true };
        } catch (error) {
          const errorMessage = error instanceof Error ? error.message : 'Failed to remove item';
          set({ error: errorMessage, isLoading: false });
          return { success: false, error: errorMessage };
        }
      },

      clearCart: async (token?: string) => {
        set({ isLoading: true, error: null });
        try {
          const apiUrl = getApiUrl();
          const { cart } = get();
          
          if (!cart) {
            throw new Error('No cart available');
          }

          const headers: Record<string, string> = {
            'Content-Type': 'application/json',
          };
          
          if (token) {
            headers['Authorization'] = `Bearer ${token}`;
          }

          const response = await fetch(`${apiUrl}/billing/carts/${cart.id}/clear/`, {
            method: 'POST',
            headers,
            credentials: 'include',
          });

          if (!response.ok) {
            throw new Error('Failed to clear cart');
          }

          const updatedCart = await response.json();
          set({ cart: updatedCart, isLoading: false });
          return { success: true };
        } catch (error) {
          const errorMessage = error instanceof Error ? error.message : 'Failed to clear cart';
          set({ error: errorMessage, isLoading: false });
          return { success: false, error: errorMessage };
        }
      },

      applyCoupon: async (code: string, token?: string) => {
        set({ couponLoading: true, couponError: null });
        try {
          const apiUrl = getApiUrl();
          const { cart } = get();
          
          if (!cart) {
            throw new Error('No cart available');
          }

          const headers: Record<string, string> = {
            'Content-Type': 'application/json',
          };
          
          if (token) {
            headers['Authorization'] = `Bearer ${token}`;
          }

          const response = await fetch(`${apiUrl}/billing/carts/${cart.id}/apply_coupon/`, {
            method: 'POST',
            headers,
            credentials: 'include',
            body: JSON.stringify({ code }),
          });

          if (!response.ok) {
            const errorData = await response.json();
            throw new Error(errorData.error || errorData.detail || 'Invalid coupon code');
          }

          const updatedCart = await response.json();
          set({ cart: updatedCart, couponLoading: false });
          return { success: true, discount: updatedCart.discount_amount };
        } catch (error) {
          const errorMessage = error instanceof Error ? error.message : 'Failed to apply coupon';
          set({ couponError: errorMessage, couponLoading: false });
          return { success: false, error: errorMessage };
        }
      },

      removeCoupon: async (token?: string) => {
        set({ couponLoading: true, couponError: null });
        try {
          const apiUrl = getApiUrl();
          const { cart } = get();
          
          if (!cart) {
            throw new Error('No cart available');
          }

          const headers: Record<string, string> = {
            'Content-Type': 'application/json',
          };
          
          if (token) {
            headers['Authorization'] = `Bearer ${token}`;
          }

          const response = await fetch(`${apiUrl}/billing/carts/${cart.id}/remove_coupon/`, {
            method: 'POST',
            headers,
            credentials: 'include',
          });

          if (!response.ok) {
            throw new Error('Failed to remove coupon');
          }

          const updatedCart = await response.json();
          set({ cart: updatedCart, couponLoading: false });
          return { success: true };
        } catch (error) {
          const errorMessage = error instanceof Error ? error.message : 'Failed to remove coupon';
          set({ couponError: errorMessage, couponLoading: false });
          return { success: false, error: errorMessage };
        }
      },

      getItemCount: () => {
        const { cart } = get();
        return cart?.item_count || 0;
      },

      getTotal: () => {
        const { cart } = get();
        return cart?.total || 0;
      },

      getDiscountedTotal: () => {
        const { cart } = get();
        // Use final_total if available (from backend), otherwise calculate from discount
        return cart?.final_total ?? ((cart?.total || 0) - (cart?.discount_amount || 0));
      },
    }),
    {
      name: 'cart-storage',
      storage: createJSONStorage(() => localStorage),
      partialize: (state) => ({ cart: state.cart }), // Only persist cart data
    }
  )
);
