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
  service_metadata: Record<string, any>;
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
}

interface CartState {
  cart: Cart | null;
  isLoading: boolean;
  error: string | null;
  fetchCart: (token?: string) => Promise<void>;
  addItem: (item: Partial<CartItem>, token?: string) => Promise<{ success: boolean; error?: string }>;
  removeItem: (itemId: number, token?: string) => Promise<{ success: boolean; error?: string }>;
  clearCart: (token?: string) => Promise<{ success: boolean; error?: string }>;
  getItemCount: () => number;
  getTotal: () => number;
}

export const useCartStore = create<CartState>()(
  persist(
    (set, get) => ({
      cart: null,
      isLoading: false,
      error: null,

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

          if (!response.ok) {
            throw new Error('Failed to fetch cart');
          }

          const cart = await response.json();
          set({ cart, isLoading: false });
        } catch (error) {
          set({ 
            error: error instanceof Error ? error.message : 'Failed to fetch cart',
            isLoading: false 
          });
        }
      },

      addItem: async (item: Partial<CartItem>, token?: string) => {
        set({ isLoading: true, error: null });
        try {
          const apiUrl = getApiUrl();
          const { cart } = get();
          
          if (!cart) {
            // Fetch cart first
            await get().fetchCart(token);
            const updatedCart = get().cart;
            if (!updatedCart) {
              throw new Error('Failed to get cart');
            }
          }

          const currentCart = get().cart;
          if (!currentCart) {
            throw new Error('No cart available');
          }

          const headers: Record<string, string> = {
            'Content-Type': 'application/json',
          };
          
          if (token) {
            headers['Authorization'] = `Bearer ${token}`;
          }

          const response = await fetch(`${apiUrl}/billing/carts/${currentCart.id}/add_item/`, {
            method: 'POST',
            headers,
            credentials: 'include',
            body: JSON.stringify(item),
          });

          if (!response.ok) {
            const errorData = await response.json();
            throw new Error(errorData.error || 'Failed to add item to cart');
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

      getItemCount: () => {
        const { cart } = get();
        return cart?.item_count || 0;
      },

      getTotal: () => {
        const { cart } = get();
        return cart?.total || 0;
      },
    }),
    {
      name: 'cart-storage',
      storage: createJSONStorage(() => localStorage),
      partialize: (state) => ({ cart: state.cart }), // Only persist cart data
    }
  )
);
