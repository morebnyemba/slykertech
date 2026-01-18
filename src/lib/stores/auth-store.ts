/**
 * Authentication Store using Zustand
 * Manages authentication state across the application
 */

import { create } from 'zustand';
import { persist, createJSONStorage } from 'zustand/middleware';

// API URL constant
const getApiUrl = () => process.env.NEXT_PUBLIC_API_URL || 'https://api.slykertech.co.zw/api';

interface User {
  id: number;
  email: string;
  first_name: string;
  last_name: string;
  mobile_number?: string;
  role?: string;
}

interface AuthState {
  user: User | null;
  token: string | null;
  isAuthenticated: boolean;
  isLoading: boolean;
  login: (email: string, password: string) => Promise<{ success: boolean; error?: string }>;
  register: (userData: {
    email: string;
    password: string;
    first_name: string;
    last_name: string;
    mobile_number?: string;
    company_name?: string;
    user_type?: string;
  }) => Promise<{ success: boolean; error?: string }>;
  logout: () => void;
  setUser: (user: User | null) => void;
  setToken: (token: string | null) => void;
}

export const useAuthStore = create<AuthState>()(
  persist(
    (set) => ({
      user: null,
      token: null,
      isAuthenticated: false,
      isLoading: false,

      login: async (email: string, password: string) => {
        set({ isLoading: true });
        try {
          const apiUrl = getApiUrl();
          const response = await fetch(`${apiUrl}/token/`, {
            method: 'POST',
            credentials: 'include',
            headers: {
              'Content-Type': 'application/json',
            },
            body: JSON.stringify({ email, password }),
          });

          const data = await response.json();

          if (!response.ok) {
            set({ isLoading: false });
            return { success: false, error: data.detail || 'Login failed' };
          }

          // Get user profile
          const profileResponse = await fetch(`${apiUrl}/accounts/profile/`, {
            credentials: 'include',
            headers: {
              'Authorization': `Bearer ${data.access}`,
            },
          });

          if (profileResponse.ok) {
            const userData = await profileResponse.json();
            set({
              user: userData,
              token: data.access,
              isAuthenticated: true,
              isLoading: false,
            });
            return { success: true };
          }

          set({
            token: data.access,
            isAuthenticated: true,
            isLoading: false,
          });
          return { success: true };
        } catch {
          set({ isLoading: false });
          return { success: false, error: 'Network error. Please try again.' };
        }
      },

      register: async (userData) => {
        set({ isLoading: true });
        try {
          const apiUrl = getApiUrl();
          const response = await fetch(`${apiUrl}/accounts/register/`, {
            method: 'POST',
            credentials: 'include',
            headers: {
              'Content-Type': 'application/json',
            },
            body: JSON.stringify(userData),
          });

          const data = await response.json();

          if (!response.ok) {
            set({ isLoading: false });
            return { success: false, error: data.detail || data.error || 'Registration failed' };
          }

          // Auto-login after registration if token is provided
          if (data.access) {
            set({
              user: data.user,
              token: data.access,
              isAuthenticated: true,
              isLoading: false,
            });
          } else {
            set({ isLoading: false });
          }

          return { success: true };
        } catch {
          set({ isLoading: false });
          return { success: false, error: 'Network error. Please try again.' };
        }
      },

      logout: () => {
        set({
          user: null,
          token: null,
          isAuthenticated: false,
        });
      },

      setUser: (user) => {
        set({ user });
      },

      setToken: (token) => {
        set({ token, isAuthenticated: !!token });
      },
    }),
    {
      name: 'auth-storage',
      storage: createJSONStorage(() => localStorage),
    }
  )
);
