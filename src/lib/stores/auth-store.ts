/**
 * Authentication Store using Zustand
 * Manages authentication state across the application
 */

import { create } from 'zustand';
import { persist, createJSONStorage } from 'zustand/middleware';
import { apiService } from '@/lib/api-service';

// API URL constant
const getApiUrl = () => process.env.NEXT_PUBLIC_API_URL || 'https://api.slykertech.co.zw/api';

interface User {
  id: number;
  email: string;
  first_name: string;
  last_name: string;
  mobile_number?: string;
  role?: string;
  user_type?: string;
  is_staff?: boolean;
  is_superuser?: boolean;
}

interface AuthState {
  user: User | null;
  token: string | null;
  refreshToken: string | null;
  isAuthenticated: boolean;
  isLoading: boolean;
  isStaff: boolean;
  login: (email: string, password: string) => Promise<{ success: boolean; error?: string }>;
  register: (userData: {
    email: string;
    password: string;
    first_name: string;
    last_name: string;
    mobile_number?: string;
    company_name?: string;
    company_website?: string;
    address?: string;
    city?: string;
    country?: string;
    referral_code?: string;
  }) => Promise<{ success: boolean; error?: string }>;
  refreshAccessToken: () => Promise<boolean>;
  logout: () => void;
  setUser: (user: User | null) => void;
  setToken: (token: string | null) => void;
}

const checkIsStaff = (user: User | null): boolean => {
  if (!user) return false;
  return (
    user.is_staff === true ||
    user.is_superuser === true ||
    user.role === 'admin' ||
    user.role === 'staff' ||
    user.user_type === 'admin' ||
    user.user_type === 'staff'
  );
};

export const useAuthStore = create<AuthState>()(
  persist(
    (set, get) => ({
      user: null,
      token: null,
      refreshToken: null,
      isAuthenticated: false,
      isLoading: false,
      isStaff: false,

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
          const profileResponse = await fetch(`${apiUrl}/accounts/users/me/`, {
            credentials: 'include',
            headers: {
              'Authorization': `Bearer ${data.access}`,
            },
          });

          if (profileResponse.ok) {
            const userData = await profileResponse.json();
            // Sync token with apiService for authenticated API requests
            apiService.setToken(data.access);
            set({
              user: userData,
              token: data.access,
              refreshToken: data.refresh,
              isAuthenticated: true,
              isStaff: checkIsStaff(userData),
              isLoading: false,
            });
            return { success: true };
          }

          // Sync token with apiService for authenticated API requests
          apiService.setToken(data.access);
          set({
            token: data.access,
            refreshToken: data.refresh,
            isAuthenticated: true,
            isStaff: false,
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
          // Include password2 for Django validation
          const registrationData = {
            ...userData,
            password2: userData.password,
          };
          const response = await fetch(`${apiUrl}/accounts/register/`, {
            method: 'POST',
            credentials: 'include',
            headers: {
              'Content-Type': 'application/json',
            },
            body: JSON.stringify(registrationData),
          });

          const data = await response.json();

          if (!response.ok) {
            set({ isLoading: false });
            // Handle Django validation errors which come as field: [errors] format
            let errorMessage = 'Registration failed';
            if (data.detail) {
              errorMessage = data.detail;
            } else if (data.error) {
              errorMessage = data.error;
            } else if (typeof data === 'object') {
              // Parse field-level errors
              const errors = Object.entries(data)
                .map(([field, messages]) => {
                  if (Array.isArray(messages)) {
                    return `${field}: ${messages.join(', ')}`;
                  }
                  return `${field}: ${messages}`;
                })
                .join('. ');
              if (errors) errorMessage = errors;
            }
            return { success: false, error: errorMessage };
          }

          // Auto-login after registration if token is provided
          if (data.access) {
            // Sync token with apiService for authenticated API requests
            apiService.setToken(data.access);
            set({
              user: data.user,
              token: data.access,
              refreshToken: data.refresh,
              isAuthenticated: true,
              isStaff: checkIsStaff(data.user),
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

      refreshAccessToken: async () => {
        const state = get();
        if (!state.refreshToken) {
          return false;
        }

        try {
          const apiUrl = getApiUrl();
          const response = await fetch(`${apiUrl}/token/refresh/`, {
            method: 'POST',
            credentials: 'include',
            headers: {
              'Content-Type': 'application/json',
            },
            body: JSON.stringify({ refresh: state.refreshToken }),
          });

          if (!response.ok) {
            // Refresh token is invalid, logout user
            get().logout();
            return false;
          }

          const data = await response.json();
          
          // Update access token
          apiService.setToken(data.access);
          set({
            token: data.access,
            // Update refresh token if it's rotated
            ...(data.refresh ? { refreshToken: data.refresh } : {}),
          });
          
          return true;
        } catch {
          // Network error during refresh, logout user
          get().logout();
          return false;
        }
      },

      logout: () => {
        // Clear token from apiService
        apiService.clearToken();
        set({
          user: null,
          token: null,
          refreshToken: null,
          isAuthenticated: false,
          isStaff: false,
        });
      },

      setUser: (user) => {
        set({ user, isStaff: checkIsStaff(user) });
      },

      setToken: (token) => {
        if (token) {
          apiService.setToken(token);
        } else {
          apiService.clearToken();
        }
        set({ token, isAuthenticated: !!token });
      },
    }),
    {
      name: 'auth-storage',
      storage: createJSONStorage(() => localStorage),
      onRehydrateStorage: () => (state) => {
        // Sync token with apiService when store is rehydrated from localStorage
        if (!state) return;
        if (state.token) {
          apiService.setToken(state.token);
        }
      },
    }
  )
);
