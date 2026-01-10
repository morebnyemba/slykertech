# Frontend Integration Guide

## Connecting Next.js Frontend to Django Backend

This guide shows how to integrate the Next.js frontend with the Django backend.

## Setup

### 1. Install Axios (or your preferred HTTP client)

```bash
npm install axios
```

### 2. Create API Client

Create a file at `src/lib/api.ts`:

```typescript
import axios from 'axios';

const API_BASE_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:8000/api';

const api = axios.create({
  baseURL: API_BASE_URL,
  headers: {
    'Content-Type': 'application/json',
  },
});

// Add token to requests if available
api.interceptors.request.use(
  (config) => {
    const token = localStorage.getItem('access_token');
    if (token) {
      config.headers.Authorization = `Bearer ${token}`;
    }
    return config;
  },
  (error) => {
    return Promise.reject(error);
  }
);

// Handle token refresh
api.interceptors.response.use(
  (response) => response,
  async (error) => {
    const originalRequest = error.config;

    if (error.response?.status === 401 && !originalRequest._retry) {
      originalRequest._retry = true;

      try {
        const refreshToken = localStorage.getItem('refresh_token');
        const response = await axios.post(`${API_BASE_URL}/token/refresh/`, {
          refresh: refreshToken,
        });

        const { access } = response.data;
        localStorage.setItem('access_token', access);

        originalRequest.headers.Authorization = `Bearer ${access}`;
        return api(originalRequest);
      } catch (refreshError) {
        // Redirect to login
        localStorage.removeItem('access_token');
        localStorage.removeItem('refresh_token');
        window.location.href = '/login';
        return Promise.reject(refreshError);
      }
    }

    return Promise.reject(error);
  }
);

export default api;
```

### 3. Create Auth Context

Create a file at `src/contexts/AuthContext.tsx`:

```typescript
'use client';

import React, { createContext, useContext, useState, useEffect } from 'react';
import api from '@/lib/api';

interface User {
  id: number;
  email: string;
  first_name: string;
  last_name: string;
  user_type: string;
}

interface AuthContextType {
  user: User | null;
  loading: boolean;
  login: (email: string, password: string) => Promise<void>;
  logout: () => void;
  register: (data: any) => Promise<void>;
}

const AuthContext = createContext<AuthContextType | undefined>(undefined);

export function AuthProvider({ children }: { children: React.ReactNode }) {
  const [user, setUser] = useState<User | null>(null);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    checkAuth();
  }, []);

  const checkAuth = async () => {
    try {
      const token = localStorage.getItem('access_token');
      if (token) {
        const response = await api.get('/accounts/users/me/');
        setUser(response.data);
      }
    } catch (error) {
      console.error('Auth check failed:', error);
      localStorage.removeItem('access_token');
      localStorage.removeItem('refresh_token');
    } finally {
      setLoading(false);
    }
  };

  const login = async (email: string, password: string) => {
    const response = await api.post('/token/', { email, password });
    const { access, refresh } = response.data;
    
    localStorage.setItem('access_token', access);
    localStorage.setItem('refresh_token', refresh);
    
    await checkAuth();
  };

  const logout = () => {
    localStorage.removeItem('access_token');
    localStorage.removeItem('refresh_token');
    setUser(null);
  };

  const register = async (data: any) => {
    await api.post('/accounts/register/', data);
  };

  return (
    <AuthContext.Provider value={{ user, loading, login, logout, register }}>
      {children}
    </AuthContext.Provider>
  );
}

export function useAuth() {
  const context = useContext(AuthContext);
  if (context === undefined) {
    throw new Error('useAuth must be used within an AuthProvider');
  }
  return context;
}
```

### 4. Create API Service Hooks

Create `src/hooks/useServices.ts`:

```typescript
import { useState, useEffect } from 'react';
import api from '@/lib/api';

export function useServices() {
  const [services, setServices] = useState([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);

  useEffect(() => {
    fetchServices();
  }, []);

  const fetchServices = async () => {
    try {
      const response = await api.get('/services/services/');
      setServices(response.data);
    } catch (err: any) {
      setError(err.message);
    } finally {
      setLoading(false);
    }
  };

  return { services, loading, error, refetch: fetchServices };
}
```

Create `src/hooks/useSubscriptions.ts`:

```typescript
import { useState, useEffect } from 'react';
import api from '@/lib/api';

export function useSubscriptions() {
  const [subscriptions, setSubscriptions] = useState([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);

  useEffect(() => {
    fetchSubscriptions();
  }, []);

  const fetchSubscriptions = async () => {
    try {
      const response = await api.get('/services/subscriptions/');
      setSubscriptions(response.data);
    } catch (err: any) {
      setError(err.message);
    } finally {
      setLoading(false);
    }
  };

  const suspendSubscription = async (id: number) => {
    await api.post(`/services/subscriptions/${id}/suspend/`);
    await fetchSubscriptions();
  };

  const activateSubscription = async (id: number) => {
    await api.post(`/services/subscriptions/${id}/activate/`);
    await fetchSubscriptions();
  };

  return { 
    subscriptions, 
    loading, 
    error, 
    refetch: fetchSubscriptions,
    suspendSubscription,
    activateSubscription 
  };
}
```

### 5. Create Client Portal Dashboard

Create `src/app/portal/page.tsx`:

```typescript
'use client';

import { useAuth } from '@/contexts/AuthContext';
import { useSubscriptions } from '@/hooks/useSubscriptions';
import { useRouter } from 'next/navigation';
import { useEffect } from 'react';

export default function PortalPage() {
  const { user, loading: authLoading } = useAuth();
  const { subscriptions, loading: subsLoading } = useSubscriptions();
  const router = useRouter();

  useEffect(() => {
    if (!authLoading && !user) {
      router.push('/login');
    }
  }, [user, authLoading, router]);

  if (authLoading || subsLoading) {
    return <div>Loading...</div>;
  }

  return (
    <div className="container mx-auto px-4 py-8">
      <h1 className="text-3xl font-bold mb-6">Welcome, {user?.first_name}</h1>
      
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
        {subscriptions.map((subscription: any) => (
          <div key={subscription.id} className="border rounded-lg p-6">
            <h2 className="text-xl font-semibold mb-2">
              {subscription.service.name}
            </h2>
            <p className="text-gray-600 mb-4">{subscription.service.description}</p>
            <div className="flex justify-between items-center">
              <span className={`px-3 py-1 rounded-full text-sm ${
                subscription.status === 'active' ? 'bg-green-100 text-green-800' :
                subscription.status === 'suspended' ? 'bg-red-100 text-red-800' :
                'bg-gray-100 text-gray-800'
              }`}>
                {subscription.status}
              </span>
              <span className="font-bold">${subscription.price}</span>
            </div>
          </div>
        ))}
      </div>
    </div>
  );
}
```

### 6. Environment Variables

Add to `.env.local`:

```
NEXT_PUBLIC_API_URL=http://localhost:8000/api
```

For production:
```
NEXT_PUBLIC_API_URL=https://api.slykertech.co.zw/api
```

### 7. Update Layout

Wrap your app with the AuthProvider in `src/app/layout.tsx`:

```typescript
import { AuthProvider } from '@/contexts/AuthContext';

export default function RootLayout({ children }) {
  return (
    <html lang="en">
      <body>
        <AuthProvider>
          {children}
        </AuthProvider>
      </body>
    </html>
  );
}
```

## API Endpoints Summary

### Authentication
- `POST /api/token/` - Login and get JWT tokens
- `POST /api/token/refresh/` - Refresh access token
- `POST /api/accounts/register/` - Register new user
- `GET /api/accounts/users/me/` - Get current user

### Services
- `GET /api/services/services/` - List all services
- `GET /api/services/subscriptions/` - List user subscriptions
- `POST /api/services/subscriptions/{id}/suspend/` - Suspend subscription
- `POST /api/services/subscriptions/{id}/activate/` - Activate subscription

### DNS Management
- `GET /api/services/dns-records/` - List DNS records
- `POST /api/services/dns-records/` - Create DNS record
- `PUT /api/services/dns-records/{id}/` - Update DNS record
- `DELETE /api/services/dns-records/{id}/` - Delete DNS record

### Integrations
- `GET /api/integrations/credentials/` - List integration credentials
- `POST /api/integrations/credentials/` - Add new integration
- `GET /api/integrations/cpanel/` - List cPanel accounts
- `POST /api/integrations/cpanel/{id}/sync/` - Sync cPanel account

## Running Both Servers

### Terminal 1 - Django Backend
```bash
cd backend
python manage.py runserver 8000
```

### Terminal 2 - Next.js Frontend
```bash
npm run dev
```

Visit:
- Frontend: http://localhost:3000
- Backend API: http://localhost:8000/api
- Admin Panel: http://localhost:8000/admin

## Production Deployment

### Backend (Django)
- Use Gunicorn or uWSGI
- Configure Nginx as reverse proxy
- Set up PostgreSQL database
- Enable HTTPS with SSL certificate
- Configure static file serving

### Frontend (Next.js)
- Build with `npm run build`
- Deploy to Vercel, Netlify, or your server
- Configure environment variables
- Enable HTTPS

## Security Considerations

1. Always use HTTPS in production
2. Set secure CORS origins
3. Implement rate limiting
4. Use environment variables for secrets
5. Enable CSRF protection
6. Sanitize user inputs
7. Keep dependencies updated
