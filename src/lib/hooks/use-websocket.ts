/**
 * WebSocket Hook for Real-time Updates
 * Manages WebSocket connections for analytics and dashboard updates
 */

import { useEffect, useRef, useState } from 'react';
import { useAuthStore } from '../stores/auth-store';

// WebSocket URL helper
const getWebSocketUrl = (endpoint: string) => {
  const protocol = typeof window !== 'undefined' && window.location.protocol === 'https:' ? 'wss:' : 'ws:';
  const wsBaseFromEnv = process.env.NEXT_PUBLIC_WS_URL?.replace(/\/$/, '');
  const apiHost = process.env.NEXT_PUBLIC_API_URL?.replace(/^https?:\/\//, '').replace(/\/api$/, '') || 'api.slykertech.co.zw';
  const wsBase = wsBaseFromEnv || `${protocol}//${apiHost}`;

  const normalizedEndpoint = endpoint.startsWith('/') ? endpoint : `/${endpoint}`;
  const endpointForBase = wsBase.endsWith('/ws') && normalizedEndpoint.startsWith('/ws/')
    ? normalizedEndpoint.slice(3)
    : normalizedEndpoint;

  return `${wsBase}${endpointForBase}`;
};

const isTokenExpired = (token: string): boolean => {
  try {
    const payloadBase64 = token.split('.')[1];
    if (!payloadBase64) return true;

    const payloadJson = atob(payloadBase64.replace(/-/g, '+').replace(/_/g, '/'));
    const payload = JSON.parse(payloadJson) as { exp?: number };

    if (!payload.exp) return true;

    return Date.now() >= (payload.exp * 1000) - 5000;
  } catch {
    return true;
  }
};

interface UseWebSocketOptions {
  endpoint: string;
  onMessage?: (data: unknown) => void;
  autoReconnect?: boolean;
  reconnectInterval?: number;
}

export function useWebSocket({
  endpoint,
  onMessage,
  autoReconnect = true,
  reconnectInterval = 5000,
}: UseWebSocketOptions) {
  const [isConnected, setIsConnected] = useState(false);
  const [lastMessage, setLastMessage] = useState<unknown>(null);
  const wsRef = useRef<WebSocket | null>(null);
  const reconnectTimeoutRef = useRef<NodeJS.Timeout | null>(null);
  const reconnectAttemptsRef = useRef<number>(0);
  const maxReconnectAttempts = 5; // Limit reconnection attempts
  const { token, isAuthenticated, hasHydrated, refreshAccessToken } = useAuthStore();

  const connect = async () => {
    // Don't connect until auth store has rehydrated
    if (!hasHydrated || !isAuthenticated || !token) {
      return;
    }

    // Don't reconnect if max attempts reached
    if (reconnectAttemptsRef.current >= maxReconnectAttempts) {
      console.warn(`WebSocket max reconnection attempts (${maxReconnectAttempts}) reached for ${endpoint}`);
      return;
    }

    try {
      let activeToken = token;

      if (isTokenExpired(activeToken)) {
        const refreshed = await refreshAccessToken();
        if (!refreshed) {
          console.warn(`WebSocket connection skipped for ${endpoint}: token refresh failed`);
          return;
        }

        const latestToken = useAuthStore.getState().token;
        if (!latestToken || isTokenExpired(latestToken)) {
          console.warn(`WebSocket connection skipped for ${endpoint}: no valid token available`);
          return;
        }

        activeToken = latestToken;
      }

      // Determine WebSocket URL based on environment
      const wsUrl = `${getWebSocketUrl(endpoint)}?token=${encodeURIComponent(activeToken)}`;

      const ws = new WebSocket(wsUrl);

      ws.onopen = () => {
        console.log('WebSocket connected:', endpoint);
        setIsConnected(true);
        reconnectAttemptsRef.current = 0; // Reset on successful connection
        if (reconnectTimeoutRef.current) {
          clearTimeout(reconnectTimeoutRef.current);
          reconnectTimeoutRef.current = null;
        }
      };

      ws.onmessage = (event) => {
        try {
          const data = JSON.parse(event.data);
          setLastMessage(data);
          onMessage?.(data);
        } catch (error) {
          console.error('Failed to parse WebSocket message:', error);
        }
      };

      ws.onerror = (error) => {
        console.error('WebSocket error:', error);
      };

      ws.onclose = (event) => {
        console.log('WebSocket disconnected:', endpoint);
        setIsConnected(false);
        wsRef.current = null;

        const isAuthFailure = event.code === 1008 || event.code === 4001;

        if (isAuthFailure) {
          console.warn(`WebSocket closed due to authentication issue (code ${event.code}) for ${endpoint}`);
          return;
        }

        reconnectAttemptsRef.current++;

        // Auto-reconnect if enabled and under max attempts
        if (autoReconnect && isAuthenticated && reconnectAttemptsRef.current < maxReconnectAttempts) {
          reconnectTimeoutRef.current = setTimeout(() => {
            console.log(`Attempting to reconnect WebSocket (attempt ${reconnectAttemptsRef.current + 1}/${maxReconnectAttempts})...`);
            connect();
          }, reconnectInterval);
        } else if (reconnectAttemptsRef.current >= maxReconnectAttempts) {
          console.warn(`WebSocket reconnection stopped after ${maxReconnectAttempts} attempts`);
        }
      };

      wsRef.current = ws;
    } catch (error) {
      console.error('Failed to create WebSocket connection:', error);
      reconnectAttemptsRef.current++;
    }
  };

  const disconnect = () => {
    if (reconnectTimeoutRef.current) {
      clearTimeout(reconnectTimeoutRef.current);
      reconnectTimeoutRef.current = null;
    }

    if (wsRef.current) {
      wsRef.current.close();
      wsRef.current = null;
    }

    setIsConnected(false);
  };

  const sendMessage = (data: unknown) => {
    if (wsRef.current && wsRef.current.readyState === WebSocket.OPEN) {
      wsRef.current.send(JSON.stringify(data));
    } else {
      console.warn('WebSocket is not connected. Cannot send message.');
    }
  };

  useEffect(() => {
    // Only connect after hydration is complete
    if (hasHydrated) {
      connect();
    }

    return () => {
      disconnect();
    };
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [endpoint, token, isAuthenticated, hasHydrated]);

  return {
    isConnected,
    lastMessage,
    sendMessage,
    connect,
    disconnect,
  };
}

/**
 * Hook specifically for analytics WebSocket
 */
export function useAnalyticsWebSocket(onUpdate?: (data: unknown) => void) {
  return useWebSocket({
    endpoint: '/ws/analytics/',
    onMessage: onUpdate,
    autoReconnect: true,
  });
}
