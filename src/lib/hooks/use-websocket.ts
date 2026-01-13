/**
 * WebSocket Hook for Real-time Updates
 * Manages WebSocket connections for analytics and dashboard updates
 */

import { useEffect, useRef, useState } from 'react';
import { useAuthStore } from '../stores/auth-store';

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
  const { token, isAuthenticated } = useAuthStore();

  const connect = () => {
    if (!isAuthenticated || !token) {
      return;
    }

    try {
      // Determine WebSocket URL based on environment
      const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
      const apiHost = process.env.NEXT_PUBLIC_API_URL?.replace(/^https?:\/\//, '') || 'api.slykertech.co.zw';
      const wsUrl = `${protocol}//${apiHost}${endpoint}?token=${token}`;

      const ws = new WebSocket(wsUrl);

      ws.onopen = () => {
        console.log('WebSocket connected:', endpoint);
        setIsConnected(true);
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

      ws.onclose = () => {
        console.log('WebSocket disconnected:', endpoint);
        setIsConnected(false);
        wsRef.current = null;

        // Auto-reconnect if enabled
        if (autoReconnect && isAuthenticated) {
          reconnectTimeoutRef.current = setTimeout(() => {
            console.log('Attempting to reconnect WebSocket...');
            connect();
          }, reconnectInterval);
        }
      };

      wsRef.current = ws;
    } catch (error) {
      console.error('Failed to create WebSocket connection:', error);
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
    connect();

    return () => {
      disconnect();
    };
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [endpoint, token, isAuthenticated]);

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
