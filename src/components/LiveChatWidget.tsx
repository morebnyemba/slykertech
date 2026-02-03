'use client';

import { useState, useEffect, useRef } from 'react';
import { FaComments, FaTimes, FaPaperPlane } from 'react-icons/fa';
import { useAuthStore } from '@/lib/stores/auth-store';
import { isUserStaff } from '@/lib/utils/user-roles';

interface Message {
  type: string;
  message?: string;
  sender?: string;
  timestamp?: string;
  is_typing?: boolean;
}

export default function LiveChatWidget() {
  const [isOpen, setIsOpen] = useState(false);
  const [messages, setMessages] = useState<Message[]>([]);
  const [inputMessage, setInputMessage] = useState('');
  const [isConnected, setIsConnected] = useState(false);
  const [selectedDepartment, setSelectedDepartment] = useState<string | null>(null);
  const [userName, setUserName] = useState('');
  const [isNameSet, setIsNameSet] = useState(false);
  
  const { isAuthenticated, user } = useAuthStore();
  const wsRef = useRef<WebSocket | null>(null);
  const messagesEndRef = useRef<HTMLDivElement>(null);
  const reconnectAttemptsRef = useRef(0);
  const reconnectTimeoutRef = useRef<ReturnType<typeof setTimeout> | null>(null);
  const heartbeatIntervalRef = useRef<ReturnType<typeof setInterval> | null>(null);
  const lastPongRef = useRef<number>(Date.now());
  const isConnectingRef = useRef(false);
  const sessionIdRef = useRef<string>(typeof crypto !== 'undefined' && 'randomUUID' in crypto ? crypto.randomUUID() : `${Date.now()}-${Math.random()}`);

  const isStaff = isUserStaff(user);

  const departments = [
    { id: 'sales', name: 'Sales', description: 'Product inquiries and quotes' },
    { id: 'support', name: 'Technical Support', description: 'Technical assistance' },
    { id: 'billing', name: 'Billing', description: 'Invoices and payments' },
    ...(isStaff ? [{ id: 'management', name: 'Management', description: 'Internal communications' }] : []),
  ];

  const scrollToBottom = () => {
    messagesEndRef.current?.scrollIntoView({ behavior: 'smooth' });
  };

  useEffect(() => {
    scrollToBottom();
  }, [messages]);

  useEffect(() => {
    // Set user name if authenticated
    if (isAuthenticated && user) {
      setUserName(`${user.first_name} ${user.last_name}`.trim() || user.email);
      setIsNameSet(true);
    }
  }, [isAuthenticated, user]);

  const getWebSocketURL = (department: string) => {
    if (typeof window === 'undefined') return '';
    
    const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
    const host = window.location.host; // includes port if not standard
    
    // Connect through nginx to Erlang livechat service
    const url = `${protocol}//${host}/livechat/ws/${department}/`;
    console.log('WebSocket URL:', url, 'Protocol:', protocol, 'Host:', host);
    return url;
  };

  const clearReconnectTimer = () => {
    if (reconnectTimeoutRef.current) {
      clearTimeout(reconnectTimeoutRef.current);
      reconnectTimeoutRef.current = null;
    }
  };

  const stopHeartbeat = () => {
    if (heartbeatIntervalRef.current) {
      clearInterval(heartbeatIntervalRef.current);
      heartbeatIntervalRef.current = null;
    }
  };

  const startHeartbeat = () => {
    stopHeartbeat();
    lastPongRef.current = Date.now();
    heartbeatIntervalRef.current = setInterval(() => {
      if (!wsRef.current || wsRef.current.readyState !== WebSocket.OPEN) return;

      const now = Date.now();
      const elapsed = now - lastPongRef.current;
      if (elapsed > 60000) {
        console.warn('Heartbeat timeout. Closing WebSocket.');
        wsRef.current.close();
        return;
      }

      wsRef.current.send(JSON.stringify({ type: 'ping', timestamp: new Date().toISOString() }));
    }, 25000);
  };

  const scheduleReconnect = () => {
    clearReconnectTimer();
    const attempt = reconnectAttemptsRef.current + 1;
    reconnectAttemptsRef.current = attempt;
    const baseDelay = Math.min(30000, 1000 * Math.pow(2, Math.min(5, attempt)));
    const jitter = Math.floor(Math.random() * 500);
    const delay = baseDelay + jitter;

    reconnectTimeoutRef.current = setTimeout(() => {
      if (isOpen && selectedDepartment) {
        console.log('Reconnecting to department:', selectedDepartment, 'attempt:', attempt);
        connectWebSocket(selectedDepartment);
      }
    }, delay);
  };

  const connectWebSocket = (department: string) => {
    if (isConnectingRef.current) return;
    if (wsRef.current?.readyState === WebSocket.CONNECTING) return;
    if (wsRef.current?.readyState === WebSocket.OPEN) {
      wsRef.current.close();
    }

    const wsURL = getWebSocketURL(department);
    if (!wsURL) {
      console.error('Failed to construct WebSocket URL');
      return;
    }

    console.log('Attempting WebSocket connection to:', wsURL);
    
    try {
      isConnectingRef.current = true;
      const ws = new WebSocket(wsURL);
      
      ws.onopen = () => {
        console.log('✓ WebSocket connected successfully for department:', department);
        isConnectingRef.current = false;
        setIsConnected(true);
        reconnectAttemptsRef.current = 0;
        clearReconnectTimer();
        startHeartbeat();
        setMessages([]);
        setMessages(prev => [...prev, {
          type: 'system',
          message: 'Connected to live support. How can we help you today?',
          timestamp: new Date().toISOString(),
        }]);
      };

      ws.onmessage = (event) => {
        try {
          const data = JSON.parse(event.data);
          console.log('Message received:', data);
          
          if (data.type === 'system') {
            setMessages(prev => [...prev, {
              type: 'system',
              message: data.message,
              timestamp: new Date().toISOString(),
            }]);
          } else if (data.type === 'ping') {
            wsRef.current?.send(JSON.stringify({ type: 'pong', timestamp: new Date().toISOString() }));
          } else if (data.type === 'pong') {
            lastPongRef.current = Date.now();
          } else if (data.type === 'message') {
            setMessages(prev => [...prev, {
              type: 'chat_message',
              message: data.text || data.message,
              sender: data.sender || 'Support',
              timestamp: new Date().toISOString(),
            }]);
            if (data.action_result?.message) {
              setMessages(prev => [...prev, {
                type: 'system',
                message: data.action_result.message,
                timestamp: new Date().toISOString(),
              }]);
            }
          } else if (data.type === 'typing') {
            if (data.is_typing) {
              setMessages(prev => [...prev, { type: 'typing', sender: data.sender }]);
            } else {
              setMessages(prev => prev.filter(m => !(m.type === 'typing' && m.sender === data.sender)));
            }
          }
        } catch (e) {
          console.error('Error parsing message:', e, 'Raw data:', event.data);
        }
      };

      ws.onerror = (error) => {
        console.error('✗ WebSocket error:', error);
        console.error('WebSocket readyState:', ws.readyState);
        isConnectingRef.current = false;
        setMessages(prev => [...prev, {
          type: 'error',
          message: 'Connection error. Check browser console for details.',
          timestamp: new Date().toISOString(),
        }]);
      };

      ws.onclose = (closeEvent) => {
        console.log('WebSocket closed. Code:', closeEvent.code, 'Reason:', closeEvent.reason);
        isConnectingRef.current = false;
        stopHeartbeat();
        setIsConnected(false);
        setMessages(prev => [...prev, {
          type: 'system',
          message: 'Disconnected. Attempting to reconnect...',
          timestamp: new Date().toISOString(),
        }]);

        scheduleReconnect();
      };

      wsRef.current = ws;
    } catch (error) {
      console.error('✗ Failed to create WebSocket:', error);
      isConnectingRef.current = false;
      setMessages(prev => [...prev, {
        type: 'error',
        message: 'Failed to create connection. Please try again.',
        timestamp: new Date().toISOString(),
      }]);
    }
  };

  const handleDepartmentSelect = (deptId: string) => {
    setSelectedDepartment(deptId);
    connectWebSocket(deptId);
  };

  const sendMessage = () => {
    if (!inputMessage.trim() || !wsRef.current || !isConnected) return;

    const message = {
      type: 'message',
      message: inputMessage,
      visitor_name: userName || 'Guest',
      department: selectedDepartment || 'support',
      session_id: sessionIdRef.current,
      user_id: user?.id,
      sender: userName || 'Guest',
      timestamp: new Date().toISOString(),
    };

    // Add to local messages immediately
    setMessages(prev => [...prev, {
      type: 'chat_message',
      message: inputMessage,
      sender: userName || 'Guest',
      timestamp: new Date().toISOString(),
    }]);

    wsRef.current.send(JSON.stringify(message));
    setInputMessage('');
  };

  const handleKeyPress = (e: React.KeyboardEvent) => {
    if (e.key === 'Enter' && !e.shiftKey) {
      e.preventDefault();
      sendMessage();
    }
  };

  const handleClose = () => {
    setIsOpen(false);
    // Keep WebSocket connection alive for persistent chat
  };

  useEffect(() => {
    // Cleanup on unmount
    return () => {
      if (wsRef.current) {
        wsRef.current.close();
      }
      stopHeartbeat();
      clearReconnectTimer();
    };
  }, []);

  return (
    <>
      {/* Chat Button */}
      {!isOpen && (
        <button
          onClick={() => setIsOpen(true)}
          className="fixed bottom-6 right-6 z-50 bg-gradient-to-r from-blue-600 to-blue-700 text-white p-4 rounded-full shadow-lg hover:shadow-xl transition-all duration-300 transform hover:scale-110"
          aria-label="Open live chat"
        >
          <FaComments className="w-6 h-6" />
          {isConnected && (
            <span className="absolute top-0 right-0 w-3 h-3 bg-green-500 rounded-full border-2 border-white" />
          )}
        </button>
      )}

      {/* Chat Window */}
      {isOpen && (
        <div className="fixed bottom-6 right-6 z-50 w-96 h-[600px] bg-white dark:bg-gray-900 rounded-2xl shadow-2xl flex flex-col overflow-hidden border border-gray-200 dark:border-gray-700">
          {/* Header */}
          <div className="bg-gradient-to-r from-blue-600 to-blue-700 text-white p-4 flex justify-between items-center">
            <div className="flex items-center gap-2">
              <div className={`w-2 h-2 rounded-full ${isConnected ? 'bg-green-400' : 'bg-red-400'}`} />
              <div>
                <h3 className="font-bold text-sm">Slyker Tech Live Chat</h3>
                <p className="text-xs opacity-90">
                  {isConnected ? 'Online' : 'Connecting...'}
                </p>
              </div>
            </div>
            <button
              onClick={handleClose}
              className="text-white hover:bg-white/20 rounded-full p-2 transition-colors"
              aria-label="Close chat"
            >
              <FaTimes className="w-5 h-5" />
            </button>
          </div>

          {/* Content */}
          <div className="flex-1 flex flex-col overflow-hidden">
            {!isNameSet ? (
              /* Name Input */
              <div className="flex-1 flex flex-col items-center justify-center p-6">
                <FaComments className="w-16 h-16 text-blue-600 mb-4" />
                <h4 className="text-lg font-bold text-gray-900 dark:text-gray-100 mb-2">
                  Welcome to Live Chat
                </h4>
                <p className="text-sm text-gray-600 dark:text-gray-400 mb-4 text-center">
                  Please enter your name to start chatting
                </p>
                <input
                  type="text"
                  value={userName}
                  onChange={(e) => setUserName(e.target.value)}
                  placeholder="Your name"
                  className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg mb-4 bg-white dark:bg-gray-800 text-gray-900 dark:text-gray-100"
                  onKeyPress={(e) => {
                    if (e.key === 'Enter' && userName.trim()) {
                      setIsNameSet(true);
                    }
                  }}
                />
                <button
                  onClick={() => userName.trim() && setIsNameSet(true)}
                  disabled={!userName.trim()}
                  className="w-full bg-blue-600 text-white py-2 rounded-lg hover:bg-blue-700 disabled:opacity-50 disabled:cursor-not-allowed transition-colors"
                >
                  Start Chat
                </button>
              </div>
            ) : !selectedDepartment ? (
              /* Department Selection */
              <div className="flex-1 overflow-y-auto p-4">
                <h4 className="text-lg font-bold text-gray-900 dark:text-gray-100 mb-4">
                  Select a Department
                </h4>
                <div className="space-y-3">
                  {departments.map((dept) => (
                    <button
                      key={dept.id}
                      onClick={() => handleDepartmentSelect(dept.id)}
                      className="w-full p-4 bg-gray-50 dark:bg-gray-800 hover:bg-blue-50 dark:hover:bg-gray-700 rounded-lg transition-colors text-left border border-gray-200 dark:border-gray-700"
                    >
                      <h5 className="font-bold text-gray-900 dark:text-gray-100">{dept.name}</h5>
                      <p className="text-sm text-gray-600 dark:text-gray-400">{dept.description}</p>
                    </button>
                  ))}
                </div>
              </div>
            ) : (
              /* Chat Messages */
              <>
                <div className="flex-1 overflow-y-auto p-4 space-y-3">
                  {messages.length === 0 ? (
                    <div className="text-center text-gray-500 dark:text-gray-400 py-8">
                      <p className="text-sm">Start a conversation...</p>
                    </div>
                  ) : (
                    messages.map((msg, index) => (
                      <div
                        key={index}
                        className={`flex ${
                          msg.sender === userName ? 'justify-end' : 'justify-start'
                        }`}
                      >
                        {msg.type === 'system' || msg.type === 'error' ? (
                          <div className="text-center w-full">
                            <p className="text-xs text-gray-500 dark:text-gray-400 italic">
                              {msg.message}
                            </p>
                          </div>
                        ) : msg.type === 'typing' ? (
                          <div className="bg-gray-200 dark:bg-gray-700 rounded-lg px-4 py-2">
                            <p className="text-sm text-gray-600 dark:text-gray-400">
                              {msg.sender} is typing...
                            </p>
                          </div>
                        ) : (
                          <div
                            className={`max-w-[75%] rounded-lg px-4 py-2 ${
                              msg.sender === userName
                                ? 'bg-blue-600 text-white rounded-br-none'
                                : 'bg-gray-200 dark:bg-gray-700 text-gray-900 dark:text-gray-100 rounded-bl-none'
                            }`}
                          >
                            {msg.sender !== userName && (
                              <p className="text-xs font-bold mb-1 opacity-75">{msg.sender}</p>
                            )}
                            <p className="text-sm">{msg.message}</p>
                            {msg.timestamp && (
                              <p className="text-xs mt-1 opacity-75">
                                {new Date(msg.timestamp).toLocaleTimeString()}
                              </p>
                            )}
                          </div>
                        )}
                      </div>
                    ))
                  )}
                  <div ref={messagesEndRef} />
                </div>

                {/* Message Input */}
                <div className="p-4 border-t border-gray-200 dark:border-gray-700">
                  <div className="flex gap-2">
                    <input
                      type="text"
                      value={inputMessage}
                      onChange={(e) => setInputMessage(e.target.value)}
                      onKeyPress={handleKeyPress}
                      placeholder={isConnected ? "Type your message..." : "Connecting..."}
                      className="flex-1 px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-800 text-gray-900 dark:text-gray-100 disabled:opacity-50"
                      disabled={!isConnected}
                    />
                    <button
                      onClick={sendMessage}
                      disabled={!inputMessage.trim() || !isConnected}
                      className="bg-blue-600 text-white p-2 rounded-lg hover:bg-blue-700 disabled:opacity-50 disabled:cursor-not-allowed transition-colors"
                      aria-label="Send message"
                    >
                      <FaPaperPlane className="w-5 h-5" />
                    </button>
                  </div>
                </div>
              </>
            )}
          </div>
        </div>
      )}
    </>
  );
}
