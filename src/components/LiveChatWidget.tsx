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

const WEBSOCKET_URL = process.env.NEXT_PUBLIC_WS_URL || 'wss://api.slykertech.co.zw/ws';

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

  const connectWebSocket = (department: string) => {
    if (wsRef.current?.readyState === WebSocket.OPEN) {
      wsRef.current.close();
    }

    const ws = new WebSocket(`${WEBSOCKET_URL}/chat/${department}/`);
    
    ws.onopen = () => {
      setIsConnected(true);
      setMessages([]);
    };

    ws.onmessage = (event) => {
      const data = JSON.parse(event.data);
      
      if (data.type === 'connection_established') {
        setMessages(prev => [...prev, {
          type: 'system',
          message: data.message,
          timestamp: new Date().toISOString(),
        }]);
      } else if (data.type === 'chat_message') {
        setMessages(prev => [...prev, data]);
      } else if (data.type === 'typing') {
        // Handle typing indicator
        if (data.is_typing) {
          setMessages(prev => [...prev, { type: 'typing', sender: data.sender }]);
        } else {
          setMessages(prev => prev.filter(m => !(m.type === 'typing' && m.sender === data.sender)));
        }
      }
    };

    ws.onerror = (error) => {
      console.error('WebSocket error:', error);
      setMessages(prev => [...prev, {
        type: 'error',
        message: 'Connection error. Please try again.',
        timestamp: new Date().toISOString(),
      }]);
    };

    ws.onclose = () => {
      setIsConnected(false);
    };

    wsRef.current = ws;
  };

  const handleDepartmentSelect = (deptId: string) => {
    setSelectedDepartment(deptId);
    connectWebSocket(deptId);
  };

  const sendMessage = () => {
    if (!inputMessage.trim() || !wsRef.current || !isConnected) return;

    const message = {
      type: 'chat_message',
      message: inputMessage,
      sender: userName || 'Guest',
      timestamp: new Date().toISOString(),
    };

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
        </button>
      )}

      {/* Chat Window */}
      {isOpen && (
        <div className="fixed bottom-6 right-6 z-50 w-96 h-[600px] bg-white dark:bg-gray-900 rounded-2xl shadow-2xl flex flex-col overflow-hidden border border-gray-200 dark:border-gray-700">
          {/* Header */}
          <div className="bg-gradient-to-r from-blue-600 to-blue-700 text-white p-4 flex justify-between items-center">
            <div className="flex items-center gap-2">
              <FaComments className="w-5 h-5" />
              <div>
                <h3 className="font-bold">Slyker Tech Live Chat</h3>
                <p className="text-xs opacity-90">
                  {isConnected ? 'Connected' : 'Connecting...'}
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
                  {messages.map((msg, index) => (
                    <div
                      key={index}
                      className={`flex ${
                        msg.sender === userName ? 'justify-end' : 'justify-start'
                      }`}
                    >
                      {msg.type === 'system' ? (
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
                              ? 'bg-blue-600 text-white'
                              : 'bg-gray-200 dark:bg-gray-700 text-gray-900 dark:text-gray-100'
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
                  ))}
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
                      placeholder="Type your message..."
                      className="flex-1 px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-800 text-gray-900 dark:text-gray-100"
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
