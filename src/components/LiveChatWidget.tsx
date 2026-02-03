'use client';

import { useState, useEffect, useRef } from 'react';
import { FaComments, FaTimes, FaPaperPlane, FaCircle } from 'react-icons/fa';

interface Message {
  id: string;
  text: string;
  sender: 'user' | 'support' | 'system';
  timestamp: Date;
}

export default function LiveChatWidget() {
  const [isOpen, setIsOpen] = useState(false);
  const [messages, setMessages] = useState<Message[]>([]);
  const [inputText, setInputText] = useState('');
  const [isConnected, setIsConnected] = useState(false);
  const [visitorName, setVisitorName] = useState('');
  const [hasSetName, setHasSetName] = useState(false);
  const messagesEndRef = useRef<HTMLDivElement>(null);
  const wsRef = useRef<WebSocket | null>(null);

  // Determine WebSocket URL based on environment
  const getWebSocketURL = () => {
    if (typeof window === 'undefined') return '';
    
    const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
    const host = window.location.host;
    
    // In production, route through nginx to Erlang service
    return `${protocol}//${host}/livechat/ws/`;
  };

  const scrollToBottom = () => {
    messagesEndRef.current?.scrollIntoView({ behavior: 'smooth' });
  };

  useEffect(() => {
    scrollToBottom();
  }, [messages]);

  const connectWebSocket = () => {
    if (wsRef.current?.readyState === WebSocket.OPEN) {
      return;
    }

    try {
      const wsURL = getWebSocketURL();
      if (!wsURL) return;

      const ws = new WebSocket(wsURL);
      
      ws.onopen = () => {
        console.log('Connected to live chat');
        setIsConnected(true);
        addSystemMessage('Connected to support. How can we help?');
      };

      ws.onmessage = (event) => {
        try {
          const data = JSON.parse(event.data);
          
          if (data.type === 'message') {
            addMessage(data.sender || 'support', data.text || data.message);
          } else if (data.type === 'system') {
            addSystemMessage(data.text || data.message);
          }
        } catch (e) {
          console.error('Error parsing message:', e);
          addMessage('support', event.data);
        }
      };

      ws.onerror = (error) => {
        console.error('WebSocket error:', error);
        addSystemMessage('Connection error. Please try again.');
      };

      ws.onclose = () => {
        console.log('Disconnected from live chat');
        setIsConnected(false);
        addSystemMessage('Disconnected. Attempting to reconnect...');
        // Attempt reconnect after 3 seconds
        setTimeout(() => {
          if (isOpen) connectWebSocket();
        }, 3000);
      };

      wsRef.current = ws;
    } catch (error) {
      console.error('Failed to connect:', error);
      addSystemMessage('Failed to connect. Please refresh and try again.');
    }
  };

  const addMessage = (sender: 'user' | 'support' | 'system', text: string) => {
    setMessages(prev => [...prev, {
      id: Date.now().toString(),
      text,
      sender,
      timestamp: new Date()
    }]);
  };

  const addSystemMessage = (text: string) => {
    addMessage('system', text);
  };

  const sendMessage = () => {
    if (!inputText.trim() || !wsRef.current || wsRef.current.readyState !== WebSocket.OPEN) {
      return;
    }

    addMessage('user', inputText);

    try {
      wsRef.current.send(JSON.stringify({
        type: 'message',
        message: inputText,
        visitor_name: visitorName,
        timestamp: new Date().toISOString()
      }));
    } catch (error) {
      console.error('Failed to send message:', error);
      addSystemMessage('Failed to send message. Please try again.');
    }

    setInputText('');
  };

  const handleOpen = () => {
    setIsOpen(true);
    if (!hasSetName) {
      const name = prompt('Please enter your name:') || 'Visitor';
      setVisitorName(name);
      setHasSetName(true);
    }
    if (!isConnected) {
      connectWebSocket();
    }
  };

  const handleClose = () => {
    setIsOpen(false);
  };

  if (!isOpen) {
    return (
      <button
        onClick={handleOpen}
        className="fixed bottom-6 right-6 w-14 h-14 bg-gradient-to-br from-blue-600 to-blue-700 text-white rounded-full shadow-lg flex items-center justify-center hover:shadow-xl hover:scale-110 transition-all z-50"
        title="Open live chat"
        aria-label="Open live chat support"
      >
        <FaComments className="w-6 h-6" />
        <span className={`absolute top-0 right-0 w-3 h-3 rounded-full ${isConnected ? 'bg-green-500' : 'bg-red-500'}`} />
      </button>
    );
  }

  return (
    <div className="fixed bottom-6 right-6 w-96 max-h-[600px] bg-white dark:bg-gray-800 rounded-lg shadow-2xl flex flex-col z-50 border border-gray-200 dark:border-gray-700">
      {/* Header */}
      <div className="bg-gradient-to-r from-blue-600 to-blue-700 text-white p-4 rounded-t-lg flex justify-between items-center flex-shrink-0">
        <div className="flex items-center gap-3">
          <div className={`w-3 h-3 rounded-full ${isConnected ? 'bg-green-400' : 'bg-red-400'}`} />
          <div>
            <div className="font-semibold text-sm">Live Support</div>
            <div className="text-xs text-blue-100">{isConnected ? 'Online' : 'Offline'}</div>
          </div>
        </div>
        <button 
          onClick={handleClose} 
          className="text-white hover:text-blue-100 transition-colors"
          aria-label="Close chat"
        >
          <FaTimes className="w-5 h-5" />
        </button>
      </div>

      {/* Messages Area */}
      <div className="flex-1 overflow-y-auto p-4 space-y-3 bg-gray-50 dark:bg-gray-900">
        {messages.length === 0 ? (
          <div className="text-center text-gray-500 dark:text-gray-400 py-8">
            <div className="text-4xl mb-2">👋</div>
            <p className="text-sm">Start a conversation with us!</p>
          </div>
        ) : (
          messages.map(msg => (
            <div key={msg.id} className={`flex ${msg.sender === 'user' ? 'justify-end' : 'justify-start'}`}>
              <div
                className={`max-w-xs px-4 py-2 rounded-lg text-sm ${
                  msg.sender === 'user'
                    ? 'bg-blue-600 text-white rounded-br-none'
                    : msg.sender === 'system'
                    ? 'bg-gray-300 dark:bg-gray-700 text-gray-900 dark:text-gray-100 italic'
                    : 'bg-gray-200 dark:bg-gray-700 text-gray-900 dark:text-gray-100 rounded-bl-none'
                }`}
              >
                {msg.text}
              </div>
            </div>
          ))
        )}
        <div ref={messagesEndRef} />
      </div>

      {/* Input Area */}
      <div className="border-t border-gray-200 dark:border-gray-700 p-4 flex gap-2 flex-shrink-0">
        <input
          type="text"
          value={inputText}
          onChange={(e) => setInputText(e.target.value)}
          onKeyPress={(e) => e.key === 'Enter' && sendMessage()}
          placeholder={isConnected ? 'Type a message...' : 'Connecting...'}
          disabled={!isConnected}
          className="flex-1 px-3 py-2 border border-gray-300 dark:border-gray-600 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-600 dark:bg-gray-700 dark:text-white disabled:opacity-50 disabled:cursor-not-allowed"
        />
        <button
          onClick={sendMessage}
          disabled={!isConnected || !inputText.trim()}
          className="px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 disabled:opacity-50 disabled:cursor-not-allowed transition-colors flex items-center gap-2"
          aria-label="Send message"
        >
          <FaPaperPlane className="w-4 h-4" />
        </button>
      </div>
    </div>
  );
}

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
                <h3 className="font-bold">Slyker Tech Web Services Live Chat</h3>
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
