'use client';

import { useState, useEffect, useRef } from 'react';
import { FaComments, FaTimes, FaPaperPlane } from 'react-icons/fa';

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
