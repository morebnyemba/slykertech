'use client';

import { useState, useEffect, useCallback } from 'react';
import AdminLayout from '@/components/admin/AdminLayout';
import { apiService } from '@/lib/api-service';
import { useAuthStore } from '@/lib/stores/auth-store';
import { 
  FaComments, FaSpinner, FaUser, FaClock, FaPaperPlane,
  FaCircle, FaTimes
} from 'react-icons/fa';

interface ChatMessage {
  id: number;
  sender_type: 'user' | 'ai' | 'agent';
  message: string;
  created_at: string;
}

interface ChatSession {
  id: number;
  user: { email: string; first_name?: string; last_name?: string };
  session_id: string;
  status: string;
  messages?: ChatMessage[];
  message_count: number;
  last_message?: {
    message: string;
    sender_type: string;
    created_at: string;
  };
  created_at: string;
  closed_at?: string;
}

interface ChatStats {
  total_sessions: number;
  active_sessions: number;
  today_sessions: number;
  total_messages: number;
  agent_messages: number;
  ai_messages: number;
}

export default function LiveChatPage() {
  const [sessions, setSessions] = useState<ChatSession[]>([]);
  const [stats, setStats] = useState<ChatStats | null>(null);
  const [loading, setLoading] = useState(true);
  const [selectedSession, setSelectedSession] = useState<ChatSession | null>(null);
  const [newMessage, setNewMessage] = useState('');
  const [sending, setSending] = useState(false);
  const [filter, setFilter] = useState<string>('active');

  const fetchSessions = useCallback(async () => {
    // Only fetch sessions if user is authenticated with a valid token
    const { isAuthenticated, token } = useAuthStore.getState();
    if (!isAuthenticated || !token) {
      setLoading(false);
      return;
    }

    try {
      const response = await apiService.getChatSessions(filter || undefined);
      if (response.data) {
        setSessions(response.data as ChatSession[]);
      }
    } catch (error) {
      console.error('Failed to fetch sessions:', error);
    } finally {
      setLoading(false);
    }
  }, [filter]);

  const fetchStats = useCallback(async () => {
    // Only fetch stats if user is authenticated with a valid token
    const { isAuthenticated, token } = useAuthStore.getState();
    if (!isAuthenticated || !token) {
      return;
    }

    try {
      const response = await apiService.getChatStats();
      if (response.data) {
        setStats(response.data as ChatStats);
      }
    } catch (error) {
      console.error('Failed to fetch stats:', error);
    }
  }, []);

  useEffect(() => {
    fetchSessions();
    fetchStats();
    
    // Auto-refresh for live updates
    const interval = setInterval(() => {
      fetchSessions();
      fetchStats();
    }, 10000);
    
    return () => clearInterval(interval);
  }, [fetchSessions, fetchStats]);

  const loadSessionDetails = async (sessionId: number) => {
    try {
      const response = await apiService.getChatSession(sessionId);
      if (response.data) {
        setSelectedSession(response.data as ChatSession);
      }
    } catch (error) {
      console.error('Failed to load session:', error);
    }
  };

  const handleSendMessage = async () => {
    if (!selectedSession || !newMessage.trim()) return;
    
    setSending(true);
    try {
      await apiService.sendChatMessage(selectedSession.id, newMessage);
      setNewMessage('');
      await loadSessionDetails(selectedSession.id);
    } catch (error) {
      console.error('Failed to send message:', error);
    } finally {
      setSending(false);
    }
  };

  const handleCloseSession = async (sessionId: number) => {
    try {
      await apiService.closeChatSession(sessionId);
      fetchSessions();
      if (selectedSession?.id === sessionId) {
        setSelectedSession(null);
      }
    } catch (error) {
      console.error('Failed to close session:', error);
    }
  };

  const statusColors: Record<string, string> = {
    active: 'text-green-500',
    closed: 'text-gray-500',
    transferred: 'text-blue-500',
  };

  const senderColors: Record<string, string> = {
    user: 'bg-gray-100 dark:bg-gray-700',
    ai: 'bg-purple-50 dark:bg-purple-900/20',
    agent: 'bg-blue-50 dark:bg-blue-900/20',
  };

  return (
    <AdminLayout>
      <div className="space-y-6">
        {/* Header with Stats */}
        <div className="flex flex-col lg:flex-row lg:items-center lg:justify-between gap-4">
          <div>
            <h1 className="text-2xl font-bold text-gray-900 dark:text-white">Live Chat Support</h1>
            <p className="text-gray-500 dark:text-gray-400">Monitor and respond to customer chats</p>
          </div>
          
          {stats && (
            <div className="flex flex-wrap gap-3">
              <div className="px-4 py-2 bg-green-50 dark:bg-green-900/20 rounded-lg">
                <span className="text-2xl font-bold text-green-600">{stats.active_sessions}</span>
                <span className="text-sm text-gray-500 ml-2">Active</span>
              </div>
              <div className="px-4 py-2 bg-blue-50 dark:bg-blue-900/20 rounded-lg">
                <span className="text-2xl font-bold text-blue-600">{stats.today_sessions}</span>
                <span className="text-sm text-gray-500 ml-2">Today</span>
              </div>
              <div className="px-4 py-2 bg-purple-50 dark:bg-purple-900/20 rounded-lg">
                <span className="text-2xl font-bold text-purple-600">{stats.total_messages}</span>
                <span className="text-sm text-gray-500 ml-2">Messages</span>
              </div>
            </div>
          )}
        </div>

        {/* Filter Tabs */}
        <div className="flex gap-2">
          {['active', 'closed', ''].map((status) => (
            <button
              key={status || 'all'}
              onClick={() => setFilter(status)}
              className={`px-4 py-2 rounded-lg text-sm font-medium transition-colors ${
                filter === status
                  ? 'bg-blue-600 text-white'
                  : 'bg-gray-100 dark:bg-gray-700 text-gray-600 dark:text-gray-300 hover:bg-gray-200 dark:hover:bg-gray-600'
              }`}
            >
              {status ? status.charAt(0).toUpperCase() + status.slice(1) : 'All'}
            </button>
          ))}
        </div>

        <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
          {/* Session List */}
          <div className="lg:col-span-1 bg-white dark:bg-gray-800 rounded-xl shadow-lg overflow-hidden">
            <div className="p-4 border-b border-gray-200 dark:border-gray-700">
              <h2 className="font-semibold text-gray-900 dark:text-white flex items-center gap-2">
                <FaComments />
                Chat Sessions ({sessions.length})
              </h2>
            </div>
            
            <div className="max-h-[600px] overflow-y-auto">
              {loading ? (
                <div className="flex items-center justify-center py-12">
                  <FaSpinner className="animate-spin h-8 w-8 text-blue-600" />
                </div>
              ) : sessions.length === 0 ? (
                <div className="p-8 text-center text-gray-500">No chat sessions found</div>
              ) : (
                sessions.map((session) => (
                  <div
                    key={session.id}
                    onClick={() => loadSessionDetails(session.id)}
                    className={`p-4 border-b border-gray-100 dark:border-gray-700 cursor-pointer hover:bg-gray-50 dark:hover:bg-gray-700/50 ${
                      selectedSession?.id === session.id ? 'bg-blue-50 dark:bg-blue-900/20' : ''
                    }`}
                  >
                    <div className="flex items-start gap-3">
                      <FaCircle className={`h-2 w-2 mt-2 ${statusColors[session.status]}`} />
                      <div className="flex-1 min-w-0">
                        <p className="font-medium text-gray-900 dark:text-white truncate">
                          {session.user?.first_name || session.user?.email || 'Guest'}
                        </p>
                        {session.last_message && (
                          <p className="text-sm text-gray-500 truncate">
                            {session.last_message.message}
                          </p>
                        )}
                        <div className="flex items-center gap-2 mt-1 text-xs text-gray-400">
                          <FaClock />
                          {new Date(session.created_at).toLocaleString()}
                        </div>
                      </div>
                      <div className="text-xs text-gray-500">
                        {session.message_count} msgs
                      </div>
                    </div>
                  </div>
                ))
              )}
            </div>
          </div>

          {/* Chat Window */}
          <div className="lg:col-span-2 bg-white dark:bg-gray-800 rounded-xl shadow-lg overflow-hidden">
            {selectedSession ? (
              <div className="flex flex-col h-[700px]">
                {/* Header */}
                <div className="p-4 border-b border-gray-200 dark:border-gray-700">
                  <div className="flex items-center justify-between">
                    <div className="flex items-center gap-3">
                      <div className="w-10 h-10 bg-gray-200 dark:bg-gray-700 rounded-full flex items-center justify-center">
                        <FaUser className="text-gray-500" />
                      </div>
                      <div>
                        <p className="font-medium text-gray-900 dark:text-white">
                          {selectedSession.user?.first_name || selectedSession.user?.email || 'Guest'}
                        </p>
                        <p className="text-sm text-gray-500">
                          Session started {new Date(selectedSession.created_at).toLocaleString()}
                        </p>
                      </div>
                    </div>
                    <div className="flex items-center gap-2">
                      <span className={`flex items-center gap-1 text-sm ${statusColors[selectedSession.status]}`}>
                        <FaCircle className="h-2 w-2" />
                        {selectedSession.status}
                      </span>
                      {selectedSession.status === 'active' && (
                        <button
                          onClick={() => handleCloseSession(selectedSession.id)}
                          className="p-2 text-gray-500 hover:text-red-500 hover:bg-red-50 dark:hover:bg-red-900/20 rounded-lg"
                          title="Close session"
                        >
                          <FaTimes />
                        </button>
                      )}
                    </div>
                  </div>
                </div>

                {/* Messages */}
                <div className="flex-1 overflow-y-auto p-4 space-y-4">
                  {selectedSession.messages?.map((message) => (
                    <div
                      key={message.id}
                      className={`rounded-lg p-4 ${senderColors[message.sender_type]} ${
                        message.sender_type === 'agent' ? 'ml-8' : message.sender_type === 'ai' ? 'ml-4' : ''
                      }`}
                    >
                      <div className="flex items-center gap-2 mb-1">
                        <span className="font-medium text-gray-900 dark:text-white text-sm">
                          {message.sender_type === 'user' ? 'Customer' : 
                           message.sender_type === 'ai' ? 'AI Assistant' : 'Support Agent'}
                        </span>
                        <span className="text-xs text-gray-500">
                          {new Date(message.created_at).toLocaleTimeString()}
                        </span>
                      </div>
                      <p className="text-gray-700 dark:text-gray-300 whitespace-pre-wrap">
                        {message.message}
                      </p>
                    </div>
                  ))}
                </div>

                {/* Message Input */}
                {selectedSession.status === 'active' && (
                  <div className="p-4 border-t border-gray-200 dark:border-gray-700">
                    <div className="flex gap-2">
                      <input
                        type="text"
                        value={newMessage}
                        onChange={(e) => setNewMessage(e.target.value)}
                        onKeyPress={(e) => e.key === 'Enter' && handleSendMessage()}
                        placeholder="Type your message..."
                        className="flex-1 px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700"
                      />
                      <button
                        onClick={handleSendMessage}
                        disabled={sending || !newMessage.trim()}
                        className="px-6 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 disabled:opacity-50 flex items-center gap-2"
                      >
                        {sending ? <FaSpinner className="animate-spin" /> : <FaPaperPlane />}
                        Send
                      </button>
                    </div>
                  </div>
                )}
              </div>
            ) : (
              <div className="h-[700px] flex items-center justify-center text-gray-500">
                <div className="text-center">
                  <FaComments className="h-12 w-12 mx-auto mb-4 opacity-50" />
                  <p>Select a chat session to view messages</p>
                </div>
              </div>
            )}
          </div>
        </div>
      </div>
    </AdminLayout>
  );
}
