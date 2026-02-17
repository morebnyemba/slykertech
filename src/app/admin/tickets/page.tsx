'use client';

import { useState, useEffect, useCallback } from 'react';
import AdminLayout from '@/components/admin/AdminLayout';
import { apiService } from '@/lib/api-service';
import { useAuthStore } from '@/lib/stores/auth-store';
import { 
  FaTicketAlt, FaSpinner, FaSearch, FaReply, FaUser, FaClock
} from 'react-icons/fa';

interface TicketReply {
  id: number;
  user: { email: string; first_name?: string; last_name?: string };
  message: string;
  is_staff_reply: boolean;
  is_internal: boolean;
  created_at: string;
}

interface Ticket {
  id: number;
  user: { email: string; first_name?: string; last_name?: string; company_name?: string };
  assigned_to?: { email: string; first_name?: string };
  subject: string;
  description: string;
  department: string;
  status: string;
  priority: string;
  replies?: TicketReply[];
  reply_count: number;
  created_at: string;
  updated_at: string;
}

interface TicketStats {
  total: number;
  open: number;
  in_progress: number;
  pending: number;
  resolved: number;
  closed: number;
  unassigned: number;
  assigned_to_me: number;
  by_priority: {
    critical: number;
    high: number;
    medium: number;
    low: number;
  };
}

const normalizeList = <T,>(data: unknown): T[] => {
  if (Array.isArray(data)) return data as T[];

  if (data && typeof data === 'object' && 'results' in data) {
    const results = (data as { results?: unknown }).results;
    return Array.isArray(results) ? (results as T[]) : [];
  }

  return [];
};

export default function TicketsPage() {
  const { isAuthenticated, token, hasHydrated } = useAuthStore();
  const [tickets, setTickets] = useState<Ticket[]>([]);
  const [stats, setStats] = useState<TicketStats | null>(null);
  const [loading, setLoading] = useState(true);
  const [selectedTicket, setSelectedTicket] = useState<Ticket | null>(null);
  const [replyMessage, setReplyMessage] = useState('');
  const [isInternal, setIsInternal] = useState(false);
  const [sending, setSending] = useState(false);
  const [filter, setFilter] = useState({ status: '', priority: '', assigned_to_me: false });
  const [searchTerm, setSearchTerm] = useState('');

  const fetchTickets = useCallback(async () => {
    // Only fetch tickets if hydration is complete and user is authenticated with a valid token
    if (!hasHydrated || !isAuthenticated || !token) {
      setLoading(false);
      return;
    }

    try {
      const params: { status?: string; priority?: string; assigned_to_me?: boolean } = {};
      if (filter.status) params.status = filter.status;
      if (filter.priority) params.priority = filter.priority;
      if (filter.assigned_to_me) params.assigned_to_me = true;
      
      const response = await apiService.getTickets(params);
      if (response.data) {
        setTickets(normalizeList<Ticket>(response.data));
      } else {
        setTickets([]);
      }
    } catch (error) {
      console.error('Failed to fetch tickets:', error);
      setTickets([]);
    } finally {
      setLoading(false);
    }
  }, [filter, hasHydrated, isAuthenticated, token]);

  const fetchStats = useCallback(async () => {
    // Only fetch stats if hydration is complete and user is authenticated with a valid token
    if (!hasHydrated || !isAuthenticated || !token) {
      return;
    }

    try {
      const response = await apiService.getTicketStats();
      if (response.data) {
        setStats(response.data as TicketStats);
      }
    } catch (error) {
      console.error('Failed to fetch stats:', error);
    }
  }, [hasHydrated, isAuthenticated, token]);

  useEffect(() => {
    fetchTickets();
    fetchStats();
  }, [fetchTickets, fetchStats]);

  const loadTicketDetails = async (ticketId: number) => {
    try {
      const response = await apiService.getTicket(ticketId);
      if (response.data) {
        const ticket = response.data as Ticket;
        setSelectedTicket({
          ...ticket,
          replies: normalizeList<TicketReply>(ticket.replies),
        });
      }
    } catch (error) {
      console.error('Failed to load ticket:', error);
    }
  };

  const handleReply = async () => {
    if (!selectedTicket || !replyMessage.trim()) return;
    
    setSending(true);
    try {
      await apiService.replyToTicket(selectedTicket.id, replyMessage, isInternal);
      setReplyMessage('');
      setIsInternal(false);
      await loadTicketDetails(selectedTicket.id);
      fetchTickets();
    } catch (error) {
      console.error('Failed to send reply:', error);
    } finally {
      setSending(false);
    }
  };

  const handleAssignToMe = async (ticketId: number) => {
    try {
      await apiService.assignTicketToMe(ticketId);
      fetchTickets();
      if (selectedTicket?.id === ticketId) {
        loadTicketDetails(ticketId);
      }
    } catch (error) {
      console.error('Failed to assign ticket:', error);
    }
  };

  const handleStatusChange = async (ticketId: number, newStatus: string) => {
    try {
      await apiService.updateTicket(ticketId, { status: newStatus });
      fetchTickets();
      fetchStats();
      if (selectedTicket?.id === ticketId) {
        loadTicketDetails(ticketId);
      }
    } catch (error) {
      console.error('Failed to update status:', error);
    }
  };

  const statusColors: Record<string, string> = {
    open: 'bg-blue-100 text-blue-800 dark:bg-blue-900/30 dark:text-blue-400',
    in_progress: 'bg-yellow-100 text-yellow-800 dark:bg-yellow-900/30 dark:text-yellow-400',
    pending: 'bg-orange-100 text-orange-800 dark:bg-orange-900/30 dark:text-orange-400',
    resolved: 'bg-green-100 text-green-800 dark:bg-green-900/30 dark:text-green-400',
    closed: 'bg-gray-100 text-gray-800 dark:bg-gray-700 dark:text-gray-400',
  };

  const priorityColors: Record<string, string> = {
    critical: 'bg-red-100 text-red-800 dark:bg-red-900/30 dark:text-red-400',
    high: 'bg-orange-100 text-orange-800 dark:bg-orange-900/30 dark:text-orange-400',
    medium: 'bg-yellow-100 text-yellow-800 dark:bg-yellow-900/30 dark:text-yellow-400',
    low: 'bg-gray-100 text-gray-800 dark:bg-gray-700 dark:text-gray-400',
  };

  const filteredTickets = tickets.filter(ticket =>
    ticket.subject.toLowerCase().includes(searchTerm.toLowerCase()) ||
    ticket.user?.email?.toLowerCase().includes(searchTerm.toLowerCase())
  );

  return (
    <AdminLayout>
      <div className="space-y-6">
        {/* Header with Stats */}
        <div className="flex flex-col lg:flex-row lg:items-center lg:justify-between gap-4">
          <div>
            <h1 className="text-2xl font-bold text-gray-900 dark:text-white">Support Tickets</h1>
            <p className="text-gray-500 dark:text-gray-400">Manage and respond to customer tickets</p>
          </div>
          
          {stats && (
            <div className="flex flex-wrap gap-3">
              <div className="px-4 py-2 bg-blue-50 dark:bg-blue-900/20 rounded-lg">
                <span className="text-2xl font-bold text-blue-600">{stats.open}</span>
                <span className="text-sm text-gray-500 ml-2">Open</span>
              </div>
              <div className="px-4 py-2 bg-yellow-50 dark:bg-yellow-900/20 rounded-lg">
                <span className="text-2xl font-bold text-yellow-600">{stats.in_progress}</span>
                <span className="text-sm text-gray-500 ml-2">In Progress</span>
              </div>
              <div className="px-4 py-2 bg-red-50 dark:bg-red-900/20 rounded-lg">
                <span className="text-2xl font-bold text-red-600">{stats.unassigned}</span>
                <span className="text-sm text-gray-500 ml-2">Unassigned</span>
              </div>
            </div>
          )}
        </div>

        {/* Filters */}
        <div className="flex flex-wrap gap-4 items-center">
          <div className="relative flex-1 min-w-[200px]">
            <FaSearch className="absolute left-3 top-1/2 -translate-y-1/2 text-gray-400" />
            <input
              type="text"
              placeholder="Search tickets..."
              value={searchTerm}
              onChange={(e) => setSearchTerm(e.target.value)}
              className="w-full pl-10 pr-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-800"
            />
          </div>
          
          <select
            value={filter.status}
            onChange={(e) => setFilter(prev => ({ ...prev, status: e.target.value }))}
            className="px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-800"
          >
            <option value="">All Status</option>
            <option value="open">Open</option>
            <option value="in_progress">In Progress</option>
            <option value="pending">Pending</option>
            <option value="resolved">Resolved</option>
            <option value="closed">Closed</option>
          </select>
          
          <select
            value={filter.priority}
            onChange={(e) => setFilter(prev => ({ ...prev, priority: e.target.value }))}
            className="px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-800"
          >
            <option value="">All Priority</option>
            <option value="critical">Critical</option>
            <option value="high">High</option>
            <option value="medium">Medium</option>
            <option value="low">Low</option>
          </select>
          
          <label className="flex items-center gap-2 cursor-pointer">
            <input
              type="checkbox"
              checked={filter.assigned_to_me}
              onChange={(e) => setFilter(prev => ({ ...prev, assigned_to_me: e.target.checked }))}
              className="rounded"
            />
            <span className="text-sm text-gray-700 dark:text-gray-300">Assigned to me</span>
          </label>
        </div>

        <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
          {/* Ticket List */}
          <div className="lg:col-span-1 bg-white dark:bg-gray-800 rounded-xl shadow-lg overflow-hidden">
            <div className="p-4 border-b border-gray-200 dark:border-gray-700">
              <h2 className="font-semibold text-gray-900 dark:text-white flex items-center gap-2">
                <FaTicketAlt />
                Tickets ({filteredTickets.length})
              </h2>
            </div>
            
            <div className="max-h-[600px] overflow-y-auto">
              {loading ? (
                <div className="flex items-center justify-center py-12">
                  <FaSpinner className="animate-spin h-8 w-8 text-blue-600" />
                </div>
              ) : filteredTickets.length === 0 ? (
                <div className="p-8 text-center text-gray-500">No tickets found</div>
              ) : (
                filteredTickets.map((ticket) => (
                  <div
                    key={ticket.id}
                    onClick={() => loadTicketDetails(ticket.id)}
                    className={`p-4 border-b border-gray-100 dark:border-gray-700 cursor-pointer hover:bg-gray-50 dark:hover:bg-gray-700/50 ${
                      selectedTicket?.id === ticket.id ? 'bg-blue-50 dark:bg-blue-900/20' : ''
                    }`}
                  >
                    <div className="flex items-start gap-3">
                      <div className="flex-1 min-w-0">
                        <p className="font-medium text-gray-900 dark:text-white truncate">
                          #{ticket.id} - {ticket.subject}
                        </p>
                        <p className="text-sm text-gray-500 truncate">{ticket.user?.email}</p>
                        <div className="flex items-center gap-2 mt-2">
                          <span className={`px-2 py-0.5 text-xs rounded-full ${statusColors[ticket.status]}`}>
                            {ticket.status.replace('_', ' ')}
                          </span>
                          <span className={`px-2 py-0.5 text-xs rounded-full ${priorityColors[ticket.priority]}`}>
                            {ticket.priority}
                          </span>
                        </div>
                      </div>
                      {ticket.reply_count > 0 && (
                        <span className="text-xs text-gray-500">
                          <FaReply className="inline mr-1" />
                          {ticket.reply_count}
                        </span>
                      )}
                    </div>
                  </div>
                ))
              )}
            </div>
          </div>

          {/* Ticket Details */}
          <div className="lg:col-span-2 bg-white dark:bg-gray-800 rounded-xl shadow-lg overflow-hidden">
            {selectedTicket ? (
              <div className="flex flex-col h-[700px]">
                {/* Header */}
                <div className="p-4 border-b border-gray-200 dark:border-gray-700">
                  <div className="flex items-start justify-between">
                    <div>
                      <h2 className="text-lg font-semibold text-gray-900 dark:text-white">
                        #{selectedTicket.id} - {selectedTicket.subject}
                      </h2>
                      <div className="flex items-center gap-4 mt-2 text-sm text-gray-500">
                        <span className="flex items-center gap-1">
                          <FaUser /> {selectedTicket.user?.email}
                        </span>
                        <span className="flex items-center gap-1">
                          <FaClock /> {new Date(selectedTicket.created_at).toLocaleString()}
                        </span>
                      </div>
                    </div>
                    <div className="flex items-center gap-2">
                      <select
                        value={selectedTicket.status}
                        onChange={(e) => handleStatusChange(selectedTicket.id, e.target.value)}
                        className={`px-3 py-1 text-sm rounded-lg border-0 ${statusColors[selectedTicket.status]}`}
                      >
                        <option value="open">Open</option>
                        <option value="in_progress">In Progress</option>
                        <option value="pending">Pending</option>
                        <option value="resolved">Resolved</option>
                        <option value="closed">Closed</option>
                      </select>
                      {!selectedTicket.assigned_to && (
                        <button
                          onClick={() => handleAssignToMe(selectedTicket.id)}
                          className="px-3 py-1 text-sm bg-blue-600 text-white rounded-lg hover:bg-blue-700"
                        >
                          Assign to me
                        </button>
                      )}
                    </div>
                  </div>
                </div>

                {/* Messages */}
                <div className="flex-1 overflow-y-auto p-4 space-y-4">
                  {/* Original message */}
                  <div className="bg-gray-50 dark:bg-gray-700/50 rounded-lg p-4">
                    <div className="flex items-center gap-2 mb-2">
                      <span className="font-medium text-gray-900 dark:text-white">
                        {selectedTicket.user?.first_name || selectedTicket.user?.email}
                      </span>
                      <span className="text-xs text-gray-500">
                        {new Date(selectedTicket.created_at).toLocaleString()}
                      </span>
                    </div>
                    <p className="text-gray-700 dark:text-gray-300 whitespace-pre-wrap">
                      {selectedTicket.description}
                    </p>
                  </div>

                  {/* Replies */}
                  {normalizeList<TicketReply>(selectedTicket.replies).map((reply) => (
                    <div
                      key={reply.id}
                      className={`rounded-lg p-4 ${
                        reply.is_staff_reply
                          ? 'bg-blue-50 dark:bg-blue-900/20 ml-8'
                          : 'bg-gray-50 dark:bg-gray-700/50'
                      } ${reply.is_internal ? 'border-2 border-yellow-400' : ''}`}
                    >
                      <div className="flex items-center gap-2 mb-2">
                        <span className="font-medium text-gray-900 dark:text-white">
                          {reply.user?.first_name || reply.user?.email}
                        </span>
                        {reply.is_staff_reply && (
                          <span className="px-2 py-0.5 bg-blue-600 text-white text-xs rounded">Staff</span>
                        )}
                        {reply.is_internal && (
                          <span className="px-2 py-0.5 bg-yellow-500 text-white text-xs rounded">Internal</span>
                        )}
                        <span className="text-xs text-gray-500">
                          {new Date(reply.created_at).toLocaleString()}
                        </span>
                      </div>
                      <p className="text-gray-700 dark:text-gray-300 whitespace-pre-wrap">
                        {reply.message}
                      </p>
                    </div>
                  ))}
                </div>

                {/* Reply Box */}
                <div className="p-4 border-t border-gray-200 dark:border-gray-700">
                  <div className="flex items-center gap-2 mb-2">
                    <label className="flex items-center gap-2 text-sm text-gray-600 dark:text-gray-400">
                      <input
                        type="checkbox"
                        checked={isInternal}
                        onChange={(e) => setIsInternal(e.target.checked)}
                        className="rounded"
                      />
                      Internal note (not visible to client)
                    </label>
                  </div>
                  <div className="flex gap-2">
                    <textarea
                      value={replyMessage}
                      onChange={(e) => setReplyMessage(e.target.value)}
                      placeholder="Type your reply..."
                      rows={3}
                      className="flex-1 px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 resize-none"
                    />
                    <button
                      onClick={handleReply}
                      disabled={sending || !replyMessage.trim()}
                      className="px-6 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 disabled:opacity-50 self-end"
                    >
                      {sending ? <FaSpinner className="animate-spin" /> : 'Send'}
                    </button>
                  </div>
                </div>
              </div>
            ) : (
              <div className="h-[700px] flex items-center justify-center text-gray-500">
                <div className="text-center">
                  <FaTicketAlt className="h-12 w-12 mx-auto mb-4 opacity-50" />
                  <p>Select a ticket to view details</p>
                </div>
              </div>
            )}
          </div>
        </div>
      </div>
    </AdminLayout>
  );
}
