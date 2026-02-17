'use client';

import { useState, useEffect } from 'react';
import Link from 'next/link';
import {
  FaTicketAlt, FaSync, FaPlus, FaCheckCircle, FaClock,
  FaExclamationCircle, FaTimes, FaReply, FaEye
} from 'react-icons/fa';
import { useAuthStore } from '@/lib/stores/auth-store';
import { apiService } from '@/lib/api-service';

interface Ticket {
  id: number;
  subject: string;
  description: string;
  status: string;
  priority: string;
  department?: string;
  created_at: string;
  updated_at: string;
}

export default function TicketsPage() {
  const { isAuthenticated } = useAuthStore();
  const [tickets, setTickets] = useState<Ticket[]>([]);
  const [loading, setLoading] = useState(true);
  const [filter, setFilter] = useState<'all' | 'open' | 'closed'>('all');
  const [showNewTicket, setShowNewTicket] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [successMessage, setSuccessMessage] = useState<string | null>(null);

  useEffect(() => {
    if (isAuthenticated) {
      loadTickets();
    }
  }, [isAuthenticated]);

  useEffect(() => {
    if (error || successMessage) {
      const timer = setTimeout(() => {
        setError(null);
        setSuccessMessage(null);
      }, 5000);
      return () => clearTimeout(timer);
    }
  }, [error, successMessage]);

  const loadTickets = async () => {
    setLoading(true);
    try {
      const response = await apiService.getTickets();
      if (response.data) {
        const ticketData = response.data.results || response.data;
        setTickets(Array.isArray(ticketData) ? ticketData : []);
      }
    } catch (err) {
      console.error('Failed to load tickets:', err);
    } finally {
      setLoading(false);
    }
  };

  const handleCreateTicket = async (data: { subject: string; description: string; priority: string; department: string }) => {
    try {
      const response = await apiService.createTicket(data);
      if (response.error) {
        setError(response.error);
      } else {
        setSuccessMessage('Ticket created successfully');
        setShowNewTicket(false);
        loadTickets();
      }
    } catch {
      setError('Failed to create ticket');
    }
  };

  const getStatusBadge = (status: string) => {
    const statusConfig: Record<string, { color: string; icon: JSX.Element }> = {
      open: { color: 'bg-blue-100 text-blue-800 dark:bg-blue-900 dark:text-blue-200', icon: <FaExclamationCircle /> },
      'in-progress': { color: 'bg-yellow-100 text-yellow-800 dark:bg-yellow-900 dark:text-yellow-200', icon: <FaClock /> },
      answered: { color: 'bg-green-100 text-green-800 dark:bg-green-900 dark:text-green-200', icon: <FaReply /> },
      closed: { color: 'bg-gray-100 text-gray-800 dark:bg-gray-700 dark:text-gray-200', icon: <FaCheckCircle /> },
    };
    const config = statusConfig[status.toLowerCase()] || statusConfig.open;
    return (
      <span className={`inline-flex items-center gap-1 px-2 py-1 rounded-full text-xs font-medium ${config.color}`}>
        {config.icon}
        {status}
      </span>
    );
  };

  const getPriorityBadge = (priority: string) => {
    const priorityConfig: Record<string, string> = {
      low: 'bg-gray-100 text-gray-800 dark:bg-gray-700 dark:text-gray-200',
      medium: 'bg-yellow-100 text-yellow-800 dark:bg-yellow-900 dark:text-yellow-200',
      high: 'bg-orange-100 text-orange-800 dark:bg-orange-900 dark:text-orange-200',
      urgent: 'bg-red-100 text-red-800 dark:bg-red-900 dark:text-red-200',
    };
    const colorClass = priorityConfig[priority.toLowerCase()] || priorityConfig.medium;
    return (
      <span className={`px-2 py-1 rounded-full text-xs font-medium ${colorClass}`}>
        {priority}
      </span>
    );
  };

  const filteredTickets = tickets.filter((ticket) => {
    if (filter === 'all') return true;
    if (filter === 'open') return ticket.status.toLowerCase() !== 'closed';
    return ticket.status.toLowerCase() === 'closed';
  });

  const openCount = tickets.filter((t) => t.status.toLowerCase() !== 'closed').length;

  if (!isAuthenticated) {
    return null;
  }

  return (
    <div className="min-h-screen bg-gray-50 dark:bg-gray-900 pt-24 pb-12 px-4">
      <div className="max-w-7xl mx-auto">
        {/* Notification banners */}
        {error && (
          <div className="mb-4 p-4 bg-red-50 dark:bg-red-900/20 border border-red-200 dark:border-red-800 rounded-lg flex items-center justify-between">
            <span className="text-red-800 dark:text-red-200">{error}</span>
            <button onClick={() => setError(null)} className="text-red-600 dark:text-red-400 hover:opacity-70">
              <FaTimes />
            </button>
          </div>
        )}
        {successMessage && (
          <div className="mb-4 p-4 bg-green-50 dark:bg-green-900/20 border border-green-200 dark:border-green-800 rounded-lg flex items-center justify-between">
            <span className="text-green-800 dark:text-green-200">{successMessage}</span>
            <button onClick={() => setSuccessMessage(null)} className="text-green-600 dark:text-green-400 hover:opacity-70">
              <FaTimes />
            </button>
          </div>
        )}

        {/* Header */}
        <div className="mb-8">
          <div className="flex items-center justify-between">
            <div>
              <h1 className="text-3xl font-bold text-gray-900 dark:text-white mb-2 flex items-center gap-3">
                <FaTicketAlt className="text-indigo-600" />
                Support Tickets
                {openCount > 0 && (
                  <span className="px-3 py-1 bg-blue-500 text-white text-sm rounded-full">
                    {openCount} open
                  </span>
                )}
              </h1>
              <p className="text-gray-600 dark:text-gray-400">
                Get help from our support team
              </p>
            </div>
            <div className="flex items-center gap-4">
              <button
                onClick={loadTickets}
                className="flex items-center gap-2 px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors"
              >
                <FaSync className={loading ? 'animate-spin' : ''} />
                Refresh
              </button>
              <button
                onClick={() => setShowNewTicket(true)}
                className="flex items-center gap-2 px-4 py-2 bg-green-600 text-white rounded-lg hover:bg-green-700 transition-colors"
              >
                <FaPlus />
                New Ticket
              </button>
            </div>
          </div>
        </div>

        {/* Filter Tabs */}
        <div className="mb-6 flex gap-4">
          {(['all', 'open', 'closed'] as const).map((status) => (
            <button
              key={status}
              onClick={() => setFilter(status)}
              className={`px-4 py-2 rounded-lg font-medium transition-colors ${
                filter === status
                  ? 'bg-blue-600 text-white'
                  : 'bg-white dark:bg-gray-800 text-gray-700 dark:text-gray-300 hover:bg-gray-100 dark:hover:bg-gray-700'
              }`}
            >
              {status.charAt(0).toUpperCase() + status.slice(1)}
              {status === 'open' && openCount > 0 && (
                <span className="ml-2 px-2 py-0.5 bg-blue-500 text-white text-xs rounded-full">
                  {openCount}
                </span>
              )}
            </button>
          ))}
        </div>

        {/* Tickets List */}
        <div className="space-y-4">
          {loading ? (
            <div className="text-center py-12">
              <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-blue-600 mx-auto"></div>
              <p className="mt-4 text-gray-600 dark:text-gray-400">Loading tickets...</p>
            </div>
          ) : filteredTickets.length === 0 ? (
            <div className="text-center py-12 bg-white dark:bg-gray-800 rounded-lg shadow">
              <FaTicketAlt className="text-5xl text-gray-400 mx-auto mb-4" />
              <p className="text-gray-600 dark:text-gray-400 mb-2">No tickets found</p>
              <p className="text-sm text-gray-500 dark:text-gray-500 mb-6">
                Create a ticket to get help from our support team
              </p>
              <button
                onClick={() => setShowNewTicket(true)}
                className="inline-flex items-center gap-2 px-6 py-3 bg-green-600 text-white rounded-lg hover:bg-green-700 transition-colors"
              >
                <FaPlus />
                Create a Ticket
              </button>
            </div>
          ) : (
            filteredTickets.map((ticket) => (
              <div
                key={ticket.id}
                className="bg-white dark:bg-gray-800 rounded-lg shadow p-6 hover:shadow-lg transition-all duration-200"
              >
                <div className="flex items-start justify-between">
                  <div className="flex-1">
                    <div className="flex items-center gap-3 mb-2">
                      <h3 className="text-lg font-semibold text-gray-900 dark:text-white">
                        {ticket.subject}
                      </h3>
                      {getStatusBadge(ticket.status)}
                      {getPriorityBadge(ticket.priority)}
                    </div>
                    <p className="text-gray-600 dark:text-gray-400 text-sm mb-3 line-clamp-2">
                      {ticket.description}
                    </p>
                    <div className="flex items-center gap-4 text-sm text-gray-500 dark:text-gray-400">
                      <span>Created: {new Date(ticket.created_at).toLocaleDateString()}</span>
                      <span>Updated: {new Date(ticket.updated_at).toLocaleDateString()}</span>
                      {ticket.department && <span>Dept: {ticket.department}</span>}
                    </div>
                  </div>
                  <Link
                    href={`/dashboard/tickets/${ticket.id}`}
                    className="flex items-center gap-2 px-4 py-2 bg-blue-100 text-blue-700 dark:bg-blue-900 dark:text-blue-200 rounded-lg hover:bg-blue-200 dark:hover:bg-blue-800 transition-colors ml-4"
                  >
                    <FaEye />
                    View
                  </Link>
                </div>
              </div>
            ))
          )}
        </div>

        {/* Back to Dashboard */}
        <div className="mt-8">
          <Link
            href="/dashboard"
            className="text-blue-600 hover:text-blue-700 dark:text-blue-400 dark:hover:text-blue-300"
          >
            ← Back to Dashboard
          </Link>
        </div>
      </div>

      {/* New Ticket Modal */}
      {showNewTicket && (
        <NewTicketModal
          onClose={() => setShowNewTicket(false)}
          onSubmit={handleCreateTicket}
        />
      )}
    </div>
  );
}

interface NewTicketModalProps {
  onClose: () => void;
  onSubmit: (data: { subject: string; description: string; priority: string; department: string }) => void;
}

function NewTicketModal({ onClose, onSubmit }: NewTicketModalProps) {
  const [subject, setSubject] = useState('');
  const [description, setDescription] = useState('');
  const [priority, setPriority] = useState('medium');
  const [department, setDepartment] = useState('support');

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    onSubmit({ subject, description, priority, department });
  };

  return (
    <div className="fixed inset-0 z-50 flex items-center justify-center bg-black/50">
      <div className="bg-white dark:bg-gray-800 rounded-lg shadow-xl w-full max-w-lg p-6">
        <div className="flex items-center justify-between mb-4">
          <h2 className="text-xl font-semibold text-gray-900 dark:text-white">Create Support Ticket</h2>
          <button onClick={onClose} className="text-gray-500 hover:text-gray-700 dark:text-gray-400 dark:hover:text-gray-200">
            <FaTimes />
          </button>
        </div>
        <form onSubmit={handleSubmit} className="space-y-4">
          <div>
            <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">Subject</label>
            <input
              type="text"
              value={subject}
              onChange={(e) => setSubject(e.target.value)}
              required
              placeholder="Brief description of your issue"
              className="w-full px-3 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-gray-100"
            />
          </div>
          <div>
            <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">Description</label>
            <textarea
              value={description}
              onChange={(e) => setDescription(e.target.value)}
              required
              rows={4}
              placeholder="Provide details about your issue..."
              className="w-full px-3 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-gray-100"
            />
          </div>
          <div className="grid grid-cols-2 gap-4">
            <div>
              <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">Priority</label>
              <select
                value={priority}
                onChange={(e) => setPriority(e.target.value)}
                className="w-full px-3 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-gray-100"
              >
                <option value="low">Low</option>
                <option value="medium">Medium</option>
                <option value="high">High</option>
                <option value="urgent">Urgent</option>
              </select>
            </div>
            <div>
              <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">Department</label>
              <select
                value={department}
                onChange={(e) => setDepartment(e.target.value)}
                className="w-full px-3 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-gray-100"
              >
                <option value="support">General Support</option>
                <option value="billing">Billing</option>
                <option value="technical">Technical</option>
                <option value="sales">Sales</option>
              </select>
            </div>
          </div>
          <div className="flex justify-end gap-3 pt-2">
            <button
              type="button"
              onClick={onClose}
              className="px-4 py-2 text-gray-700 dark:text-gray-300 bg-gray-100 dark:bg-gray-700 rounded-lg hover:bg-gray-200 dark:hover:bg-gray-600 transition-colors"
            >
              Cancel
            </button>
            <button
              type="submit"
              className="px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors"
            >
              Create Ticket
            </button>
          </div>
        </form>
      </div>
    </div>
  );
}
