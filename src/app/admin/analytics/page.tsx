'use client';

import { useState, useEffect, useCallback } from 'react';
import AdminLayout from '@/components/admin/AdminLayout';
import { apiService } from '@/lib/api-service';
import { 
  FaChartBar, FaSpinner, FaUsers, FaServer, FaTicketAlt, 
  FaComments, FaExclamationTriangle, FaCheckCircle, FaArrowUp,
  FaDollarSign, FaFileInvoiceDollar, FaMoneyBillWave
} from 'react-icons/fa';

interface TicketStats {
  total: number;
  open: number;
  in_progress: number;
  pending: number;
  resolved: number;
  closed: number;
  by_priority: {
    critical: number;
    high: number;
    medium: number;
    low: number;
  };
}

interface ChatStats {
  total_sessions: number;
  active_sessions: number;
  today_sessions: number;
  total_messages: number;
  agent_messages: number;
  ai_messages: number;
}

interface Subscription {
  status: string;
  service?: { category?: string };
}

interface InvoiceStats {
  total: number;
  by_status: {
    draft: number;
    sent: number;
    paid: number;
    overdue: number;
    cancelled: number;
  };
  revenue: {
    total: number;
    this_month: number;
    last_month: number;
    growth_percent: number;
  };
  outstanding: number;
  recent_count: number;
}

interface PaymentStats {
  total: number;
  by_status: {
    completed: number;
    pending: number;
    failed: number;
    refunded: number;
  };
  by_method: Record<string, { count: number; total: number }>;
  total_completed: number;
  last_30_days: number;
}

export default function AnalyticsPage() {
  const [loading, setLoading] = useState(true);
  const [ticketStats, setTicketStats] = useState<TicketStats | null>(null);
  const [chatStats, setChatStats] = useState<ChatStats | null>(null);
  const [invoiceStats, setInvoiceStats] = useState<InvoiceStats | null>(null);
  const [paymentStats, setPaymentStats] = useState<PaymentStats | null>(null);
  const [subscriptions, setSubscriptions] = useState<Subscription[]>([]);
  const [clients, setClients] = useState<unknown[]>([]);
  const [pendingFailures, setPendingFailures] = useState(0);

  const fetchAllData = useCallback(async () => {
    try {
      const [ticketRes, chatRes, subsRes, clientsRes, failuresRes, invoiceRes, paymentRes] = await Promise.all([
        apiService.getTicketStats().catch(() => ({ data: null })),
        apiService.getChatStats().catch(() => ({ data: null })),
        apiService.getAllSubscriptions().catch(() => ({ data: [] })),
        apiService.getAllClients().catch(() => ({ data: [] })),
        apiService.getPendingFailuresCount().catch(() => ({ data: { pending_count: 0 } })),
        apiService.getInvoiceStats().catch(() => ({ data: null })),
        apiService.getPaymentStats().catch(() => ({ data: null })),
      ]);

      if (ticketRes.data) setTicketStats(ticketRes.data as TicketStats);
      if (chatRes.data) setChatStats(chatRes.data as ChatStats);
      if (invoiceRes.data) setInvoiceStats(invoiceRes.data as InvoiceStats);
      if (paymentRes.data) setPaymentStats(paymentRes.data as PaymentStats);
      if (Array.isArray(subsRes.data)) setSubscriptions(subsRes.data as Subscription[]);
      if (Array.isArray(clientsRes.data)) setClients(clientsRes.data);
      setPendingFailures((failuresRes.data as { pending_count: number })?.pending_count || 0);
    } catch (error) {
      console.error('Failed to fetch analytics:', error);
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => {
    fetchAllData();
  }, [fetchAllData]);

  // Calculate subscription stats
  const activeSubscriptions = subscriptions.filter(s => s.status === 'active').length;
  const pendingSubscriptions = subscriptions.filter(s => s.status === 'pending').length;
  const suspendedSubscriptions = subscriptions.filter(s => s.status === 'suspended').length;

  // Format currency
  const formatCurrency = (amount: number) => {
    return new Intl.NumberFormat('en-US', {
      style: 'currency',
      currency: 'USD',
    }).format(amount);
  };

  // Category distribution
  const categoryCount: Record<string, number> = {};
  subscriptions.forEach(sub => {
    const cat = sub.service?.category || 'other';
    categoryCount[cat] = (categoryCount[cat] || 0) + 1;
  });

  if (loading) {
    return (
      <AdminLayout>
        <div className="flex items-center justify-center py-24">
          <FaSpinner className="animate-spin h-12 w-12 text-blue-600" />
        </div>
      </AdminLayout>
    );
  }

  return (
    <AdminLayout>
      <div className="space-y-8">
        {/* Header */}
        <div>
          <h1 className="text-2xl font-bold text-gray-900 dark:text-white flex items-center gap-2">
            <FaChartBar />
            Analytics Dashboard
          </h1>
          <p className="text-gray-500 dark:text-gray-400">
            Overview of your business metrics and performance
          </p>
        </div>

        {/* Revenue Stats Row */}
        {invoiceStats && (
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6">
            <div className="bg-gradient-to-br from-green-500 to-green-600 rounded-xl shadow-lg p-6 text-white">
              <div className="flex items-center justify-between">
                <div className="p-3 bg-white/20 rounded-lg">
                  <FaDollarSign className="h-6 w-6" />
                </div>
                {invoiceStats.revenue.growth_percent > 0 && (
                  <span className="text-sm flex items-center gap-1 bg-white/20 px-2 py-1 rounded">
                    <FaArrowUp /> {invoiceStats.revenue.growth_percent.toFixed(1)}%
                  </span>
                )}
              </div>
              <h3 className="text-3xl font-bold mt-4">
                {formatCurrency(invoiceStats.revenue.this_month)}
              </h3>
              <p className="text-white/80">This Month Revenue</p>
            </div>

            <div className="bg-gradient-to-br from-blue-500 to-blue-600 rounded-xl shadow-lg p-6 text-white">
              <div className="flex items-center justify-between">
                <div className="p-3 bg-white/20 rounded-lg">
                  <FaMoneyBillWave className="h-6 w-6" />
                </div>
              </div>
              <h3 className="text-3xl font-bold mt-4">
                {formatCurrency(invoiceStats.revenue.total)}
              </h3>
              <p className="text-white/80">Total Revenue</p>
            </div>

            <div className="bg-gradient-to-br from-yellow-500 to-yellow-600 rounded-xl shadow-lg p-6 text-white">
              <div className="flex items-center justify-between">
                <div className="p-3 bg-white/20 rounded-lg">
                  <FaFileInvoiceDollar className="h-6 w-6" />
                </div>
                <span className="text-sm bg-white/20 px-2 py-1 rounded">
                  {invoiceStats.by_status.overdue} overdue
                </span>
              </div>
              <h3 className="text-3xl font-bold mt-4">
                {formatCurrency(invoiceStats.outstanding)}
              </h3>
              <p className="text-white/80">Outstanding Balance</p>
            </div>

            <div className="bg-gradient-to-br from-purple-500 to-purple-600 rounded-xl shadow-lg p-6 text-white">
              <div className="flex items-center justify-between">
                <div className="p-3 bg-white/20 rounded-lg">
                  <FaFileInvoiceDollar className="h-6 w-6" />
                </div>
              </div>
              <h3 className="text-3xl font-bold mt-4">
                {invoiceStats.total}
              </h3>
              <p className="text-white/80">Total Invoices</p>
            </div>
          </div>
        )}

        {/* Main Stats Grid */}
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6">
          <div className="bg-white dark:bg-gray-800 rounded-xl shadow-lg p-6">
            <div className="flex items-center justify-between">
              <div className="p-3 bg-blue-100 dark:bg-blue-900/30 rounded-lg">
                <FaUsers className="h-6 w-6 text-blue-600" />
              </div>
              <span className="text-green-500 text-sm flex items-center gap-1">
                <FaArrowUp /> Active
              </span>
            </div>
            <h3 className="text-3xl font-bold text-gray-900 dark:text-white mt-4">
              {clients.length}
            </h3>
            <p className="text-gray-500 dark:text-gray-400">Total Clients</p>
          </div>

          <div className="bg-white dark:bg-gray-800 rounded-xl shadow-lg p-6">
            <div className="flex items-center justify-between">
              <div className="p-3 bg-green-100 dark:bg-green-900/30 rounded-lg">
                <FaServer className="h-6 w-6 text-green-600" />
              </div>
              <span className="text-green-500 text-sm flex items-center gap-1">
                <FaCheckCircle /> {activeSubscriptions} active
              </span>
            </div>
            <h3 className="text-3xl font-bold text-gray-900 dark:text-white mt-4">
              {subscriptions.length}
            </h3>
            <p className="text-gray-500 dark:text-gray-400">Total Subscriptions</p>
          </div>

          <div className="bg-white dark:bg-gray-800 rounded-xl shadow-lg p-6">
            <div className="flex items-center justify-between">
              <div className="p-3 bg-yellow-100 dark:bg-yellow-900/30 rounded-lg">
                <FaTicketAlt className="h-6 w-6 text-yellow-600" />
              </div>
              <span className="text-yellow-500 text-sm">
                {ticketStats?.open || 0} open
              </span>
            </div>
            <h3 className="text-3xl font-bold text-gray-900 dark:text-white mt-4">
              {ticketStats?.total || 0}
            </h3>
            <p className="text-gray-500 dark:text-gray-400">Total Tickets</p>
          </div>

          <div className="bg-white dark:bg-gray-800 rounded-xl shadow-lg p-6">
            <div className="flex items-center justify-between">
              <div className="p-3 bg-purple-100 dark:bg-purple-900/30 rounded-lg">
                <FaComments className="h-6 w-6 text-purple-600" />
              </div>
              <span className="text-green-500 text-sm">
                {chatStats?.active_sessions || 0} active
              </span>
            </div>
            <h3 className="text-3xl font-bold text-gray-900 dark:text-white mt-4">
              {chatStats?.total_sessions || 0}
            </h3>
            <p className="text-gray-500 dark:text-gray-400">Chat Sessions</p>
          </div>
        </div>

        {/* Detailed Stats Row */}
        <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
          {/* Subscription Status */}
          <div className="bg-white dark:bg-gray-800 rounded-xl shadow-lg p-6">
            <h3 className="font-semibold text-gray-900 dark:text-white mb-4">
              Subscription Status
            </h3>
            <div className="space-y-4">
              <div className="flex items-center justify-between">
                <span className="text-gray-600 dark:text-gray-400">Active</span>
                <div className="flex items-center gap-2">
                  <div className="w-32 h-2 bg-gray-200 dark:bg-gray-700 rounded-full overflow-hidden">
                    <div 
                      className="h-full bg-green-500 rounded-full"
                      style={{ width: `${subscriptions.length ? (activeSubscriptions / subscriptions.length * 100) : 0}%` }}
                    />
                  </div>
                  <span className="font-medium text-gray-900 dark:text-white">{activeSubscriptions}</span>
                </div>
              </div>
              <div className="flex items-center justify-between">
                <span className="text-gray-600 dark:text-gray-400">Pending</span>
                <div className="flex items-center gap-2">
                  <div className="w-32 h-2 bg-gray-200 dark:bg-gray-700 rounded-full overflow-hidden">
                    <div 
                      className="h-full bg-yellow-500 rounded-full"
                      style={{ width: `${subscriptions.length ? (pendingSubscriptions / subscriptions.length * 100) : 0}%` }}
                    />
                  </div>
                  <span className="font-medium text-gray-900 dark:text-white">{pendingSubscriptions}</span>
                </div>
              </div>
              <div className="flex items-center justify-between">
                <span className="text-gray-600 dark:text-gray-400">Suspended</span>
                <div className="flex items-center gap-2">
                  <div className="w-32 h-2 bg-gray-200 dark:bg-gray-700 rounded-full overflow-hidden">
                    <div 
                      className="h-full bg-red-500 rounded-full"
                      style={{ width: `${subscriptions.length ? (suspendedSubscriptions / subscriptions.length * 100) : 0}%` }}
                    />
                  </div>
                  <span className="font-medium text-gray-900 dark:text-white">{suspendedSubscriptions}</span>
                </div>
              </div>
            </div>
          </div>

          {/* Ticket Stats */}
          <div className="bg-white dark:bg-gray-800 rounded-xl shadow-lg p-6">
            <h3 className="font-semibold text-gray-900 dark:text-white mb-4">
              Ticket Distribution
            </h3>
            {ticketStats ? (
              <div className="space-y-3">
                <div className="flex items-center justify-between py-2 border-b border-gray-100 dark:border-gray-700">
                  <span className="text-gray-600 dark:text-gray-400">Open</span>
                  <span className="px-3 py-1 bg-blue-100 text-blue-800 dark:bg-blue-900/30 dark:text-blue-400 rounded-full text-sm font-medium">
                    {ticketStats.open}
                  </span>
                </div>
                <div className="flex items-center justify-between py-2 border-b border-gray-100 dark:border-gray-700">
                  <span className="text-gray-600 dark:text-gray-400">In Progress</span>
                  <span className="px-3 py-1 bg-yellow-100 text-yellow-800 dark:bg-yellow-900/30 dark:text-yellow-400 rounded-full text-sm font-medium">
                    {ticketStats.in_progress}
                  </span>
                </div>
                <div className="flex items-center justify-between py-2 border-b border-gray-100 dark:border-gray-700">
                  <span className="text-gray-600 dark:text-gray-400">Resolved</span>
                  <span className="px-3 py-1 bg-green-100 text-green-800 dark:bg-green-900/30 dark:text-green-400 rounded-full text-sm font-medium">
                    {ticketStats.resolved}
                  </span>
                </div>
                <div className="flex items-center justify-between py-2">
                  <span className="text-gray-600 dark:text-gray-400">Closed</span>
                  <span className="px-3 py-1 bg-gray-100 text-gray-800 dark:bg-gray-700 dark:text-gray-400 rounded-full text-sm font-medium">
                    {ticketStats.closed}
                  </span>
                </div>
              </div>
            ) : (
              <p className="text-gray-500 text-center py-8">No ticket data available</p>
            )}
          </div>

          {/* Priority Overview */}
          <div className="bg-white dark:bg-gray-800 rounded-xl shadow-lg p-6">
            <h3 className="font-semibold text-gray-900 dark:text-white mb-4">
              Active Tickets by Priority
            </h3>
            {ticketStats?.by_priority ? (
              <div className="space-y-3">
                <div className="flex items-center justify-between py-2 border-b border-gray-100 dark:border-gray-700">
                  <span className="flex items-center gap-2">
                    <span className="w-3 h-3 bg-red-500 rounded-full"></span>
                    <span className="text-gray-600 dark:text-gray-400">Critical</span>
                  </span>
                  <span className="font-bold text-red-600">{ticketStats.by_priority.critical}</span>
                </div>
                <div className="flex items-center justify-between py-2 border-b border-gray-100 dark:border-gray-700">
                  <span className="flex items-center gap-2">
                    <span className="w-3 h-3 bg-orange-500 rounded-full"></span>
                    <span className="text-gray-600 dark:text-gray-400">High</span>
                  </span>
                  <span className="font-bold text-orange-600">{ticketStats.by_priority.high}</span>
                </div>
                <div className="flex items-center justify-between py-2 border-b border-gray-100 dark:border-gray-700">
                  <span className="flex items-center gap-2">
                    <span className="w-3 h-3 bg-yellow-500 rounded-full"></span>
                    <span className="text-gray-600 dark:text-gray-400">Medium</span>
                  </span>
                  <span className="font-bold text-yellow-600">{ticketStats.by_priority.medium}</span>
                </div>
                <div className="flex items-center justify-between py-2">
                  <span className="flex items-center gap-2">
                    <span className="w-3 h-3 bg-gray-400 rounded-full"></span>
                    <span className="text-gray-600 dark:text-gray-400">Low</span>
                  </span>
                  <span className="font-bold text-gray-600">{ticketStats.by_priority.low}</span>
                </div>
              </div>
            ) : (
              <p className="text-gray-500 text-center py-8">No priority data available</p>
            )}
          </div>
        </div>

        {/* Alerts Section */}
        {pendingFailures > 0 && (
          <div className="bg-red-50 dark:bg-red-900/20 border border-red-200 dark:border-red-800 rounded-xl p-6">
            <div className="flex items-center gap-4">
              <div className="p-3 bg-red-100 dark:bg-red-900/50 rounded-lg">
                <FaExclamationTriangle className="h-6 w-6 text-red-500" />
              </div>
              <div>
                <h3 className="font-semibold text-red-800 dark:text-red-200">
                  Attention Required
                </h3>
                <p className="text-red-700 dark:text-red-300">
                  There are {pendingFailures} pending provisioning failures requiring manual intervention.
                </p>
              </div>
            </div>
          </div>
        )}

        {/* Chat Statistics */}
        {chatStats && (
          <div className="bg-white dark:bg-gray-800 rounded-xl shadow-lg p-6">
            <h3 className="font-semibold text-gray-900 dark:text-white mb-4">
              Chat Support Metrics
            </h3>
            <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
              <div className="text-center p-4 bg-gray-50 dark:bg-gray-700/50 rounded-lg">
                <p className="text-2xl font-bold text-gray-900 dark:text-white">
                  {chatStats.total_messages}
                </p>
                <p className="text-sm text-gray-500">Total Messages</p>
              </div>
              <div className="text-center p-4 bg-gray-50 dark:bg-gray-700/50 rounded-lg">
                <p className="text-2xl font-bold text-purple-600">
                  {chatStats.ai_messages}
                </p>
                <p className="text-sm text-gray-500">AI Responses</p>
              </div>
              <div className="text-center p-4 bg-gray-50 dark:bg-gray-700/50 rounded-lg">
                <p className="text-2xl font-bold text-blue-600">
                  {chatStats.agent_messages}
                </p>
                <p className="text-sm text-gray-500">Agent Responses</p>
              </div>
              <div className="text-center p-4 bg-gray-50 dark:bg-gray-700/50 rounded-lg">
                <p className="text-2xl font-bold text-green-600">
                  {chatStats.today_sessions}
                </p>
                <p className="text-sm text-gray-500">Sessions Today</p>
              </div>
            </div>
          </div>
        )}

        {/* Payment Analytics */}
        <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
          {/* Invoice Status Breakdown */}
          {invoiceStats && (
            <div className="bg-white dark:bg-gray-800 rounded-xl shadow-lg p-6">
              <h3 className="font-semibold text-gray-900 dark:text-white mb-4">
                Invoice Status
              </h3>
              <div className="space-y-3">
                <div className="flex items-center justify-between py-2">
                  <span className="flex items-center gap-2">
                    <span className="w-3 h-3 bg-gray-400 rounded-full"></span>
                    <span className="text-gray-600 dark:text-gray-400">Draft</span>
                  </span>
                  <span className="font-medium">{invoiceStats.by_status.draft}</span>
                </div>
                <div className="flex items-center justify-between py-2">
                  <span className="flex items-center gap-2">
                    <span className="w-3 h-3 bg-blue-500 rounded-full"></span>
                    <span className="text-gray-600 dark:text-gray-400">Sent</span>
                  </span>
                  <span className="font-medium">{invoiceStats.by_status.sent}</span>
                </div>
                <div className="flex items-center justify-between py-2">
                  <span className="flex items-center gap-2">
                    <span className="w-3 h-3 bg-green-500 rounded-full"></span>
                    <span className="text-gray-600 dark:text-gray-400">Paid</span>
                  </span>
                  <span className="font-medium text-green-600">{invoiceStats.by_status.paid}</span>
                </div>
                <div className="flex items-center justify-between py-2">
                  <span className="flex items-center gap-2">
                    <span className="w-3 h-3 bg-red-500 rounded-full"></span>
                    <span className="text-gray-600 dark:text-gray-400">Overdue</span>
                  </span>
                  <span className="font-medium text-red-600">{invoiceStats.by_status.overdue}</span>
                </div>
                <div className="flex items-center justify-between py-2">
                  <span className="flex items-center gap-2">
                    <span className="w-3 h-3 bg-gray-300 rounded-full"></span>
                    <span className="text-gray-600 dark:text-gray-400">Cancelled</span>
                  </span>
                  <span className="font-medium">{invoiceStats.by_status.cancelled}</span>
                </div>
              </div>
            </div>
          )}

          {/* Payment Method Breakdown */}
          {paymentStats && (
            <div className="bg-white dark:bg-gray-800 rounded-xl shadow-lg p-6">
              <h3 className="font-semibold text-gray-900 dark:text-white mb-4">
                Payment Methods
              </h3>
              {Object.keys(paymentStats.by_method).length > 0 ? (
                <div className="space-y-3">
                  {Object.entries(paymentStats.by_method).map(([method, data]) => (
                    <div key={method} className="flex items-center justify-between py-2">
                      <span className="text-gray-600 dark:text-gray-400 capitalize">
                        {method.replace('_', ' ')}
                      </span>
                      <div className="text-right">
                        <span className="font-medium text-gray-900 dark:text-white">
                          {formatCurrency(data.total)}
                        </span>
                        <span className="text-sm text-gray-500 ml-2">
                          ({data.count} payments)
                        </span>
                      </div>
                    </div>
                  ))}
                </div>
              ) : (
                <p className="text-gray-500 text-center py-4">No payment data available</p>
              )}
              
              <div className="mt-4 pt-4 border-t border-gray-200 dark:border-gray-700">
                <div className="flex items-center justify-between">
                  <span className="font-medium text-gray-900 dark:text-white">Last 30 Days</span>
                  <span className="text-xl font-bold text-green-600">
                    {formatCurrency(paymentStats.last_30_days)}
                  </span>
                </div>
              </div>
            </div>
          )}
        </div>

        {/* Revenue Comparison */}
        {invoiceStats && (
          <div className="bg-white dark:bg-gray-800 rounded-xl shadow-lg p-6">
            <h3 className="font-semibold text-gray-900 dark:text-white mb-4">
              Revenue Comparison
            </h3>
            <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
              <div className="text-center p-6 bg-gray-50 dark:bg-gray-700/50 rounded-lg">
                <p className="text-sm text-gray-500 mb-2">Last Month</p>
                <p className="text-2xl font-bold text-gray-900 dark:text-white">
                  {formatCurrency(invoiceStats.revenue.last_month)}
                </p>
              </div>
              <div className="text-center p-6 bg-green-50 dark:bg-green-900/20 rounded-lg">
                <p className="text-sm text-gray-500 mb-2">This Month</p>
                <p className="text-2xl font-bold text-green-600">
                  {formatCurrency(invoiceStats.revenue.this_month)}
                </p>
                {invoiceStats.revenue.growth_percent !== 0 && (
                  <p className={`text-sm mt-1 ${invoiceStats.revenue.growth_percent > 0 ? 'text-green-600' : 'text-red-600'}`}>
                    {invoiceStats.revenue.growth_percent > 0 ? '+' : ''}{invoiceStats.revenue.growth_percent.toFixed(1)}% vs last month
                  </p>
                )}
              </div>
              <div className="text-center p-6 bg-blue-50 dark:bg-blue-900/20 rounded-lg">
                <p className="text-sm text-gray-500 mb-2">All Time</p>
                <p className="text-2xl font-bold text-blue-600">
                  {formatCurrency(invoiceStats.revenue.total)}
                </p>
              </div>
            </div>
          </div>
        )}
      </div>
    </AdminLayout>
  );
}
