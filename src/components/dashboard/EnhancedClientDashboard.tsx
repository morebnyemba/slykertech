'use client';

import { useState, useEffect, useCallback } from 'react';
import { useRouter } from 'next/navigation';
import Link from 'next/link';
import { 
  FaServer, FaFileInvoiceDollar, FaProjectDiagram, 
  FaBell, FaWallet, FaSync, FaWifi, FaGift, FaGlobe, FaUser
} from 'react-icons/fa';
import { useAuthStore } from '@/lib/stores/auth-store';
import { apiService } from '@/lib/api-service';
import { useAnalyticsWebSocket } from '@/lib/hooks/use-websocket';

interface DashboardStats {
  subscriptions: number;
  invoices: {
    total: number;
    paid: number;
    pending: number;
  };
  walletBalance: number;
  notifications: number;
}

export default function EnhancedClientDashboard() {
  const router = useRouter();
  const { isAuthenticated, user, isLoading: authLoading } = useAuthStore();
  const [stats, setStats] = useState<DashboardStats>({
    subscriptions: 0,
    invoices: {
      total: 0,
      paid: 0,
      pending: 0,
    },
    walletBalance: 0,
    notifications: 0,
  });
  const [loading, setLoading] = useState(true);

  // Fetch dashboard stats via REST API
  const fetchDashboardStats = useCallback(async () => {
    try {
      setLoading(true);
      
      // Fetch all required data in parallel
      const [subsResponse, invoicesResponse, walletsResponse, notificationsResponse] = await Promise.all([
        apiService.getSubscriptions(),
        apiService.getInvoices(),
        apiService.getWallets(),
        apiService.getUnreadCount(),
      ]);

      const subscriptions = subsResponse.data?.results || subsResponse.data || [];
      const invoices = invoicesResponse.data?.results || invoicesResponse.data || [];
      const wallets = walletsResponse.data?.results || walletsResponse.data || [];
      const unreadCount = notificationsResponse.data?.count || 0;

      // Calculate invoice stats
      const paidInvoices = invoices.filter((inv: { status: string }) => 
        inv.status?.toLowerCase() === 'paid'
      ).length;
      const pendingInvoices = invoices.filter((inv: { status: string }) => 
        inv.status?.toLowerCase() === 'pending'
      ).length;

      // Get wallet balance
      const walletBalance = wallets.length > 0 
        ? parseFloat(wallets[0]?.balance || '0') 
        : 0;

      setStats({
        subscriptions: Array.isArray(subscriptions) ? subscriptions.length : 0,
        invoices: {
          total: Array.isArray(invoices) ? invoices.length : 0,
          paid: paidInvoices,
          pending: pendingInvoices,
        },
        walletBalance,
        notifications: unreadCount,
      });
    } catch (error) {
      console.error('Failed to fetch dashboard stats:', error);
    } finally {
      setLoading(false);
    }
  }, []);

  // WebSocket connection for real-time analytics (optional enhancement)
  const { isConnected, sendMessage } = useAnalyticsWebSocket((data: unknown) => {
    if (data && typeof data === 'object' && 'type' in data && data.type === 'analytics_update') {
      const analyticsData = (data as { data: DashboardStats }).data;
      setStats(prevStats => ({ ...prevStats, ...analyticsData }));
    }
  });

  useEffect(() => {
    // Redirect to login if not authenticated
    if (!authLoading && !isAuthenticated) {
      router.push('/login');
    }
  }, [isAuthenticated, authLoading, router]);

  useEffect(() => {
    // Fetch data when authenticated
    if (isAuthenticated && !authLoading) {
      fetchDashboardStats();
    }
  }, [isAuthenticated, authLoading, fetchDashboardStats]);

  useEffect(() => {
    // Request WebSocket update when connected (for real-time updates)
    if (isConnected) {
      sendMessage({ type: 'request_update' });
    }
  }, [isConnected, sendMessage]);

  const handleRefresh = () => {
    fetchDashboardStats();
    if (isConnected) {
      sendMessage({ type: 'request_update' });
    }
  };

  if (authLoading || !isAuthenticated) {
    return (
      <div className="min-h-screen flex items-center justify-center bg-gray-50 dark:bg-gray-900">
        <div className="text-center">
          <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-blue-600 mx-auto"></div>
          <p className="mt-4 text-gray-600 dark:text-gray-400">Loading...</p>
        </div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-gray-50 dark:bg-gray-900 pt-24 pb-12 px-4">
      <div className="max-w-7xl mx-auto">
        {/* Header */}
        <div className="mb-8">
          <div className="flex items-center justify-between">
            <div>
              <h1 className="text-3xl font-bold text-gray-900 dark:text-white mb-2">
                Welcome back, {user?.first_name || 'User'}!
              </h1>
              <p className="text-gray-600 dark:text-gray-400">
                Here&apos;s what&apos;s happening with your account
              </p>
            </div>
            <div className="flex items-center gap-4">
              {/* WebSocket Status Indicator */}
              <div className="flex items-center gap-2 px-3 py-2 bg-white dark:bg-gray-800 rounded-lg shadow">
                <FaWifi className={`${isConnected ? 'text-green-500' : 'text-gray-400'}`} />
                <span className="text-sm text-gray-600 dark:text-gray-400">
                  {isConnected ? 'Live' : 'Standard'}
                </span>
              </div>
              <button
                onClick={handleRefresh}
                className="flex items-center gap-2 px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors"
              >
                <FaSync className={loading ? 'animate-spin' : ''} />
                Refresh
              </button>
            </div>
          </div>
        </div>

        {/* Stats Grid */}
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-8">
          {/* Active Subscriptions */}
          <div className="bg-white dark:bg-gray-800 rounded-lg shadow-lg p-6 border-l-4 border-blue-500">
            <div className="flex items-center justify-between mb-4">
              <div className="p-3 bg-blue-100 dark:bg-blue-900 rounded-lg">
                <FaServer className="text-2xl text-blue-600 dark:text-blue-400" />
              </div>
            </div>
            <h3 className="text-gray-600 dark:text-gray-400 text-sm font-medium mb-1">
              Active Subscriptions
            </h3>
            <p className="text-3xl font-bold text-gray-900 dark:text-white">
              {loading ? '...' : stats.subscriptions}
            </p>
          </div>

          {/* Total Invoices */}
          <div className="bg-white dark:bg-gray-800 rounded-lg shadow-lg p-6 border-l-4 border-purple-500">
            <div className="flex items-center justify-between mb-4">
              <div className="p-3 bg-purple-100 dark:bg-purple-900 rounded-lg">
                <FaFileInvoiceDollar className="text-2xl text-purple-600 dark:text-purple-400" />
              </div>
            </div>
            <h3 className="text-gray-600 dark:text-gray-400 text-sm font-medium mb-1">
              Total Invoices
            </h3>
            <p className="text-3xl font-bold text-gray-900 dark:text-white">
              {loading ? '...' : stats.invoices.total}
            </p>
          </div>

          {/* Wallet Balance */}
          <div className="bg-white dark:bg-gray-800 rounded-lg shadow-lg p-6 border-l-4 border-green-500">
            <div className="flex items-center justify-between mb-4">
              <div className="p-3 bg-green-100 dark:bg-green-900 rounded-lg">
                <FaWallet className="text-2xl text-green-600 dark:text-green-400" />
              </div>
            </div>
            <h3 className="text-gray-600 dark:text-gray-400 text-sm font-medium mb-1">
              Wallet Balance
            </h3>
            <p className="text-3xl font-bold text-gray-900 dark:text-white">
              {loading ? '...' : `$${stats.walletBalance.toFixed(2)}`}
            </p>
          </div>

          {/* Pending Invoices */}
          <div className="bg-white dark:bg-gray-800 rounded-lg shadow-lg p-6 border-l-4 border-orange-500">
            <div className="flex items-center justify-between mb-4">
              <div className="p-3 bg-orange-100 dark:bg-orange-900 rounded-lg">
                <FaBell className="text-2xl text-orange-600 dark:text-orange-400" />
              </div>
            </div>
            <h3 className="text-gray-600 dark:text-gray-400 text-sm font-medium mb-1">
              Pending Invoices
            </h3>
            <p className="text-3xl font-bold text-gray-900 dark:text-white">
              {loading ? '...' : stats.invoices.pending}
            </p>
          </div>
        </div>

        {/* Quick Actions */}
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6 mb-8">
          <Link
            href="/dashboard/services"
            className="bg-white dark:bg-gray-800 rounded-lg shadow-lg p-6 hover:shadow-xl transition-all duration-200 transform hover:-translate-y-1"
          >
            <div className="flex items-center gap-4">
              <div className="p-3 bg-blue-100 dark:bg-blue-900 rounded-lg">
                <FaServer className="text-2xl text-blue-600 dark:text-blue-400" />
              </div>
              <div>
                <h3 className="text-lg font-semibold text-gray-900 dark:text-white">
                  Manage Services
                </h3>
                <p className="text-sm text-gray-600 dark:text-gray-400">
                  View and manage your active services
                </p>
              </div>
            </div>
          </Link>

          <Link
            href="/dashboard/invoices"
            className="bg-white dark:bg-gray-800 rounded-lg shadow-lg p-6 hover:shadow-xl transition-all duration-200 transform hover:-translate-y-1"
          >
            <div className="flex items-center gap-4">
              <div className="p-3 bg-purple-100 dark:bg-purple-900 rounded-lg">
                <FaFileInvoiceDollar className="text-2xl text-purple-600 dark:text-purple-400" />
              </div>
              <div>
                <h3 className="text-lg font-semibold text-gray-900 dark:text-white">
                  View Invoices
                </h3>
                <p className="text-sm text-gray-600 dark:text-gray-400">
                  Check your billing and invoices
                </p>
              </div>
            </div>
          </Link>

          <Link
            href="/dashboard/projects"
            className="bg-white dark:bg-gray-800 rounded-lg shadow-lg p-6 hover:shadow-xl transition-all duration-200 transform hover:-translate-y-1"
          >
            <div className="flex items-center gap-4">
              <div className="p-3 bg-green-100 dark:bg-green-900 rounded-lg">
                <FaProjectDiagram className="text-2xl text-green-600 dark:text-green-400" />
              </div>
              <div>
                <h3 className="text-lg font-semibold text-gray-900 dark:text-white">
                  My Projects
                </h3>
                <p className="text-sm text-gray-600 dark:text-gray-400">
                  Track your project progress
                </p>
              </div>
            </div>
          </Link>

          <Link
            href="/dashboard/wallet"
            className="bg-white dark:bg-gray-800 rounded-lg shadow-lg p-6 hover:shadow-xl transition-all duration-200 transform hover:-translate-y-1"
          >
            <div className="flex items-center gap-4">
              <div className="p-3 bg-green-100 dark:bg-green-900 rounded-lg">
                <FaWallet className="text-2xl text-green-600 dark:text-green-400" />
              </div>
              <div>
                <h3 className="text-lg font-semibold text-gray-900 dark:text-white">
                  My Wallet
                </h3>
                <p className="text-sm text-gray-600 dark:text-gray-400">
                  Manage your balance
                </p>
              </div>
            </div>
          </Link>

          <Link
            href="/dashboard/referrals"
            className="bg-white dark:bg-gray-800 rounded-lg shadow-lg p-6 hover:shadow-xl transition-all duration-200 transform hover:-translate-y-1"
          >
            <div className="flex items-center gap-4">
              <div className="p-3 bg-yellow-100 dark:bg-yellow-900 rounded-lg">
                <FaGift className="text-2xl text-yellow-600 dark:text-yellow-400" />
              </div>
              <div>
                <h3 className="text-lg font-semibold text-gray-900 dark:text-white">
                  Referral Program
                </h3>
                <p className="text-sm text-gray-600 dark:text-gray-400">
                  Invite friends & earn rewards
                </p>
              </div>
            </div>
          </Link>

          <Link
            href="/dashboard/dns"
            className="bg-white dark:bg-gray-800 rounded-lg shadow-lg p-6 hover:shadow-xl transition-all duration-200 transform hover:-translate-y-1"
          >
            <div className="flex items-center gap-4">
              <div className="p-3 bg-blue-100 dark:bg-blue-900 rounded-lg">
                <FaGlobe className="text-2xl text-blue-600 dark:text-blue-400" />
              </div>
              <div>
                <h3 className="text-lg font-semibold text-gray-900 dark:text-white">
                  DNS Panel
                </h3>
                <p className="text-sm text-gray-600 dark:text-gray-400">
                  Manage DNS records
                </p>
              </div>
            </div>
          </Link>

          <Link
            href="/dashboard/notifications"
            className="bg-white dark:bg-gray-800 rounded-lg shadow-lg p-6 hover:shadow-xl transition-all duration-200 transform hover:-translate-y-1"
          >
            <div className="flex items-center gap-4">
              <div className="p-3 bg-orange-100 dark:bg-orange-900 rounded-lg">
                <FaBell className="text-2xl text-orange-600 dark:text-orange-400" />
              </div>
              <div>
                <h3 className="text-lg font-semibold text-gray-900 dark:text-white">
                  Notifications
                  {stats.notifications > 0 && (
                    <span className="ml-2 px-2 py-0.5 text-xs bg-red-500 text-white rounded-full">
                      {stats.notifications}
                    </span>
                  )}
                </h3>
                <p className="text-sm text-gray-600 dark:text-gray-400">
                  View your notifications
                </p>
              </div>
            </div>
          </Link>

          <Link
            href="/dashboard/profile"
            className="bg-white dark:bg-gray-800 rounded-lg shadow-lg p-6 hover:shadow-xl transition-all duration-200 transform hover:-translate-y-1"
          >
            <div className="flex items-center gap-4">
              <div className="p-3 bg-gray-100 dark:bg-gray-700 rounded-lg">
                <FaUser className="text-2xl text-gray-600 dark:text-gray-400" />
              </div>
              <div>
                <h3 className="text-lg font-semibold text-gray-900 dark:text-white">
                  My Profile
                </h3>
                <p className="text-sm text-gray-600 dark:text-gray-400">
                  Update your account settings
                </p>
              </div>
            </div>
          </Link>
        </div>

        {/* Account Overview */}
        <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
          {/* Recent Activity */}
          <div className="bg-white dark:bg-gray-800 rounded-lg shadow-lg p-6">
            <h2 className="text-xl font-bold text-gray-900 dark:text-white mb-4">
              Quick Overview
            </h2>
            <div className="space-y-4">
              <div className="flex items-center justify-between py-3 border-b border-gray-100 dark:border-gray-700">
                <div className="flex items-center gap-3">
                  <FaServer className="text-blue-500" />
                  <span className="text-gray-700 dark:text-gray-300">Active Services</span>
                </div>
                <span className="font-semibold text-gray-900 dark:text-white">
                  {loading ? '...' : stats.subscriptions}
                </span>
              </div>
              <div className="flex items-center justify-between py-3 border-b border-gray-100 dark:border-gray-700">
                <div className="flex items-center gap-3">
                  <FaFileInvoiceDollar className="text-green-500" />
                  <span className="text-gray-700 dark:text-gray-300">Paid Invoices</span>
                </div>
                <span className="font-semibold text-green-600 dark:text-green-400">
                  {loading ? '...' : stats.invoices.paid}
                </span>
              </div>
              <div className="flex items-center justify-between py-3 border-b border-gray-100 dark:border-gray-700">
                <div className="flex items-center gap-3">
                  <FaFileInvoiceDollar className="text-yellow-500" />
                  <span className="text-gray-700 dark:text-gray-300">Pending Invoices</span>
                </div>
                <span className="font-semibold text-yellow-600 dark:text-yellow-400">
                  {loading ? '...' : stats.invoices.pending}
                </span>
              </div>
              <div className="flex items-center justify-between py-3">
                <div className="flex items-center gap-3">
                  <FaWallet className="text-green-500" />
                  <span className="text-gray-700 dark:text-gray-300">Wallet Balance</span>
                </div>
                <span className="font-semibold text-gray-900 dark:text-white">
                  {loading ? '...' : `$${stats.walletBalance.toFixed(2)}`}
                </span>
              </div>
            </div>
          </div>

          {/* Help & Support */}
          <div className="bg-white dark:bg-gray-800 rounded-lg shadow-lg p-6">
            <h2 className="text-xl font-bold text-gray-900 dark:text-white mb-4">
              Need Help?
            </h2>
            <div className="space-y-4">
              <p className="text-gray-600 dark:text-gray-400">
                Our support team is here to help you with any questions or issues.
              </p>
              <div className="flex flex-col gap-3">
                <Link
                  href="/support"
                  className="inline-flex items-center justify-center px-4 py-3 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors"
                >
                  Contact Support
                </Link>
                <Link
                  href="/contact"
                  className="inline-flex items-center justify-center px-4 py-3 border border-gray-300 dark:border-gray-600 text-gray-700 dark:text-gray-300 rounded-lg hover:bg-gray-100 dark:hover:bg-gray-700 transition-colors"
                >
                  Send Us a Message
                </Link>
              </div>
              <div className="pt-4 border-t border-gray-100 dark:border-gray-700">
                <p className="text-sm text-gray-500 dark:text-gray-400">
                  Real-time updates: {isConnected ? (
                    <span className="text-green-500 font-medium">Active ✓</span>
                  ) : (
                    <span className="text-gray-400">Using standard updates</span>
                  )}
                </p>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
