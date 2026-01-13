'use client';

import { useState, useEffect } from 'react';
import { useRouter } from 'next/navigation';
import Link from 'next/link';
import { 
  FaServer, FaFileInvoiceDollar, FaProjectDiagram, 
  FaBell, FaChartLine, FaCog, FaSync, FaWifi 
} from 'react-icons/fa';
import { useAuthStore } from '@/lib/stores/auth-store';
import { useAnalyticsWebSocket } from '@/lib/hooks/use-websocket';

interface DashboardStats {
  subscriptions: number;
  invoices: {
    total: number;
    paid: number;
    pending: number;
  };
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
  });
  const [loading, setLoading] = useState(true);

  // WebSocket connection for real-time analytics
  const { isConnected, lastMessage, sendMessage } = useAnalyticsWebSocket((data: unknown) => {
    if (data && typeof data === 'object' && 'type' in data && data.type === 'analytics_update') {
      const analyticsData = (data as { data: DashboardStats }).data;
      setStats(analyticsData);
      setLoading(false);
    }
  });

  useEffect(() => {
    // Redirect to login if not authenticated
    if (!authLoading && !isAuthenticated) {
      router.push('/login');
    }
  }, [isAuthenticated, authLoading, router]);

  useEffect(() => {
    // Request initial data when connected
    if (isConnected) {
      sendMessage({ type: 'request_update' });
    }
  }, [isConnected, sendMessage]);

  const handleRefresh = () => {
    setLoading(true);
    sendMessage({ type: 'request_update' });
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
                Here's what's happening with your account
              </p>
            </div>
            <div className="flex items-center gap-4">
              {/* WebSocket Status Indicator */}
              <div className="flex items-center gap-2 px-3 py-2 bg-white dark:bg-gray-800 rounded-lg shadow">
                <FaWifi className={`${isConnected ? 'text-green-500' : 'text-red-500'}`} />
                <span className="text-sm text-gray-600 dark:text-gray-400">
                  {isConnected ? 'Live' : 'Offline'}
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
              {stats.subscriptions}
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
              {stats.invoices.total}
            </p>
          </div>

          {/* Paid Invoices */}
          <div className="bg-white dark:bg-gray-800 rounded-lg shadow-lg p-6 border-l-4 border-green-500">
            <div className="flex items-center justify-between mb-4">
              <div className="p-3 bg-green-100 dark:bg-green-900 rounded-lg">
                <FaChartLine className="text-2xl text-green-600 dark:text-green-400" />
              </div>
            </div>
            <h3 className="text-gray-600 dark:text-gray-400 text-sm font-medium mb-1">
              Paid Invoices
            </h3>
            <p className="text-3xl font-bold text-gray-900 dark:text-white">
              {stats.invoices.paid}
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
              {stats.invoices.pending}
            </p>
          </div>
        </div>

        {/* Quick Actions */}
        <div className="grid grid-cols-1 md:grid-cols-3 gap-6 mb-8">
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
        </div>

        {/* Recent Activity */}
        <div className="bg-white dark:bg-gray-800 rounded-lg shadow-lg p-6">
          <h2 className="text-xl font-bold text-gray-900 dark:text-white mb-4">
            Recent Activity
          </h2>
          <div className="text-center py-8 text-gray-500 dark:text-gray-400">
            <FaChartLine className="text-4xl mx-auto mb-2 opacity-50" />
            <p>Activity tracking coming soon...</p>
            <p className="text-sm mt-2">
              Real-time updates via WebSocket are {isConnected ? 'active' : 'inactive'}
            </p>
          </div>
        </div>
      </div>
    </div>
  );
}
