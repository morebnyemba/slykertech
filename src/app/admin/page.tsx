'use client';

import { useState, useEffect, useCallback } from 'react';
import Link from 'next/link';
import AdminLayout from '@/components/admin/AdminLayout';
import { apiService } from '@/lib/api-service';
import { 
  FaExclamationTriangle, FaServer, FaUsers, FaCheckCircle, 
  FaSpinner, FaArrowRight
} from 'react-icons/fa';

interface DashboardStats {
  pendingFailures: number;
  totalSubscriptions: number;
  activeSubscriptions: number;
  totalClients: number;
}

export default function AdminDashboard() {
  const [stats, setStats] = useState<DashboardStats>({
    pendingFailures: 0,
    totalSubscriptions: 0,
    activeSubscriptions: 0,
    totalClients: 0,
  });
  const [loading, setLoading] = useState(true);

  const fetchStats = useCallback(async () => {
    // Only fetch stats if user is authenticated with a valid token
    const { useAuthStore } = await import('@/lib/stores/auth-store');
    const { isAuthenticated, token } = useAuthStore.getState();
    if (!isAuthenticated || !token) {
      setLoading(false);
      return;
    }

    try {
      const [failuresRes, subscriptionsRes, clientsRes] = await Promise.all([
        apiService.getPendingFailuresCount(),
        apiService.getAllSubscriptions(),
        apiService.getAllClients(),
      ]);

      const subscriptions = Array.isArray(subscriptionsRes.data) ? subscriptionsRes.data : [];
      const clients = Array.isArray(clientsRes.data) ? clientsRes.data : [];

      setStats({
        pendingFailures: (failuresRes.data as { pending_count: number })?.pending_count || 0,
        totalSubscriptions: subscriptions.length,
        activeSubscriptions: subscriptions.filter((s: { status: string }) => s.status === 'active').length,
        totalClients: clients.length,
      });
    } catch (error) {
      console.error('Failed to fetch stats:', error);
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => {
    fetchStats();
  }, [fetchStats]);

  const statCards = [
    {
      title: 'Pending Failures',
      value: stats.pendingFailures,
      icon: FaExclamationTriangle,
      color: stats.pendingFailures > 0 ? 'text-red-500' : 'text-green-500',
      bgColor: stats.pendingFailures > 0 ? 'bg-red-100 dark:bg-red-900/30' : 'bg-green-100 dark:bg-green-900/30',
      link: '/admin/provisioning-failures',
      description: stats.pendingFailures > 0 ? 'Requires attention' : 'All clear',
    },
    {
      title: 'Active Subscriptions',
      value: stats.activeSubscriptions,
      icon: FaCheckCircle,
      color: 'text-green-500',
      bgColor: 'bg-green-100 dark:bg-green-900/30',
      link: '/admin/subscriptions',
      description: `of ${stats.totalSubscriptions} total`,
    },
    {
      title: 'Total Clients',
      value: stats.totalClients,
      icon: FaUsers,
      color: 'text-blue-500',
      bgColor: 'bg-blue-100 dark:bg-blue-900/30',
      link: '/admin/clients',
      description: 'Registered clients',
    },
    {
      title: 'Services Active',
      value: stats.activeSubscriptions,
      icon: FaServer,
      color: 'text-purple-500',
      bgColor: 'bg-purple-100 dark:bg-purple-900/30',
      link: '/admin/subscriptions',
      description: 'Running services',
    },
  ];

  return (
    <AdminLayout>
      <div className="space-y-6">
        {/* Header */}
        <div>
          <h1 className="text-3xl font-bold text-gray-900 dark:text-white">
            Admin Dashboard
          </h1>
          <p className="text-gray-500 dark:text-gray-400 mt-1">
            Overview of your service management platform
          </p>
        </div>

        {/* Stats Grid */}
        {loading ? (
          <div className="flex items-center justify-center py-12">
            <FaSpinner className="animate-spin h-8 w-8 text-blue-600" />
          </div>
        ) : (
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6">
            {statCards.map((stat) => (
              <Link
                key={stat.title}
                href={stat.link}
                className="bg-white dark:bg-gray-800 rounded-xl shadow-lg p-6 hover:shadow-xl transition-shadow"
              >
                <div className="flex items-center justify-between mb-4">
                  <div className={`p-3 rounded-lg ${stat.bgColor}`}>
                    <stat.icon className={`h-6 w-6 ${stat.color}`} />
                  </div>
                  <FaArrowRight className="h-4 w-4 text-gray-400" />
                </div>
                <h3 className="text-sm font-medium text-gray-500 dark:text-gray-400">
                  {stat.title}
                </h3>
                <p className="text-3xl font-bold text-gray-900 dark:text-white mt-1">
                  {stat.value}
                </p>
                <p className="text-sm text-gray-500 dark:text-gray-400 mt-1">
                  {stat.description}
                </p>
              </Link>
            ))}
          </div>
        )}

        {/* Quick Actions */}
        <div className="bg-white dark:bg-gray-800 rounded-xl shadow-lg p-6">
          <h2 className="text-lg font-semibold text-gray-900 dark:text-white mb-4">
            Quick Actions
          </h2>
          <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
            <Link
              href="/admin/provisioning-failures"
              className="flex items-center gap-3 p-4 bg-gray-50 dark:bg-gray-700 rounded-lg hover:bg-gray-100 dark:hover:bg-gray-600 transition-colors"
            >
              <FaExclamationTriangle className="h-5 w-5 text-yellow-500" />
              <span className="font-medium text-gray-700 dark:text-gray-300">
                Review Provisioning Failures
              </span>
            </Link>
            <Link
              href="/admin/subscriptions"
              className="flex items-center gap-3 p-4 bg-gray-50 dark:bg-gray-700 rounded-lg hover:bg-gray-100 dark:hover:bg-gray-600 transition-colors"
            >
              <FaServer className="h-5 w-5 text-blue-500" />
              <span className="font-medium text-gray-700 dark:text-gray-300">
                Manage Subscriptions
              </span>
            </Link>
            <Link
              href="/admin/clients"
              className="flex items-center gap-3 p-4 bg-gray-50 dark:bg-gray-700 rounded-lg hover:bg-gray-100 dark:hover:bg-gray-600 transition-colors"
            >
              <FaUsers className="h-5 w-5 text-green-500" />
              <span className="font-medium text-gray-700 dark:text-gray-300">
                View All Clients
              </span>
            </Link>
          </div>
        </div>

        {/* Alert for pending failures */}
        {stats.pendingFailures > 0 && (
          <div className="bg-red-50 dark:bg-red-900/20 border border-red-200 dark:border-red-800 rounded-xl p-6">
            <div className="flex items-start gap-4">
              <div className="p-2 bg-red-100 dark:bg-red-900/50 rounded-lg">
                <FaExclamationTriangle className="h-6 w-6 text-red-500" />
              </div>
              <div className="flex-1">
                <h3 className="font-semibold text-red-800 dark:text-red-200">
                  Attention Required
                </h3>
                <p className="text-red-700 dark:text-red-300 mt-1">
                  You have {stats.pendingFailures} pending provisioning failure{stats.pendingFailures > 1 ? 's' : ''} that may require manual intervention.
                </p>
                <Link
                  href="/admin/provisioning-failures"
                  className="inline-flex items-center gap-2 mt-3 text-red-600 dark:text-red-400 font-medium hover:underline"
                >
                  Review failures
                  <FaArrowRight className="h-3 w-3" />
                </Link>
              </div>
            </div>
          </div>
        )}
      </div>
    </AdminLayout>
  );
}
