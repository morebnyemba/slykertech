'use client';

import { useState, useEffect } from 'react';
import Link from 'next/link';
import { FaServer, FaPlus, FaSync, FaEye, FaClock, FaCheckCircle } from 'react-icons/fa';
import { useAuthStore } from '@/lib/stores/auth-store';
import { apiService } from '@/lib/api-service';

interface Service {
  id: number;
  name: string;
  description: string;
  status: string;
  created_at: string;
}

interface Subscription {
  id: number;
  service: number;
  service_name: string;
  status: string;
  start_date: string;
  next_billing_date: string;
  amount: string;
}

export default function ServicesPage() {
  const { isAuthenticated } = useAuthStore();
  const [subscriptions, setSubscriptions] = useState<Subscription[]>([]);
  const [availableServices, setAvailableServices] = useState<Service[]>([]);
  const [loading, setLoading] = useState(true);
  const [activeTab, setActiveTab] = useState<'active' | 'available'>('active');

  useEffect(() => {
    if (isAuthenticated) {
      loadData();
    }
  }, [isAuthenticated]);

  const loadData = async () => {
    setLoading(true);
    try {
      const [subsResponse, servicesResponse] = await Promise.all([
        apiService.getSubscriptions(),
        apiService.getServices(),
      ]);

      if (subsResponse.data) {
        setSubscriptions(subsResponse.data.results || subsResponse.data);
      }
      if (servicesResponse.data) {
        setAvailableServices(servicesResponse.data.results || servicesResponse.data);
      }
    } catch (error) {
      console.error('Failed to load services:', error);
    } finally {
      setLoading(false);
    }
  };

  const getStatusBadge = (status: string) => {
    const statusConfig: Record<string, { color: string; icon: JSX.Element }> = {
      active: { color: 'bg-green-100 text-green-800 dark:bg-green-900 dark:text-green-200', icon: <FaCheckCircle /> },
      pending: { color: 'bg-yellow-100 text-yellow-800 dark:bg-yellow-900 dark:text-yellow-200', icon: <FaClock /> },
      suspended: { color: 'bg-red-100 text-red-800 dark:bg-red-900 dark:text-red-200', icon: <FaClock /> },
    };

    const config = statusConfig[status.toLowerCase()] || statusConfig.pending;

    return (
      <span className={`inline-flex items-center gap-1 px-2 py-1 rounded-full text-xs font-medium ${config.color}`}>
        {config.icon}
        {status}
      </span>
    );
  };

  if (!isAuthenticated) {
    return null;
  }

  return (
    <div className="min-h-screen bg-gray-50 dark:bg-gray-900 pt-24 pb-12 px-4">
      <div className="max-w-7xl mx-auto">
        {/* Header */}
        <div className="mb-8">
          <div className="flex items-center justify-between">
            <div>
              <h1 className="text-3xl font-bold text-gray-900 dark:text-white mb-2 flex items-center gap-3">
                <FaServer className="text-blue-600" />
                My Services
              </h1>
              <p className="text-gray-600 dark:text-gray-400">
                Manage your active subscriptions and explore available services
              </p>
            </div>
            <div className="flex items-center gap-4">
              <button
                onClick={loadData}
                className="flex items-center gap-2 px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors"
              >
                <FaSync className={loading ? 'animate-spin' : ''} />
                Refresh
              </button>
            </div>
          </div>
        </div>

        {/* Tabs */}
        <div className="mb-6 border-b border-gray-200 dark:border-gray-700">
          <nav className="flex gap-8">
            <button
              onClick={() => setActiveTab('active')}
              className={`pb-4 px-1 border-b-2 font-medium transition-colors ${
                activeTab === 'active'
                  ? 'border-blue-600 text-blue-600'
                  : 'border-transparent text-gray-500 hover:text-gray-700 dark:text-gray-400 dark:hover:text-gray-300'
              }`}
            >
              Active Subscriptions ({subscriptions.length})
            </button>
            <button
              onClick={() => setActiveTab('available')}
              className={`pb-4 px-1 border-b-2 font-medium transition-colors ${
                activeTab === 'available'
                  ? 'border-blue-600 text-blue-600'
                  : 'border-transparent text-gray-500 hover:text-gray-700 dark:text-gray-400 dark:hover:text-gray-300'
              }`}
            >
              Available Services ({availableServices.length})
            </button>
          </nav>
        </div>

        {/* Content */}
        {activeTab === 'active' ? (
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
            {loading ? (
              <div className="col-span-full text-center py-12">
                <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-blue-600 mx-auto"></div>
              </div>
            ) : subscriptions.length === 0 ? (
              <div className="col-span-full text-center py-12 bg-white dark:bg-gray-800 rounded-lg">
                <FaServer className="text-5xl text-gray-400 mx-auto mb-4" />
                <p className="text-gray-600 dark:text-gray-400 mb-4">No active subscriptions</p>
                <button
                  onClick={() => setActiveTab('available')}
                  className="px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors"
                >
                  Browse Services
                </button>
              </div>
            ) : (
              subscriptions.map((subscription) => (
                <div
                  key={subscription.id}
                  className="bg-white dark:bg-gray-800 rounded-lg shadow-lg p-6 hover:shadow-xl transition-all duration-200"
                >
                  <div className="flex items-start justify-between mb-4">
                    <h3 className="text-lg font-semibold text-gray-900 dark:text-white">
                      {subscription.service_name || `Service #${subscription.service}`}
                    </h3>
                    {getStatusBadge(subscription.status)}
                  </div>
                  <div className="space-y-2 text-sm text-gray-600 dark:text-gray-400">
                    <p>
                      <strong>Started:</strong> {new Date(subscription.start_date).toLocaleDateString()}
                    </p>
                    <p>
                      <strong>Next Billing:</strong> {new Date(subscription.next_billing_date).toLocaleDateString()}
                    </p>
                    <p>
                      <strong>Amount:</strong> ${subscription.amount}
                    </p>
                  </div>
                  <div className="mt-4 flex gap-2">
                    <Link
                      href={`/dashboard/services/${subscription.id}`}
                      className="flex-1 flex items-center justify-center gap-2 px-4 py-2 bg-blue-100 text-blue-700 dark:bg-blue-900 dark:text-blue-200 rounded-lg hover:bg-blue-200 dark:hover:bg-blue-800 transition-colors"
                    >
                      <FaEye />
                      View Details
                    </Link>
                  </div>
                </div>
              ))
            )}
          </div>
        ) : (
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
            {loading ? (
              <div className="col-span-full text-center py-12">
                <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-blue-600 mx-auto"></div>
              </div>
            ) : availableServices.length === 0 ? (
              <div className="col-span-full text-center py-12 bg-white dark:bg-gray-800 rounded-lg">
                <FaServer className="text-5xl text-gray-400 mx-auto mb-4" />
                <p className="text-gray-600 dark:text-gray-400">No services available at the moment</p>
              </div>
            ) : (
              availableServices.map((service) => (
                <div
                  key={service.id}
                  className="bg-white dark:bg-gray-800 rounded-lg shadow-lg p-6 hover:shadow-xl transition-all duration-200"
                >
                  <h3 className="text-lg font-semibold text-gray-900 dark:text-white mb-2">
                    {service.name}
                  </h3>
                  <p className="text-sm text-gray-600 dark:text-gray-400 mb-4">
                    {service.description}
                  </p>
                  <button
                    className="w-full flex items-center justify-center gap-2 px-4 py-2 bg-green-600 text-white rounded-lg hover:bg-green-700 transition-colors"
                  >
                    <FaPlus />
                    Subscribe
                  </button>
                </div>
              ))
            )}
          </div>
        )}

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
    </div>
  );
}
