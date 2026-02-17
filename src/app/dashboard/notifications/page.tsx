'use client';

import { useState, useEffect } from 'react';
import Link from 'next/link';
import { FaBell, FaSync, FaCheckCircle, FaEnvelope, FaEnvelopeOpen } from 'react-icons/fa';
import { useAuthStore } from '@/lib/stores/auth-store';
import { apiService } from '@/lib/api-service';

interface Notification {
  id: number;
  title: string;
  message: string;
  type: string;
  is_read: boolean;
  created_at: string;
}

export default function NotificationsPage() {
  const { isAuthenticated } = useAuthStore();
  const [notifications, setNotifications] = useState<Notification[]>([]);
  const [loading, setLoading] = useState(true);
  const [filter, setFilter] = useState<'all' | 'unread' | 'read'>('all');

  useEffect(() => {
    if (isAuthenticated) {
      loadNotifications();
    }
  }, [isAuthenticated]);

  const loadNotifications = async () => {
    setLoading(true);
    try {
      const response = await apiService.getNotifications();
      if (response.data) {
        setNotifications(response.data.results || response.data);
      }
    } catch (error) {
      console.error('Failed to load notifications:', error);
    } finally {
      setLoading(false);
    }
  };

  const markAsRead = async (id: number) => {
    try {
      await apiService.markNotificationRead(id);
      setNotifications(notifications.map(n => 
        n.id === id ? { ...n, is_read: true } : n
      ));
    } catch (error) {
      console.error('Failed to mark notification as read:', error);
    }
  };

  const markAllAsRead = async () => {
    try {
      await apiService.markAllNotificationsRead();
      setNotifications(notifications.map(n => ({ ...n, is_read: true })));
    } catch (error) {
      console.error('Failed to mark all as read:', error);
    }
  };

  const filteredNotifications = notifications.filter(n => {
    if (filter === 'all') return true;
    if (filter === 'unread') return !n.is_read;
    return n.is_read;
  });

  const unreadCount = notifications.filter(n => !n.is_read).length;

  if (!isAuthenticated) {
    return null;
  }

  return (
    <div className="min-h-screen bg-gray-50 dark:bg-gray-900 pt-24 pb-12 px-4">
      <div className="max-w-4xl mx-auto">
        {/* Header */}
        <div className="mb-8">
          <div className="flex items-center justify-between">
            <div>
              <h1 className="text-3xl font-bold text-gray-900 dark:text-white mb-2 flex items-center gap-3">
                <FaBell className="text-orange-600" />
                Notifications
                {unreadCount > 0 && (
                  <span className="px-3 py-1 bg-red-500 text-white text-sm rounded-full">
                    {unreadCount}
                  </span>
                )}
              </h1>
              <p className="text-gray-600 dark:text-gray-400">
                Stay updated with your account activities
              </p>
            </div>
            <div className="flex items-center gap-4">
              {unreadCount > 0 && (
                <button
                  onClick={markAllAsRead}
                  className="flex items-center gap-2 px-4 py-2 bg-green-600 text-white rounded-lg hover:bg-green-700 transition-colors"
                >
                  <FaCheckCircle />
                  Mark All Read
                </button>
              )}
              <button
                onClick={loadNotifications}
                className="flex items-center gap-2 px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors"
              >
                <FaSync className={loading ? 'animate-spin' : ''} />
                Refresh
              </button>
            </div>
          </div>
        </div>

        {/* Filter Tabs */}
        <div className="mb-6 flex gap-4">
          {(['all', 'unread', 'read'] as const).map((status) => (
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
              {status === 'unread' && unreadCount > 0 && (
                <span className="ml-2 px-2 py-0.5 bg-red-500 text-white text-xs rounded-full">
                  {unreadCount}
                </span>
              )}
            </button>
          ))}
        </div>

        {/* Notifications List */}
        <div className="space-y-4">
          {loading ? (
            <div className="text-center py-12">
              <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-blue-600 mx-auto"></div>
            </div>
          ) : filteredNotifications.length === 0 ? (
            <div className="text-center py-12 bg-white dark:bg-gray-800 rounded-lg">
              <FaBell className="text-5xl text-gray-400 mx-auto mb-4" />
              <p className="text-gray-600 dark:text-gray-400">No notifications found</p>
            </div>
          ) : (
            filteredNotifications.map((notification) => (
              <div
                key={notification.id}
                className={`bg-white dark:bg-gray-800 rounded-lg shadow p-6 hover:shadow-lg transition-all duration-200 ${
                  !notification.is_read ? 'border-l-4 border-blue-600' : ''
                }`}
              >
                <div className="flex items-start justify-between">
                  <div className="flex items-start gap-4 flex-1">
                    <div className={`p-3 rounded-lg ${
                      !notification.is_read 
                        ? 'bg-blue-100 dark:bg-blue-900' 
                        : 'bg-gray-100 dark:bg-gray-700'
                    }`}>
                      {!notification.is_read ? (
                        <FaEnvelope className="text-blue-600 dark:text-blue-400" />
                      ) : (
                        <FaEnvelopeOpen className="text-gray-600 dark:text-gray-400" />
                      )}
                    </div>
                    <div className="flex-1">
                      <h3 className="text-lg font-semibold text-gray-900 dark:text-white mb-2">
                        {notification.title}
                      </h3>
                      <p className="text-gray-600 dark:text-gray-400 mb-2">
                        {notification.message}
                      </p>
                      <p className="text-sm text-gray-500 dark:text-gray-500">
                        {new Date(notification.created_at).toLocaleString()}
                      </p>
                    </div>
                  </div>
                  <div className="flex gap-2">
                    {!notification.is_read && (
                      <button
                        onClick={() => markAsRead(notification.id)}
                        className="text-blue-600 hover:text-blue-800 dark:text-blue-400 dark:hover:text-blue-300"
                        title="Mark as read"
                      >
                        <FaCheckCircle />
                      </button>
                    )}
                  </div>
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
    </div>
  );
}
