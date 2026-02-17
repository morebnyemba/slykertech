'use client';

import { useState, useEffect } from 'react';
import Link from 'next/link';
import { FaFileInvoiceDollar, FaSync, FaEye, FaCreditCard, FaCheckCircle, FaClock, FaExclamationCircle } from 'react-icons/fa';
import { useAuthStore } from '@/lib/stores/auth-store';
import { apiService } from '@/lib/api-service';

interface Invoice {
  id: number;
  invoice_number: string;
  amount: string;
  status: string;
  due_date: string;
  created_at: string;
  client: number;
}

export default function InvoicesPage() {
  const { isAuthenticated } = useAuthStore();
  const [invoices, setInvoices] = useState<Invoice[]>([]);
  const [loading, setLoading] = useState(true);
  const [filter, setFilter] = useState<'all' | 'paid' | 'pending' | 'overdue'>('all');

  useEffect(() => {
    if (isAuthenticated) {
      loadInvoices();
    }
  }, [isAuthenticated]);

  const loadInvoices = async () => {
    setLoading(true);
    try {
      const response = await apiService.getInvoices();
      if (response.data) {
        setInvoices(response.data.results || response.data);
      }
    } catch (error) {
      console.error('Failed to load invoices:', error);
    } finally {
      setLoading(false);
    }
  };

  const getStatusIcon = (status: string) => {
    const statusIcons: Record<string, JSX.Element> = {
      paid: <FaCheckCircle className="text-green-500" />,
      pending: <FaClock className="text-yellow-500" />,
      overdue: <FaExclamationCircle className="text-red-500" />,
    };
    return statusIcons[status.toLowerCase()] || <FaClock className="text-gray-500" />;
  };

  const getStatusBadge = (status: string) => {
    const statusConfig: Record<string, string> = {
      paid: 'bg-green-100 text-green-800 dark:bg-green-900 dark:text-green-200',
      pending: 'bg-yellow-100 text-yellow-800 dark:bg-yellow-900 dark:text-yellow-200',
      overdue: 'bg-red-100 text-red-800 dark:bg-red-900 dark:text-red-200',
    };

    const colorClass = statusConfig[status.toLowerCase()] || 'bg-gray-100 text-gray-800 dark:bg-gray-700 dark:text-gray-200';

    return (
      <span className={`inline-flex items-center gap-1 px-3 py-1 rounded-full text-xs font-medium ${colorClass}`}>
        {getStatusIcon(status)}
        {status}
      </span>
    );
  };

  const filteredInvoices = invoices.filter(invoice => {
    if (filter === 'all') return true;
    return invoice.status.toLowerCase() === filter;
  });

  const totalAmount = invoices.reduce((sum, inv) => sum + parseFloat(inv.amount), 0);
  const paidAmount = invoices.filter(inv => inv.status.toLowerCase() === 'paid').reduce((sum, inv) => sum + parseFloat(inv.amount), 0);
  const pendingAmount = totalAmount - paidAmount;

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
                <FaFileInvoiceDollar className="text-purple-600" />
                Invoices & Billing
              </h1>
              <p className="text-gray-600 dark:text-gray-400">
                View and manage your invoices
              </p>
            </div>
            <button
              onClick={loadInvoices}
              className="flex items-center gap-2 px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors"
            >
              <FaSync className={loading ? 'animate-spin' : ''} />
              Refresh
            </button>
          </div>
        </div>

        {/* Summary Cards */}
        <div className="grid grid-cols-1 md:grid-cols-3 gap-6 mb-8">
          <div className="bg-white dark:bg-gray-800 rounded-lg shadow-lg p-6">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-gray-600 dark:text-gray-400 text-sm mb-1">Total Invoiced</p>
                <p className="text-2xl font-bold text-gray-900 dark:text-white">${totalAmount.toFixed(2)}</p>
              </div>
              <div className="p-3 bg-blue-100 dark:bg-blue-900 rounded-lg">
                <FaFileInvoiceDollar className="text-2xl text-blue-600 dark:text-blue-400" />
              </div>
            </div>
          </div>

          <div className="bg-white dark:bg-gray-800 rounded-lg shadow-lg p-6">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-gray-600 dark:text-gray-400 text-sm mb-1">Paid</p>
                <p className="text-2xl font-bold text-green-600 dark:text-green-400">${paidAmount.toFixed(2)}</p>
              </div>
              <div className="p-3 bg-green-100 dark:bg-green-900 rounded-lg">
                <FaCheckCircle className="text-2xl text-green-600 dark:text-green-400" />
              </div>
            </div>
          </div>

          <div className="bg-white dark:bg-gray-800 rounded-lg shadow-lg p-6">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-gray-600 dark:text-gray-400 text-sm mb-1">Pending</p>
                <p className="text-2xl font-bold text-yellow-600 dark:text-yellow-400">${pendingAmount.toFixed(2)}</p>
              </div>
              <div className="p-3 bg-yellow-100 dark:bg-yellow-900 rounded-lg">
                <FaClock className="text-2xl text-yellow-600 dark:text-yellow-400" />
              </div>
            </div>
          </div>
        </div>

        {/* Filter Tabs */}
        <div className="mb-6 flex gap-4">
          {(['all', 'paid', 'pending', 'overdue'] as const).map((status) => (
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
            </button>
          ))}
        </div>

        {/* Invoices Table */}
        <div className="bg-white dark:bg-gray-800 rounded-lg shadow-lg overflow-hidden">
          <div className="overflow-x-auto">
            <table className="w-full">
              <thead className="bg-gray-50 dark:bg-gray-700">
                <tr>
                  <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 dark:text-gray-300 uppercase tracking-wider">
                    Invoice #
                  </th>
                  <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 dark:text-gray-300 uppercase tracking-wider">
                    Date
                  </th>
                  <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 dark:text-gray-300 uppercase tracking-wider">
                    Due Date
                  </th>
                  <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 dark:text-gray-300 uppercase tracking-wider">
                    Amount
                  </th>
                  <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 dark:text-gray-300 uppercase tracking-wider">
                    Status
                  </th>
                  <th className="px-6 py-3 text-right text-xs font-medium text-gray-500 dark:text-gray-300 uppercase tracking-wider">
                    Actions
                  </th>
                </tr>
              </thead>
              <tbody className="bg-white dark:bg-gray-800 divide-y divide-gray-200 dark:divide-gray-700">
                {loading ? (
                  <tr>
                    <td colSpan={6} className="px-6 py-4 text-center text-gray-500 dark:text-gray-400">
                      Loading invoices...
                    </td>
                  </tr>
                ) : filteredInvoices.length === 0 ? (
                  <tr>
                    <td colSpan={6} className="px-6 py-8 text-center">
                      <FaFileInvoiceDollar className="text-5xl text-gray-400 mx-auto mb-4" />
                      <p className="text-gray-500 dark:text-gray-400">No invoices found</p>
                    </td>
                  </tr>
                ) : (
                  filteredInvoices.map((invoice) => (
                    <tr key={invoice.id} className="hover:bg-gray-50 dark:hover:bg-gray-700">
                      <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-gray-900 dark:text-gray-100">
                        {invoice.invoice_number}
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500 dark:text-gray-400">
                        {new Date(invoice.created_at).toLocaleDateString()}
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500 dark:text-gray-400">
                        {new Date(invoice.due_date).toLocaleDateString()}
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm font-semibold text-gray-900 dark:text-gray-100">
                        ${parseFloat(invoice.amount).toFixed(2)}
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap">
                        {getStatusBadge(invoice.status)}
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-right text-sm font-medium">
                        <div className="flex items-center justify-end gap-2">
                          <Link
                            href={`/dashboard/invoices/${invoice.id}`}
                            className="text-blue-600 hover:text-blue-900 dark:text-blue-400 dark:hover:text-blue-300"
                          >
                            <FaEye />
                          </Link>
                          {invoice.status.toLowerCase() === 'pending' && (
                            <button className="text-green-600 hover:text-green-900 dark:text-green-400 dark:hover:text-green-300">
                              <FaCreditCard />
                            </button>
                          )}
                        </div>
                      </td>
                    </tr>
                  ))
                )}
              </tbody>
            </table>
          </div>
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
