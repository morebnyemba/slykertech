'use client';

import { useState, useEffect, useCallback } from 'react';
import AdminLayout from '@/components/admin/AdminLayout';
import { apiService } from '@/lib/api-service';
import { 
  FaFileInvoiceDollar, FaSpinner, FaSearch, FaPaperPlane,
  FaCheckCircle, FaTimes
} from 'react-icons/fa';

interface InvoiceItem {
  id: number;
  description: string;
  quantity: number;
  unit_price: number;
  amount: number;
}

interface Invoice {
  id: number;
  client: { id: number; company_name: string; email?: string };
  invoice_number: string;
  status: string;
  issue_date: string;
  due_date: string;
  paid_date?: string;
  subtotal: number;
  tax_rate: number;
  tax_amount: number;
  discount_amount: number;
  total: number;
  notes?: string;
  terms?: string;
  items: InvoiceItem[];
  payments?: unknown[];
  created_at: string;
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

export default function InvoicesPage() {
  const [invoices, setInvoices] = useState<Invoice[]>([]);
  const [stats, setStats] = useState<InvoiceStats | null>(null);
  const [loading, setLoading] = useState(true);
  const [selectedInvoice, setSelectedInvoice] = useState<Invoice | null>(null);
  const [filter, setFilter] = useState<string>('');
  const [searchTerm, setSearchTerm] = useState('');
  const [actionLoading, setActionLoading] = useState<number | null>(null);

  const fetchInvoices = useCallback(async () => {
    try {
      const response = await apiService.getInvoices(filter || undefined);
      if (response.data) {
        setInvoices(response.data as Invoice[]);
      }
    } catch (error) {
      console.error('Failed to fetch invoices:', error);
    } finally {
      setLoading(false);
    }
  }, [filter]);

  const fetchStats = useCallback(async () => {
    try {
      const response = await apiService.getInvoiceStats();
      if (response.data) {
        setStats(response.data as InvoiceStats);
      }
    } catch (error) {
      console.error('Failed to fetch stats:', error);
    }
  }, []);

  useEffect(() => {
    fetchInvoices();
    fetchStats();
  }, [fetchInvoices, fetchStats]);

  const handleSendInvoice = async (id: number) => {
    setActionLoading(id);
    try {
      await apiService.sendInvoice(id);
      fetchInvoices();
      fetchStats();
    } catch (error) {
      console.error('Failed to send invoice:', error);
    } finally {
      setActionLoading(null);
    }
  };

  const handleMarkPaid = async (id: number) => {
    setActionLoading(id);
    try {
      await apiService.markInvoicePaid(id);
      fetchInvoices();
      fetchStats();
      if (selectedInvoice?.id === id) {
        const response = await apiService.getInvoice(id);
        setSelectedInvoice(response.data as Invoice);
      }
    } catch (error) {
      console.error('Failed to mark paid:', error);
    } finally {
      setActionLoading(null);
    }
  };

  const handleCancel = async (id: number) => {
    setActionLoading(id);
    try {
      await apiService.cancelInvoice(id);
      fetchInvoices();
      fetchStats();
    } catch (error) {
      console.error('Failed to cancel invoice:', error);
    } finally {
      setActionLoading(null);
    }
  };

  const statusColors: Record<string, string> = {
    draft: 'bg-gray-100 text-gray-800 dark:bg-gray-700 dark:text-gray-400',
    sent: 'bg-blue-100 text-blue-800 dark:bg-blue-900/30 dark:text-blue-400',
    paid: 'bg-green-100 text-green-800 dark:bg-green-900/30 dark:text-green-400',
    overdue: 'bg-red-100 text-red-800 dark:bg-red-900/30 dark:text-red-400',
    cancelled: 'bg-gray-100 text-gray-500 dark:bg-gray-700 dark:text-gray-500',
  };

  const filteredInvoices = invoices.filter(invoice =>
    invoice.invoice_number.toLowerCase().includes(searchTerm.toLowerCase()) ||
    invoice.client?.company_name?.toLowerCase().includes(searchTerm.toLowerCase())
  );

  const formatCurrency = (amount: number) => {
    return new Intl.NumberFormat('en-US', {
      style: 'currency',
      currency: 'USD',
    }).format(amount);
  };

  return (
    <AdminLayout>
      <div className="space-y-6">
        {/* Header with Stats */}
        <div className="flex flex-col lg:flex-row lg:items-center lg:justify-between gap-4">
          <div>
            <h1 className="text-2xl font-bold text-gray-900 dark:text-white flex items-center gap-2">
              <FaFileInvoiceDollar />
              Invoices
            </h1>
            <p className="text-gray-500 dark:text-gray-400">Manage billing and invoices</p>
          </div>
          
          {stats && (
            <div className="flex flex-wrap gap-3">
              <div className="px-4 py-2 bg-green-50 dark:bg-green-900/20 rounded-lg">
                <span className="text-xl font-bold text-green-600">{formatCurrency(stats.revenue.this_month)}</span>
                <span className="text-sm text-gray-500 ml-2">This Month</span>
              </div>
              <div className="px-4 py-2 bg-yellow-50 dark:bg-yellow-900/20 rounded-lg">
                <span className="text-xl font-bold text-yellow-600">{formatCurrency(stats.outstanding)}</span>
                <span className="text-sm text-gray-500 ml-2">Outstanding</span>
              </div>
              <div className="px-4 py-2 bg-blue-50 dark:bg-blue-900/20 rounded-lg">
                <span className="text-xl font-bold text-blue-600">{stats.by_status.paid}</span>
                <span className="text-sm text-gray-500 ml-2">Paid</span>
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
              placeholder="Search invoices..."
              value={searchTerm}
              onChange={(e) => setSearchTerm(e.target.value)}
              className="w-full pl-10 pr-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-800"
            />
          </div>
          
          <select
            value={filter}
            onChange={(e) => setFilter(e.target.value)}
            className="px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-800"
          >
            <option value="">All Status</option>
            <option value="draft">Draft</option>
            <option value="sent">Sent</option>
            <option value="paid">Paid</option>
            <option value="overdue">Overdue</option>
            <option value="cancelled">Cancelled</option>
          </select>
        </div>

        <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
          {/* Invoice List */}
          <div className="lg:col-span-2 bg-white dark:bg-gray-800 rounded-xl shadow-lg overflow-hidden">
            <div className="p-4 border-b border-gray-200 dark:border-gray-700 flex items-center justify-between">
              <h2 className="font-semibold text-gray-900 dark:text-white">
                Invoices ({filteredInvoices.length})
              </h2>
            </div>
            
            <div className="overflow-x-auto">
              {loading ? (
                <div className="flex items-center justify-center py-12">
                  <FaSpinner className="animate-spin h-8 w-8 text-blue-600" />
                </div>
              ) : filteredInvoices.length === 0 ? (
                <div className="p-8 text-center text-gray-500">No invoices found</div>
              ) : (
                <table className="w-full">
                  <thead className="bg-gray-50 dark:bg-gray-700">
                    <tr>
                      <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase">Invoice #</th>
                      <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase">Client</th>
                      <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase">Amount</th>
                      <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase">Status</th>
                      <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase">Due Date</th>
                      <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase">Actions</th>
                    </tr>
                  </thead>
                  <tbody className="divide-y divide-gray-100 dark:divide-gray-700">
                    {filteredInvoices.map((invoice) => (
                      <tr 
                        key={invoice.id}
                        className={`hover:bg-gray-50 dark:hover:bg-gray-700/50 cursor-pointer ${
                          selectedInvoice?.id === invoice.id ? 'bg-blue-50 dark:bg-blue-900/20' : ''
                        }`}
                        onClick={() => setSelectedInvoice(invoice)}
                      >
                        <td className="px-4 py-3 font-medium text-gray-900 dark:text-white">
                          {invoice.invoice_number}
                        </td>
                        <td className="px-4 py-3 text-gray-600 dark:text-gray-400">
                          {invoice.client?.company_name || 'N/A'}
                        </td>
                        <td className="px-4 py-3 font-medium text-gray-900 dark:text-white">
                          {formatCurrency(invoice.total)}
                        </td>
                        <td className="px-4 py-3">
                          <span className={`px-2 py-1 text-xs rounded-full ${statusColors[invoice.status]}`}>
                            {invoice.status}
                          </span>
                        </td>
                        <td className="px-4 py-3 text-gray-600 dark:text-gray-400">
                          {new Date(invoice.due_date).toLocaleDateString()}
                        </td>
                        <td className="px-4 py-3">
                          <div className="flex items-center gap-2">
                            {invoice.status === 'draft' && (
                              <button
                                onClick={(e) => { e.stopPropagation(); handleSendInvoice(invoice.id); }}
                                disabled={actionLoading === invoice.id}
                                className="p-1.5 text-blue-600 hover:bg-blue-50 dark:hover:bg-blue-900/20 rounded"
                                title="Send Invoice"
                              >
                                {actionLoading === invoice.id ? <FaSpinner className="animate-spin" /> : <FaPaperPlane />}
                              </button>
                            )}
                            {['sent', 'overdue'].includes(invoice.status) && (
                              <button
                                onClick={(e) => { e.stopPropagation(); handleMarkPaid(invoice.id); }}
                                disabled={actionLoading === invoice.id}
                                className="p-1.5 text-green-600 hover:bg-green-50 dark:hover:bg-green-900/20 rounded"
                                title="Mark Paid"
                              >
                                {actionLoading === invoice.id ? <FaSpinner className="animate-spin" /> : <FaCheckCircle />}
                              </button>
                            )}
                            {['draft', 'sent'].includes(invoice.status) && (
                              <button
                                onClick={(e) => { e.stopPropagation(); handleCancel(invoice.id); }}
                                disabled={actionLoading === invoice.id}
                                className="p-1.5 text-red-600 hover:bg-red-50 dark:hover:bg-red-900/20 rounded"
                                title="Cancel"
                              >
                                <FaTimes />
                              </button>
                            )}
                          </div>
                        </td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              )}
            </div>
          </div>

          {/* Invoice Details */}
          <div className="bg-white dark:bg-gray-800 rounded-xl shadow-lg overflow-hidden">
            {selectedInvoice ? (
              <div className="p-4 space-y-4">
                <div className="flex items-center justify-between">
                  <h2 className="text-lg font-semibold text-gray-900 dark:text-white">
                    {selectedInvoice.invoice_number}
                  </h2>
                  <span className={`px-3 py-1 text-sm rounded-full ${statusColors[selectedInvoice.status]}`}>
                    {selectedInvoice.status}
                  </span>
                </div>

                <div className="border-t border-gray-200 dark:border-gray-700 pt-4">
                  <h3 className="text-sm font-medium text-gray-500 mb-2">Client</h3>
                  <p className="text-gray-900 dark:text-white">{selectedInvoice.client?.company_name}</p>
                </div>

                <div className="grid grid-cols-2 gap-4">
                  <div>
                    <h3 className="text-sm font-medium text-gray-500 mb-1">Issue Date</h3>
                    <p className="text-gray-900 dark:text-white">{new Date(selectedInvoice.issue_date).toLocaleDateString()}</p>
                  </div>
                  <div>
                    <h3 className="text-sm font-medium text-gray-500 mb-1">Due Date</h3>
                    <p className="text-gray-900 dark:text-white">{new Date(selectedInvoice.due_date).toLocaleDateString()}</p>
                  </div>
                </div>

                <div className="border-t border-gray-200 dark:border-gray-700 pt-4">
                  <h3 className="text-sm font-medium text-gray-500 mb-2">Items</h3>
                  <div className="space-y-2">
                    {selectedInvoice.items?.map((item) => (
                      <div key={item.id} className="flex justify-between text-sm">
                        <span className="text-gray-600 dark:text-gray-400">{item.description}</span>
                        <span className="text-gray-900 dark:text-white">{formatCurrency(item.amount)}</span>
                      </div>
                    ))}
                  </div>
                </div>

                <div className="border-t border-gray-200 dark:border-gray-700 pt-4 space-y-2">
                  <div className="flex justify-between">
                    <span className="text-gray-500">Subtotal</span>
                    <span className="text-gray-900 dark:text-white">{formatCurrency(selectedInvoice.subtotal)}</span>
                  </div>
                  {selectedInvoice.tax_amount > 0 && (
                    <div className="flex justify-between">
                      <span className="text-gray-500">Tax ({selectedInvoice.tax_rate}%)</span>
                      <span className="text-gray-900 dark:text-white">{formatCurrency(selectedInvoice.tax_amount)}</span>
                    </div>
                  )}
                  {selectedInvoice.discount_amount > 0 && (
                    <div className="flex justify-between">
                      <span className="text-gray-500">Discount</span>
                      <span className="text-green-600">-{formatCurrency(selectedInvoice.discount_amount)}</span>
                    </div>
                  )}
                  <div className="flex justify-between text-lg font-bold pt-2 border-t border-gray-200 dark:border-gray-700">
                    <span className="text-gray-900 dark:text-white">Total</span>
                    <span className="text-gray-900 dark:text-white">{formatCurrency(selectedInvoice.total)}</span>
                  </div>
                </div>

                {/* Actions */}
                <div className="border-t border-gray-200 dark:border-gray-700 pt-4 flex gap-2">
                  {selectedInvoice.status === 'draft' && (
                    <button
                      onClick={() => handleSendInvoice(selectedInvoice.id)}
                      disabled={actionLoading === selectedInvoice.id}
                      className="flex-1 flex items-center justify-center gap-2 px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 disabled:opacity-50"
                    >
                      <FaPaperPlane /> Send
                    </button>
                  )}
                  {['sent', 'overdue'].includes(selectedInvoice.status) && (
                    <button
                      onClick={() => handleMarkPaid(selectedInvoice.id)}
                      disabled={actionLoading === selectedInvoice.id}
                      className="flex-1 flex items-center justify-center gap-2 px-4 py-2 bg-green-600 text-white rounded-lg hover:bg-green-700 disabled:opacity-50"
                    >
                      <FaCheckCircle /> Mark Paid
                    </button>
                  )}
                </div>
              </div>
            ) : (
              <div className="h-[500px] flex items-center justify-center text-gray-500">
                <div className="text-center">
                  <FaFileInvoiceDollar className="h-12 w-12 mx-auto mb-4 opacity-50" />
                  <p>Select an invoice to view details</p>
                </div>
              </div>
            )}
          </div>
        </div>
      </div>
    </AdminLayout>
  );
}
