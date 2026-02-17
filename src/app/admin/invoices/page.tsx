'use client';

import { useState, useEffect, useCallback } from 'react';
import AdminLayout from '@/components/admin/AdminLayout';
import { apiService } from '@/lib/api-service';
import { useAuthStore } from '@/lib/stores/auth-store';
import { 
  FaFileInvoiceDollar, FaSpinner, FaSearch, FaPaperPlane,
  FaCheckCircle, FaTimes, FaPlus, FaTrash
} from 'react-icons/fa';

interface InvoiceItem {
  id?: number;
  description: string;
  quantity: number;
  unit_price: number;
  amount: number;
}

interface Client {
  id: number;
  company_name: string;
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

const normalizeList = <T,>(data: unknown): T[] => {
  if (Array.isArray(data)) return data as T[];

  if (data && typeof data === 'object' && 'results' in data) {
    const results = (data as { results?: unknown }).results;
    return Array.isArray(results) ? (results as T[]) : [];
  }

  return [];
};

const normalizeInvoice = (invoice: Invoice): Invoice => ({
  ...invoice,
  items: normalizeList<InvoiceItem>(invoice.items),
});

export default function InvoicesPage() {
  const { isAuthenticated, token, hasHydrated } = useAuthStore();
  const [invoices, setInvoices] = useState<Invoice[]>([]);
  const [clients, setClients] = useState<Client[]>([]);
  const [stats, setStats] = useState<InvoiceStats | null>(null);
  const [loading, setLoading] = useState(true);
  const [selectedInvoice, setSelectedInvoice] = useState<Invoice | null>(null);
  const [filter, setFilter] = useState<string>('');
  const [searchTerm, setSearchTerm] = useState('');
  const [actionLoading, setActionLoading] = useState<number | null>(null);
  const [showCreateForm, setShowCreateForm] = useState(false);
  
  // Create form state
  const [formData, setFormData] = useState({
    client: '',
    issue_date: new Date().toISOString().split('T')[0],
    due_date: new Date(Date.now() + 30 * 24 * 60 * 60 * 1000).toISOString().split('T')[0],
    tax_rate: '0',
    discount_amount: '0',
    notes: '',
    terms: 'Payment is due within 30 days of invoice date.',
  });
  const [invoiceItems, setInvoiceItems] = useState<Array<{ description: string; quantity: string; unit_price: string }>>([
    { description: '', quantity: '1', unit_price: '' }
  ]);

  const fetchClients = useCallback(async () => {
    // Only fetch clients if hydration is complete and user is authenticated with a valid token
    if (!hasHydrated || !isAuthenticated || !token) {
      return;
    }

    try {
      const response = await apiService.getAllClients();
      if (response.data) {
        setClients(normalizeList<Client>(response.data));
      } else {
        setClients([]);
      }
    } catch (error) {
      console.error('Failed to fetch clients:', error);
      setClients([]);
    }
  }, [hasHydrated, isAuthenticated, token]);

  const fetchInvoices = useCallback(async () => {
    // Only fetch invoices if hydration is complete and user is authenticated with a valid token
    if (!hasHydrated || !isAuthenticated || !token) {
      setLoading(false);
      return;
    }

    try {
      const response = await apiService.getInvoices(filter || undefined);
      if (response.data) {
        setInvoices(normalizeList<Invoice>(response.data).map(normalizeInvoice));
      } else {
        setInvoices([]);
      }
    } catch (error) {
      console.error('Failed to fetch invoices:', error);
      setInvoices([]);
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
      const response = await apiService.getInvoiceStats();
      if (response.data) {
        setStats(response.data as InvoiceStats);
      }
    } catch (error) {
      console.error('Failed to fetch stats:', error);
    }
  }, [hasHydrated, isAuthenticated, token]);

  useEffect(() => {
    fetchInvoices();
    fetchStats();
    fetchClients();
  }, [fetchInvoices, fetchStats, fetchClients]);

  const handleCreateInvoice = async (e: React.FormEvent) => {
    e.preventDefault();
    setActionLoading(-1);
    
    try {
      const items = invoiceItems
        .filter(item => item.description && item.unit_price)
        .map(item => ({
          description: item.description,
          quantity: parseInt(item.quantity) || 1,
          unit_price: parseFloat(item.unit_price) || 0,
        }));
      
      await apiService.createInvoice({
        client: parseInt(formData.client),
        issue_date: formData.issue_date,
        due_date: formData.due_date,
        items,
        tax_rate: parseFloat(formData.tax_rate) || 0,
        discount_amount: parseFloat(formData.discount_amount) || 0,
        notes: formData.notes,
        terms: formData.terms,
      });
      
      fetchInvoices();
      fetchStats();
      resetForm();
    } catch (error) {
      console.error('Failed to create invoice:', error);
    } finally {
      setActionLoading(null);
    }
  };

  const resetForm = () => {
    setShowCreateForm(false);
    setFormData({
      client: '',
      issue_date: new Date().toISOString().split('T')[0],
      due_date: new Date(Date.now() + 30 * 24 * 60 * 60 * 1000).toISOString().split('T')[0],
      tax_rate: '0',
      discount_amount: '0',
      notes: '',
      terms: 'Payment is due within 30 days of invoice date.',
    });
    setInvoiceItems([{ description: '', quantity: '1', unit_price: '' }]);
  };

  const addInvoiceItem = () => {
    setInvoiceItems([...invoiceItems, { description: '', quantity: '1', unit_price: '' }]);
  };

  const removeInvoiceItem = (index: number) => {
    if (invoiceItems.length > 1) {
      setInvoiceItems(invoiceItems.filter((_, i) => i !== index));
    }
  };

  const updateInvoiceItem = (index: number, field: string, value: string) => {
    const updated = [...invoiceItems];
    updated[index] = { ...updated[index], [field]: value };
    setInvoiceItems(updated);
  };

  const calculateTotal = () => {
    const subtotal = invoiceItems.reduce((sum, item) => {
      return sum + ((parseFloat(item.quantity) || 0) * (parseFloat(item.unit_price) || 0));
    }, 0);
    const taxAmount = subtotal * (parseFloat(formData.tax_rate) || 0) / 100;
    const discount = parseFloat(formData.discount_amount) || 0;
    return subtotal + taxAmount - discount;
  };

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
        if (response.data) {
          setSelectedInvoice(normalizeInvoice(response.data as Invoice));
        }
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
          
          <div className="flex flex-wrap items-center gap-3">
            {stats && (
              <>
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
              </>
            )}
            <button
              onClick={() => setShowCreateForm(true)}
              className="flex items-center gap-2 px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors"
            >
              <FaPlus /> Create Invoice
            </button>
          </div>
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
                        onClick={() => setSelectedInvoice(normalizeInvoice(invoice))}
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
                    {normalizeList<InvoiceItem>(selectedInvoice.items).map((item) => (
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

        {/* Create Invoice Modal */}
        {showCreateForm && (
          <div className="fixed inset-0 bg-black/50 flex items-center justify-center z-50 p-4">
            <div className="bg-white dark:bg-gray-800 rounded-xl shadow-2xl max-w-2xl w-full max-h-[90vh] overflow-y-auto">
              <div className="p-6">
                <div className="flex items-center justify-between mb-6">
                  <h2 className="text-xl font-semibold text-gray-900 dark:text-white">
                    Create New Invoice
                  </h2>
                  <button onClick={resetForm} className="text-gray-500 hover:text-gray-700">
                    <FaTimes />
                  </button>
                </div>

                <form onSubmit={handleCreateInvoice} className="space-y-4">
                  <div>
                    <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">
                      Client *
                    </label>
                    <select
                      value={formData.client}
                      onChange={(e) => setFormData({ ...formData, client: e.target.value })}
                      required
                      className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-white"
                    >
                      <option value="">Select a client</option>
                      {clients.map(client => (
                        <option key={client.id} value={client.id}>{client.company_name}</option>
                      ))}
                    </select>
                  </div>

                  <div className="grid grid-cols-2 gap-4">
                    <div>
                      <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">
                        Issue Date *
                      </label>
                      <input
                        type="date"
                        value={formData.issue_date}
                        onChange={(e) => setFormData({ ...formData, issue_date: e.target.value })}
                        required
                        className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-white"
                      />
                    </div>
                    <div>
                      <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">
                        Due Date *
                      </label>
                      <input
                        type="date"
                        value={formData.due_date}
                        onChange={(e) => setFormData({ ...formData, due_date: e.target.value })}
                        required
                        className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-white"
                      />
                    </div>
                  </div>

                  {/* Invoice Items */}
                  <div>
                    <div className="flex items-center justify-between mb-2">
                      <label className="block text-sm font-medium text-gray-700 dark:text-gray-300">
                        Line Items *
                      </label>
                      <button
                        type="button"
                        onClick={addInvoiceItem}
                        className="text-sm text-blue-600 hover:text-blue-700 flex items-center gap-1"
                      >
                        <FaPlus className="h-3 w-3" /> Add Item
                      </button>
                    </div>
                    <div className="space-y-2">
                      {invoiceItems.map((item, index) => (
                        <div key={index} className="flex gap-2 items-start">
                          <input
                            type="text"
                            value={item.description}
                            onChange={(e) => updateInvoiceItem(index, 'description', e.target.value)}
                            placeholder="Description"
                            className="flex-1 px-3 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-white text-sm"
                          />
                          <input
                            type="number"
                            value={item.quantity}
                            onChange={(e) => updateInvoiceItem(index, 'quantity', e.target.value)}
                            placeholder="Qty"
                            min="1"
                            className="w-20 px-3 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-white text-sm"
                          />
                          <input
                            type="number"
                            value={item.unit_price}
                            onChange={(e) => updateInvoiceItem(index, 'unit_price', e.target.value)}
                            placeholder="Price"
                            step="0.01"
                            min="0"
                            className="w-28 px-3 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-white text-sm"
                          />
                          {invoiceItems.length > 1 && (
                            <button
                              type="button"
                              onClick={() => removeInvoiceItem(index)}
                              className="p-2 text-red-500 hover:bg-red-50 dark:hover:bg-red-900/20 rounded-lg"
                            >
                              <FaTrash className="h-4 w-4" />
                            </button>
                          )}
                        </div>
                      ))}
                    </div>
                  </div>

                  <div className="grid grid-cols-2 gap-4">
                    <div>
                      <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">
                        Tax Rate (%)
                      </label>
                      <input
                        type="number"
                        value={formData.tax_rate}
                        onChange={(e) => setFormData({ ...formData, tax_rate: e.target.value })}
                        step="0.01"
                        min="0"
                        className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-white"
                      />
                    </div>
                    <div>
                      <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">
                        Discount Amount
                      </label>
                      <input
                        type="number"
                        value={formData.discount_amount}
                        onChange={(e) => setFormData({ ...formData, discount_amount: e.target.value })}
                        step="0.01"
                        min="0"
                        className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-white"
                      />
                    </div>
                  </div>

                  <div>
                    <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">
                      Notes
                    </label>
                    <textarea
                      value={formData.notes}
                      onChange={(e) => setFormData({ ...formData, notes: e.target.value })}
                      rows={2}
                      className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-white"
                      placeholder="Additional notes..."
                    />
                  </div>

                  <div>
                    <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">
                      Terms
                    </label>
                    <textarea
                      value={formData.terms}
                      onChange={(e) => setFormData({ ...formData, terms: e.target.value })}
                      rows={2}
                      className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-white"
                    />
                  </div>

                  {/* Total Preview */}
                  <div className="bg-gray-50 dark:bg-gray-700/50 rounded-lg p-4">
                    <div className="flex justify-between text-lg font-bold">
                      <span className="text-gray-900 dark:text-white">Estimated Total</span>
                      <span className="text-blue-600">{formatCurrency(calculateTotal())}</span>
                    </div>
                  </div>

                  <div className="flex gap-3 pt-4">
                    <button
                      type="button"
                      onClick={resetForm}
                      className="flex-1 px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg hover:bg-gray-50 dark:hover:bg-gray-700 text-gray-700 dark:text-gray-300 transition-colors"
                    >
                      Cancel
                    </button>
                    <button
                      type="submit"
                      disabled={actionLoading === -1 || !formData.client || !invoiceItems.some(i => i.description && i.unit_price)}
                      className="flex-1 px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 disabled:opacity-50 transition-colors"
                    >
                      {actionLoading === -1 ? (
                        <FaSpinner className="animate-spin mx-auto" />
                      ) : 'Create Invoice'}
                    </button>
                  </div>
                </form>
              </div>
            </div>
          </div>
        )}
      </div>
    </AdminLayout>
  );
}
