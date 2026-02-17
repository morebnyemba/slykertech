'use client';

import { useState, useEffect, useCallback } from 'react';
import AdminLayout from '@/components/admin/AdminLayout';
import { apiService } from '@/lib/api-service';
import { 
  FaMoneyBillWave, FaSpinner, FaSearch, FaPlus, FaEdit, 
  FaTrash, FaTimes, FaCheck, FaServer, FaGlobe
} from 'react-icons/fa';

interface Expense {
  id: number;
  name: string;
  category: string;
  category_display: string;
  amount: number;
  recurring: string;
  recurring_display: string;
  service?: number;
  service_name?: string;
  vendor?: string;
  expense_date: string;
  next_due_date?: string;
  reference_number?: string;
  notes?: string;
  is_paid: boolean;
  created_at: string;
}

interface ExpenseStats {
  total_expenses: number;
  total_amount: number;
  this_month: number;
  last_month: number;
  by_category: Record<string, { count: number; total: number }>;
  recurring: {
    monthly: number;
    annual: number;
    estimated_monthly: number;
  };
  profit: {
    this_month_revenue: number;
    this_month_expenses: number;
    this_month_profit: number;
    profit_margin_percent: number;
  };
}

const CATEGORY_OPTIONS = [
  { value: 'server', label: 'Server/Hosting Costs' },
  { value: 'domain', label: 'Domain Registry Costs' },
  { value: 'license', label: 'Software Licenses' },
  { value: 'cpanel', label: 'cPanel/Control Panel' },
  { value: 'ssl', label: 'SSL Certificates' },
  { value: 'bandwidth', label: 'Bandwidth/CDN' },
  { value: 'marketing', label: 'Marketing & Advertising' },
  { value: 'salary', label: 'Salaries & Wages' },
  { value: 'office', label: 'Office & Utilities' },
  { value: 'other', label: 'Other' },
];

const RECURRING_OPTIONS = [
  { value: 'none', label: 'One-time' },
  { value: 'monthly', label: 'Monthly' },
  { value: 'quarterly', label: 'Quarterly' },
  { value: 'annual', label: 'Annual' },
];

const normalizeList = <T,>(data: unknown): T[] => {
  if (Array.isArray(data)) return data as T[];

  if (data && typeof data === 'object' && 'results' in data) {
    const results = (data as { results?: unknown }).results;
    return Array.isArray(results) ? (results as T[]) : [];
  }

  return [];
};

export default function ExpensesPage() {
  const [expenses, setExpenses] = useState<Expense[]>([]);
  const [stats, setStats] = useState<ExpenseStats | null>(null);
  const [loading, setLoading] = useState(true);
  const [filter, setFilter] = useState('');
  const [searchTerm, setSearchTerm] = useState('');
  const [showForm, setShowForm] = useState(false);
  const [editingExpense, setEditingExpense] = useState<Expense | null>(null);
  const [actionLoading, setActionLoading] = useState<number | null>(null);
  
  // Form state
  const [formData, setFormData] = useState({
    name: '',
    category: 'server',
    amount: '',
    recurring: 'none',
    vendor: '',
    expense_date: new Date().toISOString().split('T')[0],
    next_due_date: '',
    reference_number: '',
    notes: '',
    is_paid: true,
  });

  const fetchExpenses = useCallback(async () => {
    try {
      const response = await apiService.getExpenses(filter || undefined);
      if (response.data) {
        setExpenses(normalizeList<Expense>(response.data));
      } else {
        setExpenses([]);
      }
    } catch (error) {
      console.error('Failed to fetch expenses:', error);
      setExpenses([]);
    } finally {
      setLoading(false);
    }
  }, [filter]);

  const fetchStats = useCallback(async () => {
    try {
      const response = await apiService.getExpenseStats();
      if (response.data) {
        setStats(response.data as ExpenseStats);
      }
    } catch (error) {
      console.error('Failed to fetch stats:', error);
    }
  }, []);

  useEffect(() => {
    fetchExpenses();
    fetchStats();
  }, [fetchExpenses, fetchStats]);

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setActionLoading(-1);
    
    try {
      const data = {
        ...formData,
        amount: parseFloat(formData.amount),
        next_due_date: formData.next_due_date || undefined,
      };

      if (editingExpense) {
        await apiService.updateExpense(editingExpense.id, data);
      } else {
        await apiService.createExpense(data);
      }
      
      fetchExpenses();
      fetchStats();
      resetForm();
    } catch (error) {
      console.error('Failed to save expense:', error);
    } finally {
      setActionLoading(null);
    }
  };

  const handleDelete = async (id: number) => {
    if (!confirm('Are you sure you want to delete this expense?')) return;
    
    setActionLoading(id);
    try {
      await apiService.deleteExpense(id);
      fetchExpenses();
      fetchStats();
    } catch (error) {
      console.error('Failed to delete expense:', error);
    } finally {
      setActionLoading(null);
    }
  };

  const handleEdit = (expense: Expense) => {
    setEditingExpense(expense);
    setFormData({
      name: expense.name,
      category: expense.category,
      amount: expense.amount.toString(),
      recurring: expense.recurring,
      vendor: expense.vendor || '',
      expense_date: expense.expense_date,
      next_due_date: expense.next_due_date || '',
      reference_number: expense.reference_number || '',
      notes: expense.notes || '',
      is_paid: expense.is_paid,
    });
    setShowForm(true);
  };

  const resetForm = () => {
    setShowForm(false);
    setEditingExpense(null);
    setFormData({
      name: '',
      category: 'server',
      amount: '',
      recurring: 'none',
      vendor: '',
      expense_date: new Date().toISOString().split('T')[0],
      next_due_date: '',
      reference_number: '',
      notes: '',
      is_paid: true,
    });
  };

  const formatCurrency = (amount: number) => {
    return new Intl.NumberFormat('en-US', {
      style: 'currency',
      currency: 'USD',
    }).format(amount);
  };

  const filteredExpenses = expenses.filter(expense =>
    expense.name.toLowerCase().includes(searchTerm.toLowerCase()) ||
    expense.vendor?.toLowerCase().includes(searchTerm.toLowerCase())
  );

  const getCategoryIcon = (category: string) => {
    switch (category) {
      case 'server':
      case 'cpanel':
        return <FaServer className="text-blue-500" />;
      case 'domain':
        return <FaGlobe className="text-green-500" />;
      default:
        return <FaMoneyBillWave className="text-gray-500" />;
    }
  };

  return (
    <AdminLayout>
      <div className="space-y-6">
        {/* Header with Stats */}
        <div className="flex flex-col lg:flex-row lg:items-center lg:justify-between gap-4">
          <div>
            <h1 className="text-2xl font-bold text-gray-900 dark:text-white flex items-center gap-2">
              <FaMoneyBillWave />
              Expense Tracking
            </h1>
            <p className="text-gray-500 dark:text-gray-400">Track operational costs and profit margins</p>
          </div>
          
          <button
            onClick={() => setShowForm(true)}
            className="flex items-center gap-2 px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700"
          >
            <FaPlus /> Add Expense
          </button>
        </div>

        {/* Profit Summary */}
        {stats && (
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
            <div className="bg-gradient-to-br from-green-500 to-green-600 rounded-xl p-6 text-white">
              <p className="text-white/80 text-sm">This Month Revenue</p>
              <p className="text-2xl font-bold">{formatCurrency(stats.profit.this_month_revenue)}</p>
            </div>
            <div className="bg-gradient-to-br from-red-500 to-red-600 rounded-xl p-6 text-white">
              <p className="text-white/80 text-sm">This Month Expenses</p>
              <p className="text-2xl font-bold">{formatCurrency(stats.profit.this_month_expenses)}</p>
            </div>
            <div className={`bg-gradient-to-br ${stats.profit.this_month_profit >= 0 ? 'from-emerald-500 to-emerald-600' : 'from-orange-500 to-orange-600'} rounded-xl p-6 text-white`}>
              <p className="text-white/80 text-sm">Net Profit</p>
              <p className="text-2xl font-bold">{formatCurrency(stats.profit.this_month_profit)}</p>
            </div>
            <div className="bg-gradient-to-br from-purple-500 to-purple-600 rounded-xl p-6 text-white">
              <p className="text-white/80 text-sm">Profit Margin</p>
              <p className="text-2xl font-bold">{stats.profit.profit_margin_percent.toFixed(1)}%</p>
            </div>
          </div>
        )}

        {/* Recurring Costs Summary */}
        {stats && (
          <div className="bg-white dark:bg-gray-800 rounded-xl shadow-lg p-6">
            <h3 className="font-semibold text-gray-900 dark:text-white mb-4">Recurring Costs</h3>
            <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
              <div className="text-center p-4 bg-gray-50 dark:bg-gray-700/50 rounded-lg">
                <p className="text-sm text-gray-500">Monthly</p>
                <p className="text-xl font-bold text-gray-900 dark:text-white">{formatCurrency(stats.recurring.monthly)}</p>
              </div>
              <div className="text-center p-4 bg-gray-50 dark:bg-gray-700/50 rounded-lg">
                <p className="text-sm text-gray-500">Annual</p>
                <p className="text-xl font-bold text-gray-900 dark:text-white">{formatCurrency(stats.recurring.annual)}</p>
              </div>
              <div className="text-center p-4 bg-blue-50 dark:bg-blue-900/20 rounded-lg">
                <p className="text-sm text-gray-500">Est. Monthly Total</p>
                <p className="text-xl font-bold text-blue-600">{formatCurrency(stats.recurring.estimated_monthly)}</p>
              </div>
            </div>
          </div>
        )}

        {/* Filters */}
        <div className="flex flex-wrap gap-4 items-center">
          <div className="relative flex-1 min-w-[200px]">
            <FaSearch className="absolute left-3 top-1/2 -translate-y-1/2 text-gray-400" />
            <input
              type="text"
              placeholder="Search expenses..."
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
            <option value="">All Categories</option>
            {CATEGORY_OPTIONS.map(cat => (
              <option key={cat.value} value={cat.value}>{cat.label}</option>
            ))}
          </select>
        </div>

        {/* Expenses Table */}
        <div className="bg-white dark:bg-gray-800 rounded-xl shadow-lg overflow-hidden">
          <div className="overflow-x-auto">
            {loading ? (
              <div className="flex items-center justify-center py-12">
                <FaSpinner className="animate-spin h-8 w-8 text-blue-600" />
              </div>
            ) : filteredExpenses.length === 0 ? (
              <div className="p-8 text-center text-gray-500">No expenses found</div>
            ) : (
              <table className="w-full">
                <thead className="bg-gray-50 dark:bg-gray-700">
                  <tr>
                    <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase">Name</th>
                    <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase">Category</th>
                    <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase">Amount</th>
                    <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase">Recurring</th>
                    <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase">Vendor</th>
                    <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase">Date</th>
                    <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase">Paid</th>
                    <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase">Actions</th>
                  </tr>
                </thead>
                <tbody className="divide-y divide-gray-100 dark:divide-gray-700">
                  {filteredExpenses.map((expense) => (
                    <tr key={expense.id} className="hover:bg-gray-50 dark:hover:bg-gray-700/50">
                      <td className="px-4 py-3">
                        <div className="flex items-center gap-2">
                          {getCategoryIcon(expense.category)}
                          <span className="font-medium text-gray-900 dark:text-white">{expense.name}</span>
                        </div>
                      </td>
                      <td className="px-4 py-3 text-gray-600 dark:text-gray-400">
                        {expense.category_display}
                      </td>
                      <td className="px-4 py-3 font-medium text-gray-900 dark:text-white">
                        {formatCurrency(expense.amount)}
                      </td>
                      <td className="px-4 py-3">
                        <span className={`px-2 py-1 text-xs rounded-full ${
                          expense.recurring === 'none' 
                            ? 'bg-gray-100 text-gray-600 dark:bg-gray-700' 
                            : 'bg-blue-100 text-blue-600 dark:bg-blue-900/30'
                        }`}>
                          {expense.recurring_display}
                        </span>
                      </td>
                      <td className="px-4 py-3 text-gray-600 dark:text-gray-400">
                        {expense.vendor || '-'}
                      </td>
                      <td className="px-4 py-3 text-gray-600 dark:text-gray-400">
                        {new Date(expense.expense_date).toLocaleDateString()}
                      </td>
                      <td className="px-4 py-3">
                        {expense.is_paid ? (
                          <FaCheck className="text-green-500" />
                        ) : (
                          <FaTimes className="text-red-500" />
                        )}
                      </td>
                      <td className="px-4 py-3">
                        <div className="flex items-center gap-2">
                          <button
                            onClick={() => handleEdit(expense)}
                            className="p-1.5 text-blue-600 hover:bg-blue-50 dark:hover:bg-blue-900/20 rounded"
                          >
                            <FaEdit />
                          </button>
                          <button
                            onClick={() => handleDelete(expense.id)}
                            disabled={actionLoading === expense.id}
                            className="p-1.5 text-red-600 hover:bg-red-50 dark:hover:bg-red-900/20 rounded"
                          >
                            {actionLoading === expense.id ? <FaSpinner className="animate-spin" /> : <FaTrash />}
                          </button>
                        </div>
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            )}
          </div>
        </div>

        {/* Add/Edit Form Modal */}
        {showForm && (
          <div className="fixed inset-0 bg-black/50 flex items-center justify-center z-50 p-4">
            <div className="bg-white dark:bg-gray-800 rounded-xl shadow-2xl max-w-lg w-full max-h-[90vh] overflow-y-auto">
              <div className="p-6">
                <div className="flex items-center justify-between mb-6">
                  <h2 className="text-xl font-semibold text-gray-900 dark:text-white">
                    {editingExpense ? 'Edit Expense' : 'Add Expense'}
                  </h2>
                  <button onClick={resetForm} className="text-gray-500 hover:text-gray-700">
                    <FaTimes />
                  </button>
                </div>

                <form onSubmit={handleSubmit} className="space-y-4">
                  <div>
                    <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">
                      Name *
                    </label>
                    <input
                      type="text"
                      value={formData.name}
                      onChange={(e) => setFormData({ ...formData, name: e.target.value })}
                      required
                      className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700"
                      placeholder="e.g., cPanel License, Domain Registry"
                    />
                  </div>

                  <div className="grid grid-cols-2 gap-4">
                    <div>
                      <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">
                        Category *
                      </label>
                      <select
                        value={formData.category}
                        onChange={(e) => setFormData({ ...formData, category: e.target.value })}
                        className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700"
                      >
                        {CATEGORY_OPTIONS.map(cat => (
                          <option key={cat.value} value={cat.value}>{cat.label}</option>
                        ))}
                      </select>
                    </div>
                    <div>
                      <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">
                        Amount *
                      </label>
                      <input
                        type="number"
                        step="0.01"
                        value={formData.amount}
                        onChange={(e) => setFormData({ ...formData, amount: e.target.value })}
                        required
                        className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700"
                        placeholder="0.00"
                      />
                    </div>
                  </div>

                  <div className="grid grid-cols-2 gap-4">
                    <div>
                      <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">
                        Recurring
                      </label>
                      <select
                        value={formData.recurring}
                        onChange={(e) => setFormData({ ...formData, recurring: e.target.value })}
                        className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700"
                      >
                        {RECURRING_OPTIONS.map(opt => (
                          <option key={opt.value} value={opt.value}>{opt.label}</option>
                        ))}
                      </select>
                    </div>
                    <div>
                      <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">
                        Vendor
                      </label>
                      <input
                        type="text"
                        value={formData.vendor}
                        onChange={(e) => setFormData({ ...formData, vendor: e.target.value })}
                        className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700"
                        placeholder="e.g., cPanel Inc"
                      />
                    </div>
                  </div>

                  <div className="grid grid-cols-2 gap-4">
                    <div>
                      <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">
                        Expense Date *
                      </label>
                      <input
                        type="date"
                        value={formData.expense_date}
                        onChange={(e) => setFormData({ ...formData, expense_date: e.target.value })}
                        required
                        className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700"
                      />
                    </div>
                    <div>
                      <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">
                        Next Due Date
                      </label>
                      <input
                        type="date"
                        value={formData.next_due_date}
                        onChange={(e) => setFormData({ ...formData, next_due_date: e.target.value })}
                        className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700"
                      />
                    </div>
                  </div>

                  <div>
                    <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">
                      Reference Number
                    </label>
                    <input
                      type="text"
                      value={formData.reference_number}
                      onChange={(e) => setFormData({ ...formData, reference_number: e.target.value })}
                      className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700"
                      placeholder="Invoice/Receipt number"
                    />
                  </div>

                  <div>
                    <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">
                      Notes
                    </label>
                    <textarea
                      value={formData.notes}
                      onChange={(e) => setFormData({ ...formData, notes: e.target.value })}
                      className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700"
                      rows={3}
                      placeholder="Additional notes..."
                    />
                  </div>

                  <div className="flex items-center gap-2">
                    <input
                      type="checkbox"
                      id="is_paid"
                      checked={formData.is_paid}
                      onChange={(e) => setFormData({ ...formData, is_paid: e.target.checked })}
                      className="rounded"
                    />
                    <label htmlFor="is_paid" className="text-sm text-gray-700 dark:text-gray-300">
                      Already Paid
                    </label>
                  </div>

                  <div className="flex gap-3 pt-4">
                    <button
                      type="button"
                      onClick={resetForm}
                      className="flex-1 px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg hover:bg-gray-50 dark:hover:bg-gray-700"
                    >
                      Cancel
                    </button>
                    <button
                      type="submit"
                      disabled={actionLoading === -1}
                      className="flex-1 px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 disabled:opacity-50"
                    >
                      {actionLoading === -1 ? (
                        <FaSpinner className="animate-spin mx-auto" />
                      ) : editingExpense ? 'Update' : 'Add Expense'}
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
