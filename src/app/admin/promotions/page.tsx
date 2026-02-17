'use client';

import { useState, useEffect, useCallback } from 'react';
import AdminLayout from '@/components/admin/AdminLayout';
import { apiService } from '@/lib/api-service';
import { 
  FaTag, FaSpinner, FaSearch, FaPlus, FaEdit, FaTrash, 
  FaTimes, FaPercent, FaGift, FaDollarSign, FaCalendarAlt,
  FaToggleOn, FaToggleOff, FaEye
} from 'react-icons/fa';

interface Promotion {
  id: number;
  name: string;
  code?: string;
  promotion_type: string;
  promotion_type_display: string;
  discount_type: string;
  discount_type_display: string;
  discount_value: number;
  description?: string;
  start_date: string;
  end_date: string;
  is_active: boolean;
  usage_limit?: number;
  usage_count: number;
  minimum_order_amount?: number;
  applicable_services?: number[];
  applicable_categories?: string[];
  bundle_services?: number[];
  free_service?: number;
  free_service_name?: string;
  free_service_duration?: number;
  created_at: string;
}

interface Service {
  id: number;
  name: string;
  category: string;
}

const PROMOTION_TYPES = [
  { value: 'coupon', label: 'Coupon Code' },
  { value: 'sale', label: 'Sale/Discount' },
  { value: 'bundle', label: 'Bundle Deal' },
  { value: 'free_service', label: 'Free Service with Purchase' },
];

const DISCOUNT_TYPES = [
  { value: 'percentage', label: 'Percentage Off' },
  { value: 'fixed', label: 'Fixed Amount Off' },
  { value: 'free', label: 'Free Item' },
];

const CATEGORIES = [
  { value: 'hosting', label: 'Hosting' },
  { value: 'domain', label: 'Domains' },
  { value: 'development', label: 'Web Development' },
  { value: 'design', label: 'Design' },
  { value: 'ssl', label: 'SSL Certificates' },
];

// Characters for generating coupon codes (excludes ambiguous chars like 0/O, 1/I/L)
const COUPON_CODE_CHARACTERS = 'ABCDEFGHJKLMNPQRSTUVWXYZ23456789';

const normalizeList = <T,>(data: unknown): T[] => {
  if (Array.isArray(data)) return data as T[];

  if (data && typeof data === 'object' && 'results' in data) {
    const results = (data as { results?: unknown }).results;
    return Array.isArray(results) ? (results as T[]) : [];
  }

  return [];
};

export default function PromotionsPage() {
  const [promotions, setPromotions] = useState<Promotion[]>([]);
  const [services, setServices] = useState<Service[]>([]);
  const [loading, setLoading] = useState(true);
  const [searchTerm, setSearchTerm] = useState('');
  const [typeFilter, setTypeFilter] = useState('');
  const [showForm, setShowForm] = useState(false);
  const [showDetails, setShowDetails] = useState(false);
  const [editingPromotion, setEditingPromotion] = useState<Promotion | null>(null);
  const [selectedPromotion, setSelectedPromotion] = useState<Promotion | null>(null);
  const [actionLoading, setActionLoading] = useState<number | null>(null);

  // Form state
  const [formData, setFormData] = useState({
    name: '',
    code: '',
    promotion_type: 'coupon',
    discount_type: 'percentage',
    discount_value: '',
    description: '',
    start_date: new Date().toISOString().split('T')[0],
    end_date: new Date(Date.now() + 30 * 24 * 60 * 60 * 1000).toISOString().split('T')[0],
    is_active: true,
    usage_limit: '',
    minimum_order_amount: '',
    applicable_categories: [] as string[],
    applicable_services: [] as number[],
    bundle_services: [] as number[],
    free_service: '',
    free_service_duration: '12',
  });

  const fetchPromotions = useCallback(async () => {
    try {
      const params: { promotion_type?: string } = {};
      if (typeFilter) params.promotion_type = typeFilter;
      const response = await apiService.getPromotions(params);
      if (response.data) {
        setPromotions(normalizeList<Promotion>(response.data));
      } else {
        setPromotions([]);
      }
    } catch (error) {
      console.error('Failed to fetch promotions:', error);
      setPromotions([]);
    } finally {
      setLoading(false);
    }
  }, [typeFilter]);

  const fetchServices = useCallback(async () => {
    try {
      const response = await apiService.getServices();
      if (response.data) {
        setServices(normalizeList<Service>(response.data));
      } else {
        setServices([]);
      }
    } catch (error) {
      console.error('Failed to fetch services:', error);
      setServices([]);
    }
  }, []);

  useEffect(() => {
    fetchPromotions();
    fetchServices();
  }, [fetchPromotions, fetchServices]);

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setActionLoading(-1);

    try {
      const data = {
        name: formData.name,
        code: formData.code || undefined,
        promotion_type: formData.promotion_type,
        discount_type: formData.discount_type,
        discount_value: parseFloat(formData.discount_value) || 0,
        description: formData.description || undefined,
        start_date: formData.start_date,
        end_date: formData.end_date,
        is_active: formData.is_active,
        usage_limit: formData.usage_limit ? parseInt(formData.usage_limit) : undefined,
        minimum_order_amount: formData.minimum_order_amount ? parseFloat(formData.minimum_order_amount) : undefined,
        applicable_categories: formData.applicable_categories.length > 0 ? formData.applicable_categories : undefined,
        applicable_services: formData.applicable_services.length > 0 ? formData.applicable_services : undefined,
        bundle_services: formData.bundle_services.length > 0 ? formData.bundle_services : undefined,
        free_service: formData.free_service ? parseInt(formData.free_service) : undefined,
        free_service_duration: formData.free_service_duration ? parseInt(formData.free_service_duration) : undefined,
      };

      if (editingPromotion) {
        await apiService.updatePromotion(editingPromotion.id, data);
      } else {
        await apiService.createPromotion(data);
      }

      fetchPromotions();
      resetForm();
    } catch (error) {
      console.error('Failed to save promotion:', error);
    } finally {
      setActionLoading(null);
    }
  };

  const handleDelete = async (id: number) => {
    if (!confirm('Are you sure you want to delete this promotion?')) return;

    setActionLoading(id);
    try {
      await apiService.deletePromotion(id);
      fetchPromotions();
    } catch (error) {
      console.error('Failed to delete promotion:', error);
    } finally {
      setActionLoading(null);
    }
  };

  const handleToggleActive = async (promotion: Promotion) => {
    setActionLoading(promotion.id);
    try {
      await apiService.updatePromotion(promotion.id, { is_active: !promotion.is_active });
      fetchPromotions();
    } catch (error) {
      console.error('Failed to toggle promotion:', error);
    } finally {
      setActionLoading(null);
    }
  };

  const handleEdit = (promotion: Promotion) => {
    setEditingPromotion(promotion);
    setFormData({
      name: promotion.name,
      code: promotion.code || '',
      promotion_type: promotion.promotion_type,
      discount_type: promotion.discount_type,
      discount_value: promotion.discount_value.toString(),
      description: promotion.description || '',
      start_date: promotion.start_date,
      end_date: promotion.end_date,
      is_active: promotion.is_active,
      usage_limit: promotion.usage_limit?.toString() || '',
      minimum_order_amount: promotion.minimum_order_amount?.toString() || '',
      applicable_categories: promotion.applicable_categories || [],
      applicable_services: promotion.applicable_services || [],
      bundle_services: promotion.bundle_services || [],
      free_service: promotion.free_service?.toString() || '',
      free_service_duration: promotion.free_service_duration?.toString() || '12',
    });
    setShowForm(true);
  };

  const handleViewDetails = (promotion: Promotion) => {
    setSelectedPromotion(promotion);
    setShowDetails(true);
  };

  const resetForm = () => {
    setShowForm(false);
    setEditingPromotion(null);
    setFormData({
      name: '',
      code: '',
      promotion_type: 'coupon',
      discount_type: 'percentage',
      discount_value: '',
      description: '',
      start_date: new Date().toISOString().split('T')[0],
      end_date: new Date(Date.now() + 30 * 24 * 60 * 60 * 1000).toISOString().split('T')[0],
      is_active: true,
      usage_limit: '',
      minimum_order_amount: '',
      applicable_categories: [],
      applicable_services: [],
      bundle_services: [],
      free_service: '',
      free_service_duration: '12',
    });
  };

  const generateCouponCode = () => {
    let code = '';
    for (let i = 0; i < 8; i++) {
      code += COUPON_CODE_CHARACTERS.charAt(Math.floor(Math.random() * COUPON_CODE_CHARACTERS.length));
    }
    setFormData({ ...formData, code });
  };

  const formatDiscount = (promotion: Promotion) => {
    if (promotion.discount_type === 'percentage') {
      return `${promotion.discount_value}% off`;
    } else if (promotion.discount_type === 'fixed') {
      return `$${promotion.discount_value} off`;
    } else {
      return 'Free';
    }
  };

  const filteredPromotions = promotions.filter(promo =>
    promo.name.toLowerCase().includes(searchTerm.toLowerCase()) ||
    promo.code?.toLowerCase().includes(searchTerm.toLowerCase())
  );

  // Stats
  const activeCount = promotions.filter(p => p.is_active).length;
  const couponCount = promotions.filter(p => p.promotion_type === 'coupon').length;
  const totalUsage = promotions.reduce((sum, p) => sum + p.usage_count, 0);

  return (
    <AdminLayout>
      <div className="space-y-6">
        {/* Header */}
        <div className="flex flex-col lg:flex-row lg:items-center lg:justify-between gap-4">
          <div>
            <h1 className="text-2xl font-bold text-gray-900 dark:text-white flex items-center gap-2">
              <FaTag />
              Promotions & Coupons
            </h1>
            <p className="text-gray-500 dark:text-gray-400">
              Manage discounts, coupons, and promotional offers
            </p>
          </div>

          <div className="flex flex-wrap items-center gap-3">
            <div className="px-3 py-1.5 bg-green-50 dark:bg-green-900/20 rounded-lg">
              <span className="text-lg font-bold text-green-600">{activeCount}</span>
              <span className="text-sm text-gray-500 ml-1">Active</span>
            </div>
            <div className="px-3 py-1.5 bg-blue-50 dark:bg-blue-900/20 rounded-lg">
              <span className="text-lg font-bold text-blue-600">{couponCount}</span>
              <span className="text-sm text-gray-500 ml-1">Coupons</span>
            </div>
            <div className="px-3 py-1.5 bg-purple-50 dark:bg-purple-900/20 rounded-lg">
              <span className="text-lg font-bold text-purple-600">{totalUsage}</span>
              <span className="text-sm text-gray-500 ml-1">Uses</span>
            </div>
            <button
              onClick={() => setShowForm(true)}
              className="flex items-center gap-2 px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors"
            >
              <FaPlus /> Create Promotion
            </button>
          </div>
        </div>

        {/* Filters */}
        <div className="flex flex-wrap gap-4 items-center">
          <div className="relative flex-1 min-w-[200px]">
            <FaSearch className="absolute left-3 top-1/2 -translate-y-1/2 text-gray-400" />
            <input
              type="text"
              placeholder="Search promotions..."
              value={searchTerm}
              onChange={(e) => setSearchTerm(e.target.value)}
              className="w-full pl-10 pr-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-800 text-gray-900 dark:text-white"
            />
          </div>

          <select
            value={typeFilter}
            onChange={(e) => setTypeFilter(e.target.value)}
            className="px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-800 text-gray-900 dark:text-white"
          >
            <option value="">All Types</option>
            {PROMOTION_TYPES.map(type => (
              <option key={type.value} value={type.value}>{type.label}</option>
            ))}
          </select>
        </div>

        {/* Promotions Table */}
        {loading ? (
          <div className="flex items-center justify-center py-12">
            <FaSpinner className="animate-spin h-8 w-8 text-blue-600" />
          </div>
        ) : (
          <div className="bg-white dark:bg-gray-800 rounded-xl shadow-lg overflow-hidden">
            <div className="overflow-x-auto">
              <table className="w-full">
                <thead className="bg-gray-50 dark:bg-gray-700">
                  <tr>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider">
                      Promotion
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider">
                      Type
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider">
                      Discount
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider">
                      Validity
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider">
                      Usage
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider">
                      Status
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 dark:text-gray-400 uppercase tracking-wider">
                      Actions
                    </th>
                  </tr>
                </thead>
                <tbody className="divide-y divide-gray-200 dark:divide-gray-700">
                  {filteredPromotions.length === 0 ? (
                    <tr>
                      <td colSpan={7} className="px-6 py-12 text-center text-gray-500 dark:text-gray-400">
                        <FaTag className="h-8 w-8 mx-auto mb-2 opacity-50" />
                        No promotions found
                      </td>
                    </tr>
                  ) : (
                    filteredPromotions.map((promo) => (
                      <tr key={promo.id} className="hover:bg-gray-50 dark:hover:bg-gray-700/50">
                        <td className="px-6 py-4">
                          <div>
                            <p className="font-medium text-gray-900 dark:text-white">
                              {promo.name}
                            </p>
                            {promo.code && (
                              <p className="text-sm text-blue-600 dark:text-blue-400 font-mono">
                                {promo.code}
                              </p>
                            )}
                          </div>
                        </td>
                        <td className="px-6 py-4">
                          <span className={`px-2 py-1 rounded-full text-xs font-medium ${
                            promo.promotion_type === 'coupon' ? 'bg-blue-100 text-blue-800 dark:bg-blue-900/30 dark:text-blue-400' :
                            promo.promotion_type === 'sale' ? 'bg-green-100 text-green-800 dark:bg-green-900/30 dark:text-green-400' :
                            promo.promotion_type === 'bundle' ? 'bg-purple-100 text-purple-800 dark:bg-purple-900/30 dark:text-purple-400' :
                            'bg-yellow-100 text-yellow-800 dark:bg-yellow-900/30 dark:text-yellow-400'
                          }`}>
                            {promo.promotion_type_display || promo.promotion_type}
                          </span>
                        </td>
                        <td className="px-6 py-4">
                          <div className="flex items-center gap-1">
                            {promo.discount_type === 'percentage' ? (
                              <FaPercent className="h-3 w-3 text-gray-400" />
                            ) : promo.discount_type === 'fixed' ? (
                              <FaDollarSign className="h-3 w-3 text-gray-400" />
                            ) : (
                              <FaGift className="h-3 w-3 text-gray-400" />
                            )}
                            <span className="font-medium text-gray-900 dark:text-white">
                              {formatDiscount(promo)}
                            </span>
                          </div>
                        </td>
                        <td className="px-6 py-4 text-sm text-gray-600 dark:text-gray-400">
                          <div className="flex items-center gap-1">
                            <FaCalendarAlt className="h-3 w-3" />
                            {new Date(promo.start_date).toLocaleDateString()} - {new Date(promo.end_date).toLocaleDateString()}
                          </div>
                        </td>
                        <td className="px-6 py-4 text-gray-600 dark:text-gray-400">
                          {promo.usage_count}
                          {promo.usage_limit && <span className="text-gray-400"> / {promo.usage_limit}</span>}
                        </td>
                        <td className="px-6 py-4">
                          <button
                            onClick={() => handleToggleActive(promo)}
                            disabled={actionLoading === promo.id}
                            className={`flex items-center gap-1 px-2 py-1 rounded-full text-sm font-medium transition-colors ${
                              promo.is_active
                                ? 'bg-green-100 text-green-800 dark:bg-green-900/30 dark:text-green-400'
                                : 'bg-gray-100 text-gray-600 dark:bg-gray-700 dark:text-gray-400'
                            }`}
                          >
                            {promo.is_active ? <FaToggleOn className="h-4 w-4" /> : <FaToggleOff className="h-4 w-4" />}
                            {promo.is_active ? 'Active' : 'Inactive'}
                          </button>
                        </td>
                        <td className="px-6 py-4">
                          <div className="flex items-center gap-2">
                            <button
                              onClick={() => handleViewDetails(promo)}
                              className="p-2 text-gray-600 hover:text-blue-600 hover:bg-blue-50 dark:hover:bg-blue-900/20 rounded-lg transition-colors"
                              title="View Details"
                            >
                              <FaEye className="h-4 w-4" />
                            </button>
                            <button
                              onClick={() => handleEdit(promo)}
                              className="p-2 text-gray-600 hover:text-blue-600 hover:bg-blue-50 dark:hover:bg-blue-900/20 rounded-lg transition-colors"
                              title="Edit"
                            >
                              <FaEdit className="h-4 w-4" />
                            </button>
                            <button
                              onClick={() => handleDelete(promo.id)}
                              disabled={actionLoading === promo.id}
                              className="p-2 text-gray-600 hover:text-red-600 hover:bg-red-50 dark:hover:bg-red-900/20 rounded-lg transition-colors disabled:opacity-50"
                              title="Delete"
                            >
                              {actionLoading === promo.id ? (
                                <FaSpinner className="h-4 w-4 animate-spin" />
                              ) : (
                                <FaTrash className="h-4 w-4" />
                              )}
                            </button>
                          </div>
                        </td>
                      </tr>
                    ))
                  )}
                </tbody>
              </table>
            </div>
          </div>
        )}

        {/* Create/Edit Form Modal */}
        {showForm && (
          <div className="fixed inset-0 bg-black/50 flex items-center justify-center z-50 p-4">
            <div className="bg-white dark:bg-gray-800 rounded-xl shadow-2xl max-w-2xl w-full max-h-[90vh] overflow-y-auto">
              <div className="p-6">
                <div className="flex items-center justify-between mb-6">
                  <h2 className="text-xl font-semibold text-gray-900 dark:text-white">
                    {editingPromotion ? 'Edit Promotion' : 'Create Promotion'}
                  </h2>
                  <button onClick={resetForm} className="text-gray-500 hover:text-gray-700">
                    <FaTimes />
                  </button>
                </div>

                <form onSubmit={handleSubmit} className="space-y-4">
                  <div className="grid grid-cols-2 gap-4">
                    <div className="col-span-2">
                      <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">
                        Promotion Name *
                      </label>
                      <input
                        type="text"
                        value={formData.name}
                        onChange={(e) => setFormData({ ...formData, name: e.target.value })}
                        required
                        className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-white"
                        placeholder="e.g., Summer Sale 2024"
                      />
                    </div>

                    <div>
                      <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">
                        Promotion Type *
                      </label>
                      <select
                        value={formData.promotion_type}
                        onChange={(e) => setFormData({ ...formData, promotion_type: e.target.value })}
                        className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-white"
                      >
                        {PROMOTION_TYPES.map(type => (
                          <option key={type.value} value={type.value}>{type.label}</option>
                        ))}
                      </select>
                    </div>

                    {formData.promotion_type === 'coupon' && (
                      <div>
                        <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">
                          Coupon Code
                        </label>
                        <div className="flex gap-2">
                          <input
                            type="text"
                            value={formData.code}
                            onChange={(e) => setFormData({ ...formData, code: e.target.value.toUpperCase() })}
                            className="flex-1 px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-white font-mono"
                            placeholder="SUMMER20"
                          />
                          <button
                            type="button"
                            onClick={generateCouponCode}
                            className="px-3 py-2 bg-gray-100 dark:bg-gray-600 rounded-lg hover:bg-gray-200 dark:hover:bg-gray-500 text-sm"
                          >
                            Generate
                          </button>
                        </div>
                      </div>
                    )}

                    <div>
                      <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">
                        Discount Type *
                      </label>
                      <select
                        value={formData.discount_type}
                        onChange={(e) => setFormData({ ...formData, discount_type: e.target.value })}
                        className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-white"
                      >
                        {DISCOUNT_TYPES.map(type => (
                          <option key={type.value} value={type.value}>{type.label}</option>
                        ))}
                      </select>
                    </div>

                    <div>
                      <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">
                        Discount Value *
                      </label>
                      <input
                        type="number"
                        value={formData.discount_value}
                        onChange={(e) => setFormData({ ...formData, discount_value: e.target.value })}
                        required
                        min="0"
                        step="0.01"
                        className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-white"
                        placeholder={formData.discount_type === 'percentage' ? '20' : '10.00'}
                      />
                    </div>

                    <div>
                      <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">
                        Start Date *
                      </label>
                      <input
                        type="date"
                        value={formData.start_date}
                        onChange={(e) => setFormData({ ...formData, start_date: e.target.value })}
                        required
                        className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-white"
                      />
                    </div>

                    <div>
                      <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">
                        End Date *
                      </label>
                      <input
                        type="date"
                        value={formData.end_date}
                        onChange={(e) => setFormData({ ...formData, end_date: e.target.value })}
                        required
                        className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-white"
                      />
                    </div>

                    <div>
                      <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">
                        Usage Limit
                      </label>
                      <input
                        type="number"
                        value={formData.usage_limit}
                        onChange={(e) => setFormData({ ...formData, usage_limit: e.target.value })}
                        min="0"
                        className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-white"
                        placeholder="Unlimited"
                      />
                    </div>

                    <div>
                      <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">
                        Minimum Order Amount
                      </label>
                      <input
                        type="number"
                        value={formData.minimum_order_amount}
                        onChange={(e) => setFormData({ ...formData, minimum_order_amount: e.target.value })}
                        min="0"
                        step="0.01"
                        className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-white"
                        placeholder="No minimum"
                      />
                    </div>

                    <div className="col-span-2">
                      <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">
                        Applicable Categories
                      </label>
                      <div className="flex flex-wrap gap-2">
                        {CATEGORIES.map(cat => (
                          <label
                            key={cat.value}
                            className={`flex items-center gap-2 px-3 py-1.5 rounded-lg cursor-pointer transition-colors ${
                              formData.applicable_categories.includes(cat.value)
                                ? 'bg-blue-100 text-blue-800 dark:bg-blue-900/30 dark:text-blue-400'
                                : 'bg-gray-100 text-gray-600 dark:bg-gray-700 dark:text-gray-400'
                            }`}
                          >
                            <input
                              type="checkbox"
                              checked={formData.applicable_categories.includes(cat.value)}
                              onChange={(e) => {
                                if (e.target.checked) {
                                  setFormData({ ...formData, applicable_categories: [...formData.applicable_categories, cat.value] });
                                } else {
                                  setFormData({ ...formData, applicable_categories: formData.applicable_categories.filter(c => c !== cat.value) });
                                }
                              }}
                              className="hidden"
                            />
                            {cat.label}
                          </label>
                        ))}
                      </div>
                      <p className="text-xs text-gray-500 mt-1">Leave empty to apply to all categories</p>
                    </div>

                    {(formData.promotion_type === 'free_service' || formData.promotion_type === 'bundle') && (
                      <>
                        <div>
                          <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">
                            Free Service
                          </label>
                          <select
                            value={formData.free_service}
                            onChange={(e) => setFormData({ ...formData, free_service: e.target.value })}
                            className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-white"
                          >
                            <option value="">Select service...</option>
                            {services.map(service => (
                              <option key={service.id} value={service.id}>{service.name}</option>
                            ))}
                          </select>
                        </div>
                        <div>
                          <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">
                            Free Duration (months)
                          </label>
                          <input
                            type="number"
                            value={formData.free_service_duration}
                            onChange={(e) => setFormData({ ...formData, free_service_duration: e.target.value })}
                            min="1"
                            className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-white"
                            placeholder="12"
                          />
                        </div>
                      </>
                    )}

                    <div className="col-span-2">
                      <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">
                        Description
                      </label>
                      <textarea
                        value={formData.description}
                        onChange={(e) => setFormData({ ...formData, description: e.target.value })}
                        rows={3}
                        className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-white"
                        placeholder="Describe the promotion..."
                      />
                    </div>

                    <div className="col-span-2">
                      <label className="flex items-center gap-2">
                        <input
                          type="checkbox"
                          checked={formData.is_active}
                          onChange={(e) => setFormData({ ...formData, is_active: e.target.checked })}
                          className="rounded"
                        />
                        <span className="text-sm text-gray-700 dark:text-gray-300">Active (visible and applicable)</span>
                      </label>
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
                      disabled={actionLoading === -1}
                      className="flex-1 px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 disabled:opacity-50 transition-colors"
                    >
                      {actionLoading === -1 ? (
                        <FaSpinner className="animate-spin mx-auto" />
                      ) : editingPromotion ? 'Update Promotion' : 'Create Promotion'}
                    </button>
                  </div>
                </form>
              </div>
            </div>
          </div>
        )}

        {/* Details Modal */}
        {showDetails && selectedPromotion && (
          <div className="fixed inset-0 bg-black/50 flex items-center justify-center z-50 p-4">
            <div className="bg-white dark:bg-gray-800 rounded-xl shadow-2xl max-w-lg w-full">
              <div className="p-6">
                <div className="flex items-center justify-between mb-6">
                  <h2 className="text-xl font-semibold text-gray-900 dark:text-white">
                    Promotion Details
                  </h2>
                  <button onClick={() => setShowDetails(false)} className="text-gray-500 hover:text-gray-700">
                    <FaTimes />
                  </button>
                </div>

                <div className="space-y-4">
                  <div className="flex items-center gap-4 pb-4 border-b border-gray-200 dark:border-gray-700">
                    <div className="w-12 h-12 bg-gradient-to-br from-blue-500 to-purple-600 rounded-full flex items-center justify-center">
                      <FaTag className="h-6 w-6 text-white" />
                    </div>
                    <div className="flex-1">
                      <h3 className="font-semibold text-gray-900 dark:text-white">
                        {selectedPromotion.name}
                      </h3>
                      {selectedPromotion.code && (
                        <p className="text-blue-600 dark:text-blue-400 font-mono text-sm">
                          {selectedPromotion.code}
                        </p>
                      )}
                    </div>
                    <span className={`px-3 py-1 rounded-full text-sm font-medium ${
                      selectedPromotion.is_active
                        ? 'bg-green-100 text-green-800 dark:bg-green-900/30 dark:text-green-400'
                        : 'bg-gray-100 text-gray-600 dark:bg-gray-700 dark:text-gray-400'
                    }`}>
                      {selectedPromotion.is_active ? 'Active' : 'Inactive'}
                    </span>
                  </div>

                  <div className="grid grid-cols-2 gap-4">
                    <div>
                      <p className="text-sm text-gray-500 dark:text-gray-400">Type</p>
                      <p className="font-medium text-gray-900 dark:text-white">
                        {selectedPromotion.promotion_type_display || selectedPromotion.promotion_type}
                      </p>
                    </div>
                    <div>
                      <p className="text-sm text-gray-500 dark:text-gray-400">Discount</p>
                      <p className="font-medium text-gray-900 dark:text-white">
                        {formatDiscount(selectedPromotion)}
                      </p>
                    </div>
                    <div>
                      <p className="text-sm text-gray-500 dark:text-gray-400">Valid From</p>
                      <p className="font-medium text-gray-900 dark:text-white">
                        {new Date(selectedPromotion.start_date).toLocaleDateString()}
                      </p>
                    </div>
                    <div>
                      <p className="text-sm text-gray-500 dark:text-gray-400">Valid Until</p>
                      <p className="font-medium text-gray-900 dark:text-white">
                        {new Date(selectedPromotion.end_date).toLocaleDateString()}
                      </p>
                    </div>
                    <div>
                      <p className="text-sm text-gray-500 dark:text-gray-400">Usage</p>
                      <p className="font-medium text-gray-900 dark:text-white">
                        {selectedPromotion.usage_count}
                        {selectedPromotion.usage_limit && ` / ${selectedPromotion.usage_limit}`}
                      </p>
                    </div>
                    {selectedPromotion.minimum_order_amount && (
                      <div>
                        <p className="text-sm text-gray-500 dark:text-gray-400">Min. Order</p>
                        <p className="font-medium text-gray-900 dark:text-white">
                          ${selectedPromotion.minimum_order_amount}
                        </p>
                      </div>
                    )}
                  </div>

                  {selectedPromotion.description && (
                    <div>
                      <p className="text-sm text-gray-500 dark:text-gray-400">Description</p>
                      <p className="font-medium text-gray-900 dark:text-white">
                        {selectedPromotion.description}
                      </p>
                    </div>
                  )}

                  {selectedPromotion.free_service_name && (
                    <div className="bg-green-50 dark:bg-green-900/20 rounded-lg p-4">
                      <p className="text-sm text-green-600 dark:text-green-400 font-medium">
                        Free: {selectedPromotion.free_service_name} for {selectedPromotion.free_service_duration} months
                      </p>
                    </div>
                  )}

                  <div className="flex gap-3 pt-4">
                    <button
                      onClick={() => {
                        setShowDetails(false);
                        handleEdit(selectedPromotion);
                      }}
                      className="flex-1 flex items-center justify-center gap-2 px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors"
                    >
                      <FaEdit /> Edit
                    </button>
                    <button
                      onClick={() => setShowDetails(false)}
                      className="flex-1 px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg hover:bg-gray-50 dark:hover:bg-gray-700 text-gray-700 dark:text-gray-300 transition-colors"
                    >
                      Close
                    </button>
                  </div>
                </div>
              </div>
            </div>
          </div>
        )}
      </div>
    </AdminLayout>
  );
}
