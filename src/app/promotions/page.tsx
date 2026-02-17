'use client';

import { useState, useEffect, useCallback } from 'react';
import Link from 'next/link';
import { 
  FaTag, FaSpinner, FaPercent, FaGift, FaDollarSign, 
  FaClock, FaShoppingCart, FaArrowRight
} from 'react-icons/fa';
import { apiService } from '@/lib/api-service';

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
  minimum_order_amount?: number;
  applicable_categories?: string[];
  free_service_name?: string;
  free_service_duration?: number;
}

// Normalize API responses that can be arrays or paginated objects
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
  const [loading, setLoading] = useState(true);
  const [copiedCode, setCopiedCode] = useState<string | null>(null);

  const fetchPromotions = useCallback(async () => {
    try {
      const response = await apiService.getActivePromotions();
      if (response.data) {
        setPromotions(normalizeList<Promotion>(response.data));
      }
    } catch (error) {
      console.error('Failed to fetch promotions:', error);
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => {
    fetchPromotions();
  }, [fetchPromotions]);

  const handleCopyCode = (code: string) => {
    navigator.clipboard.writeText(code);
    setCopiedCode(code);
    setTimeout(() => setCopiedCode(null), 2000);
  };

  const formatDiscount = (promotion: Promotion) => {
    if (promotion.discount_type === 'percentage') {
      return `${promotion.discount_value}% OFF`;
    } else if (promotion.discount_type === 'fixed') {
      return `$${promotion.discount_value} OFF`;
    } else {
      return 'FREE';
    }
  };

  const getTimeRemaining = (endDate: string) => {
    const end = new Date(endDate).getTime();
    const now = Date.now();
    const diff = end - now;
    
    if (diff <= 0) return 'Expired';
    
    const days = Math.floor(diff / (1000 * 60 * 60 * 24));
    const hours = Math.floor((diff % (1000 * 60 * 60 * 24)) / (1000 * 60 * 60));
    
    if (days > 7) {
      return `Ends ${new Date(endDate).toLocaleDateString()}`;
    } else if (days > 0) {
      return `${days} day${days > 1 ? 's' : ''} left`;
    } else {
      return `${hours} hour${hours > 1 ? 's' : ''} left`;
    }
  };

  const getCategoryLabel = (category: string) => {
    const labels: Record<string, string> = {
      hosting: 'Hosting',
      domain: 'Domains',
      development: 'Web Development',
      design: 'Design',
      ssl: 'SSL Certificates',
    };
    return labels[category] || category;
  };

  const getPromotionTypeStyles = (type: string) => {
    switch (type) {
      case 'discount':
        return 'from-blue-500 to-blue-700';
      case 'bundle':
        return 'from-purple-500 to-purple-700';
      case 'free_service':
        return 'from-yellow-500 to-orange-600';
      case 'referral':
        return 'from-green-500 to-green-700';
      default:
        return 'from-gray-500 to-gray-700';
    }
  };

  return (
    <div className="min-h-screen bg-gray-50 dark:bg-gray-900 py-12">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
        {/* Hero Section */}
        <div className="text-center mb-12">
          <div className="inline-flex items-center gap-2 px-4 py-2 bg-blue-100 dark:bg-blue-900/30 text-blue-700 dark:text-blue-300 rounded-full text-sm font-medium mb-4">
            <FaTag className="animate-pulse" />
            Special Offers
          </div>
          <h1 className="text-4xl md:text-5xl font-bold text-gray-900 dark:text-white mb-4">
            Promotions & Deals
          </h1>
          <p className="text-xl text-gray-600 dark:text-gray-400 max-w-2xl mx-auto">
            Save big on web hosting, domain registration, and web development services with our exclusive offers.
          </p>
        </div>

        {/* Loading */}
        {loading ? (
          <div className="flex items-center justify-center py-24">
            <FaSpinner className="animate-spin h-12 w-12 text-blue-600" />
          </div>
        ) : promotions.length === 0 ? (
          <div className="text-center py-24 bg-white dark:bg-gray-800 rounded-2xl shadow-lg">
            <FaTag className="h-16 w-16 text-gray-300 dark:text-gray-600 mx-auto mb-4" />
            <h2 className="text-2xl font-bold text-gray-900 dark:text-white mb-2">
              No Active Promotions
            </h2>
            <p className="text-gray-600 dark:text-gray-400 mb-6">
              Check back soon for amazing deals and offers!
            </p>
            <Link
              href="/services"
              className="inline-flex items-center gap-2 px-6 py-3 bg-blue-600 text-white rounded-lg font-medium hover:bg-blue-700 transition-colors"
            >
              Browse Services <FaArrowRight />
            </Link>
          </div>
        ) : (
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-8">
            {promotions.map((promo) => (
              <div
                key={promo.id}
                className="bg-white dark:bg-gray-800 rounded-2xl shadow-lg overflow-hidden hover:shadow-xl transition-shadow group"
              >
                {/* Header with discount */}
                <div className={`bg-gradient-to-r ${getPromotionTypeStyles(promo.promotion_type)} p-6 text-white relative overflow-hidden`}>
                  <div className="absolute top-0 right-0 w-32 h-32 bg-white/10 rounded-full -translate-y-1/2 translate-x-1/2" />
                  <div className="absolute bottom-0 left-0 w-24 h-24 bg-white/10 rounded-full translate-y-1/2 -translate-x-1/2" />
                  
                  <div className="relative">
                    <div className="flex items-center gap-2 mb-2">
                      {promo.discount_type === 'percentage' ? (
                        <FaPercent className="h-5 w-5" />
                      ) : promo.discount_type === 'fixed' ? (
                        <FaDollarSign className="h-5 w-5" />
                      ) : (
                        <FaGift className="h-5 w-5" />
                      )}
                      <span className="text-sm opacity-90">{promo.promotion_type_display || promo.promotion_type}</span>
                    </div>
                    <div className="text-4xl font-bold mb-1">
                      {formatDiscount(promo)}
                    </div>
                    <div className="text-lg opacity-90">{promo.name}</div>
                  </div>
                </div>

                {/* Content */}
                <div className="p-6">
                  {promo.description && (
                    <p className="text-gray-600 dark:text-gray-400 mb-4">
                      {promo.description}
                    </p>
                  )}

                  {/* Coupon Code */}
                  {promo.code && (
                    <div className="bg-gray-50 dark:bg-gray-700/50 rounded-lg p-4 mb-4">
                      <p className="text-xs text-gray-500 dark:text-gray-400 mb-1">Use code at checkout:</p>
                      <div className="flex items-center justify-between">
                        <span className="font-mono text-xl font-bold text-blue-600 dark:text-blue-400">
                          {promo.code}
                        </span>
                        <button
                          onClick={() => handleCopyCode(promo.code || '')}
                          className="text-sm text-blue-600 hover:text-blue-700 dark:text-blue-400 transition-colors"
                        >
                          {copiedCode === promo.code ? '✓ Copied!' : 'Copy'}
                        </button>
                      </div>
                    </div>
                  )}

                  {/* Free Service */}
                  {promo.free_service_name && (
                    <div className="bg-green-50 dark:bg-green-900/20 rounded-lg p-4 mb-4">
                      <div className="flex items-center gap-2 text-green-700 dark:text-green-400">
                        <FaGift />
                        <span className="font-medium">
                          FREE: {promo.free_service_name} for {promo.free_service_duration} months
                        </span>
                      </div>
                    </div>
                  )}

                  {/* Applicable Categories */}
                  {promo.applicable_categories && promo.applicable_categories.length > 0 && (
                    <div className="flex flex-wrap gap-2 mb-4">
                      {promo.applicable_categories.map(cat => (
                        <span
                          key={cat}
                          className="px-2 py-1 bg-gray-100 dark:bg-gray-700 text-gray-600 dark:text-gray-400 text-xs rounded-full"
                        >
                          {getCategoryLabel(cat)}
                        </span>
                      ))}
                    </div>
                  )}

                  {/* Minimum Order */}
                  {promo.minimum_order_amount && promo.minimum_order_amount > 0 && (
                    <p className="text-sm text-gray-500 dark:text-gray-400 mb-4">
                      Min. order: ${promo.minimum_order_amount}
                    </p>
                  )}

                  {/* Footer */}
                  <div className="flex items-center justify-between pt-4 border-t border-gray-200 dark:border-gray-700">
                    <div className="flex items-center gap-2 text-sm text-gray-500 dark:text-gray-400">
                      <FaClock />
                      <span>{getTimeRemaining(promo.end_date)}</span>
                    </div>
                    <Link
                      href="/services"
                      className="flex items-center gap-2 px-4 py-2 bg-blue-600 text-white rounded-lg font-medium hover:bg-blue-700 transition-colors text-sm group-hover:translate-x-1 duration-200"
                    >
                      <FaShoppingCart />
                      Shop Now
                    </Link>
                  </div>
                </div>
              </div>
            ))}
          </div>
        )}

        {/* Bottom CTA */}
        <div className="mt-16 text-center bg-gradient-to-r from-blue-600 to-purple-600 rounded-2xl p-8 md:p-12">
          <h2 className="text-3xl font-bold text-white mb-4">
            Don&apos;t Miss Out on These Deals!
          </h2>
          <p className="text-lg text-white/80 mb-6 max-w-2xl mx-auto">
            Start building your online presence today with our premium hosting and development services.
          </p>
          <div className="flex flex-wrap justify-center gap-4">
            <Link
              href="/services/hosting"
              className="inline-flex items-center gap-2 px-6 py-3 bg-white text-blue-600 rounded-lg font-bold hover:bg-gray-100 transition-colors"
            >
              View Hosting Plans
            </Link>
            <Link
              href="/services/domains"
              className="inline-flex items-center gap-2 px-6 py-3 bg-white/10 text-white rounded-lg font-bold hover:bg-white/20 transition-colors border border-white/30"
            >
              Register Domain
            </Link>
          </div>
        </div>
      </div>
    </div>
  );
}
