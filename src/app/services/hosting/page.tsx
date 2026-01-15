'use client';

import { useState, useEffect } from 'react';
import { FaServer, FaCloud, FaShieldAlt, FaClock, FaCheck } from 'react-icons/fa';
import { useCartStore } from '@/lib/stores/cart-store';
import { useAuthStore } from '@/lib/stores/auth-store';
import { apiService } from '@/lib/api-service';

interface HostingProduct {
  id: number;
  name: string;
  slug: string;
  description: string;
  hosting_type: string;
  disk_space: number;
  bandwidth: number;
  email_accounts: number;
  databases: number;
  ftp_accounts: number;
  subdomains: number;
  addon_domains: number;
  parked_domains: number;
  ssl_certificate: boolean;
  dedicated_ip: boolean;
  cpanel_access: boolean;
  ssh_access: boolean;
  cron_jobs: boolean;
  backups_included: boolean;
  monthly_price: string;
  quarterly_price: string | null;
  semi_annual_price: string | null;
  annual_price: string | null;
  biennial_price: string | null;
  triennial_price: string | null;
  is_featured: boolean;
  is_active: boolean;
}

export default function HostingPage() {
  const [hostingProducts, setHostingProducts] = useState<HostingProduct[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [selectedProduct, setSelectedProduct] = useState<HostingProduct | null>(null);
  const [selectedRegion, setSelectedRegion] = useState('');
  const [selectedOS, setSelectedOS] = useState('');
  const [billingCycle, setBillingCycle] = useState('monthly');
  const [filterType, setFilterType] = useState<string>('all');
  
  const { addItem } = useCartStore();
  const { token } = useAuthStore();

  useEffect(() => {
    fetchHostingProducts();
  }, []);

  const fetchHostingProducts = async () => {
    try {
      setLoading(true);
      const response = await apiService.getHostingProducts();
      if (response.data) {
        setHostingProducts(response.data);
      } else {
        setError(response.error || 'Failed to load hosting products');
      }
    } catch (err) {
      setError('Failed to load hosting products');
    } finally {
      setLoading(false);
    }
  };

  const formatFeatures = (product: HostingProduct): string[] => {
    const features: string[] = [];
    
    if (product.disk_space === 0) {
      features.push('Unlimited Storage');
    } else {
      features.push(`${Math.floor(product.disk_space / 1024)} GB Storage`);
    }
    
    if (product.bandwidth === 0) {
      features.push('Unlimited Bandwidth');
    } else {
      features.push(`${Math.floor(product.bandwidth / 1024)} GB Bandwidth`);
    }
    
    if (product.email_accounts === 0) {
      features.push('Unlimited Email Accounts');
    } else {
      features.push(`${product.email_accounts} Email Accounts`);
    }
    
    if (product.databases === 0) {
      features.push('Unlimited Databases');
    } else {
      features.push(`${product.databases} Databases`);
    }
    
    if (product.ssl_certificate) features.push('Free SSL Certificate');
    if (product.dedicated_ip) features.push('Dedicated IP Address');
    if (product.cpanel_access) features.push('cPanel Access');
    if (product.ssh_access) features.push('SSH Access');
    if (product.cron_jobs) features.push('Cron Jobs');
    if (product.backups_included) features.push('Daily Backups');
    
    return features;
  };

  const getPrice = (product: HostingProduct): string => {
    switch (billingCycle) {
      case 'quarterly':
        return product.quarterly_price || product.monthly_price;
      case 'semi_annual':
        return product.semi_annual_price || product.monthly_price;
      case 'annual':
        return product.annual_price || product.monthly_price;
      case 'biennial':
        return product.biennial_price || product.monthly_price;
      case 'triennial':
        return product.triennial_price || product.monthly_price;
      default:
        return product.monthly_price;
    }
  };

  const handleAddToCart = async (product: HostingProduct) => {
    if (!selectedRegion) {
      alert('Please select a region');
      return;
    }

    if ((product.hosting_type === 'vps' || product.hosting_type === 'dedicated') && !selectedOS) {
      alert('Please select an operating system');
      return;
    }

    const cartItem = {
      service: product.id,
      service_metadata: {
        type: product.hosting_type,
        region: selectedRegion,
        ...(selectedOS && { os: selectedOS }),
      },
      quantity: 1,
      unit_price: parseFloat(getPrice(product)),
      billing_cycle: billingCycle,
    };

    const result = await addItem(cartItem, token || undefined);
    
    if (result.success) {
      alert('Added to cart successfully!');
      setSelectedProduct(null);
      setSelectedRegion('');
      setSelectedOS('');
    } else {
      alert(`Failed to add to cart: ${result.error}`);
    }
  };

  const filteredProducts = filterType === 'all' 
    ? hostingProducts 
    : hostingProducts.filter(p => p.hosting_type === filterType);

  if (loading) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <div className="text-xl">Loading hosting products...</div>
      </div>
    );
  }

  if (error) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <div className="text-xl text-red-600">{error}</div>
      </div>
    );
  }

  return (
    <div className="min-h-screen py-12">
      <div className="max-w-7xl mx-auto px-4">
        {/* Hero Section */}
        <div className="text-center mb-16">
          <h1 className="text-4xl md:text-5xl font-bold text-gray-900 dark:text-white mb-4">
            Web Hosting Services
          </h1>
          <p className="text-xl text-gray-600 dark:text-gray-300 max-w-3xl mx-auto">
            Reliable, fast, and secure hosting solutions for your websites and applications
          </p>
        </div>

        {/* Features Grid */}
        <div className="grid md:grid-cols-4 gap-6 mb-16">
          <div className="bg-white dark:bg-gray-800 p-6 rounded-lg shadow-lg text-center">
            <FaServer className="w-12 h-12 text-blue-600 mx-auto mb-4" />
            <h3 className="font-bold text-gray-900 dark:text-white mb-2">Latest Hardware</h3>
            <p className="text-sm text-gray-600 dark:text-gray-400">Enterprise-grade servers</p>
          </div>
          <div className="bg-white dark:bg-gray-800 p-6 rounded-lg shadow-lg text-center">
            <FaShieldAlt className="w-12 h-12 text-blue-600 mx-auto mb-4" />
            <h3 className="font-bold text-gray-900 dark:text-white mb-2">DDoS Protection</h3>
            <p className="text-sm text-gray-600 dark:text-gray-400">Advanced security measures</p>
          </div>
          <div className="bg-white dark:bg-gray-800 p-6 rounded-lg shadow-lg text-center">
            <FaClock className="w-12 h-12 text-blue-600 mx-auto mb-4" />
            <h3 className="font-bold text-gray-900 dark:text-white mb-2">99.9% Uptime</h3>
            <p className="text-sm text-gray-600 dark:text-gray-400">Guaranteed availability</p>
          </div>
          <div className="bg-white dark:bg-gray-800 p-6 rounded-lg shadow-lg text-center">
            <FaCloud className="w-12 h-12 text-blue-600 mx-auto mb-4" />
            <h3 className="font-bold text-gray-900 dark:text-white mb-2">Daily Backups</h3>
            <p className="text-sm text-gray-600 dark:text-gray-400">Your data is safe</p>
          </div>
        </div>

        {/* Filter Buttons */}
        <div className="flex justify-center gap-4 mb-8 flex-wrap">
          <button
            onClick={() => setFilterType('all')}
            className={`px-6 py-2 rounded-lg font-medium transition-colors ${
              filterType === 'all'
                ? 'bg-blue-600 text-white'
                : 'bg-white dark:bg-gray-800 text-gray-700 dark:text-gray-300 hover:bg-gray-100 dark:hover:bg-gray-700'
            }`}
          >
            All Hosting
          </button>
          <button
            onClick={() => setFilterType('shared')}
            className={`px-6 py-2 rounded-lg font-medium transition-colors ${
              filterType === 'shared'
                ? 'bg-blue-600 text-white'
                : 'bg-white dark:bg-gray-800 text-gray-700 dark:text-gray-300 hover:bg-gray-100 dark:hover:bg-gray-700'
            }`}
          >
            Shared Hosting
          </button>
          <button
            onClick={() => setFilterType('vps')}
            className={`px-6 py-2 rounded-lg font-medium transition-colors ${
              filterType === 'vps'
                ? 'bg-blue-600 text-white'
                : 'bg-white dark:bg-gray-800 text-gray-700 dark:text-gray-300 hover:bg-gray-100 dark:hover:bg-gray-700'
            }`}
          >
            VPS Hosting
          </button>
          <button
            onClick={() => setFilterType('dedicated')}
            className={`px-6 py-2 rounded-lg font-medium transition-colors ${
              filterType === 'dedicated'
                ? 'bg-blue-600 text-white'
                : 'bg-white dark:bg-gray-800 text-gray-700 dark:text-gray-300 hover:bg-gray-100 dark:hover:bg-gray-700'
            }`}
          >
            Dedicated Servers
          </button>
        </div>

        {/* Hosting Plans */}
        {filteredProducts.length === 0 ? (
          <div className="text-center py-12">
            <p className="text-xl text-gray-600 dark:text-gray-400">
              No hosting products available for this category yet.
            </p>
          </div>
        ) : (
          <div className="grid md:grid-cols-3 gap-8 mb-16">
            {filteredProducts.map((product) => (
              <div
                key={product.id}
                className={`bg-white dark:bg-gray-800 rounded-xl shadow-xl overflow-hidden border-2 ${
                  product.is_featured ? 'border-blue-600' : 'border-transparent'
                } transition-transform hover:scale-105`}
              >
                {product.is_featured && (
                  <div className="bg-blue-600 text-white text-center py-2 font-bold">
                    Most Popular
                  </div>
                )}
                
                <div className="p-8">
                  <h3 className="text-2xl font-bold text-gray-900 dark:text-white mb-2">
                    {product.name}
                  </h3>
                  <p className="text-gray-600 dark:text-gray-400 mb-6">{product.description}</p>
                  
                  <div className="mb-6">
                    <span className="text-4xl font-bold text-blue-600">
                      ${parseFloat(getPrice(product)).toFixed(2)}
                    </span>
                    <span className="text-gray-600 dark:text-gray-400">/month</span>
                  </div>

                  <ul className="space-y-3 mb-8">
                    {formatFeatures(product).map((feature, index) => (
                      <li key={index} className="flex items-start gap-2">
                        <FaCheck className="w-5 h-5 text-green-500 flex-shrink-0 mt-0.5" />
                        <span className="text-gray-700 dark:text-gray-300">{feature}</span>
                      </li>
                    ))}
                  </ul>

                  <button
                    onClick={() => setSelectedProduct(product)}
                    className="w-full bg-blue-600 text-white py-3 rounded-lg font-bold hover:bg-blue-700 transition-colors"
                  >
                    Select Plan
                  </button>
                </div>
              </div>
            ))}
          </div>
        )}

        {/* Configuration Modal */}
        {selectedProduct && (
          <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center p-4 z-50">
            <div className="bg-white dark:bg-gray-900 rounded-2xl p-8 max-w-md w-full">
              <h3 className="text-2xl font-bold text-gray-900 dark:text-white mb-6">
                Configure {selectedProduct.name}
              </h3>

              {/* Region Selection */}
              <div className="mb-6">
                <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                  Select Region
                </label>
                <select
                  value={selectedRegion}
                  onChange={(e) => setSelectedRegion(e.target.value)}
                  className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-800 text-gray-900 dark:text-white"
                >
                  <option value="">Choose a region...</option>
                  <option value="US">United States</option>
                  <option value="EU">Europe</option>
                  <option value="Asia">Asia</option>
                  <option value="Africa">Africa</option>
                </select>
              </div>

              {/* OS Selection (for VPS and Dedicated) */}
              {(selectedProduct.hosting_type === 'vps' || selectedProduct.hosting_type === 'dedicated') && (
                <div className="mb-6">
                  <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                    Select Operating System
                  </label>
                  <select
                    value={selectedOS}
                    onChange={(e) => setSelectedOS(e.target.value)}
                    className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-800 text-gray-900 dark:text-white"
                  >
                    <option value="">Choose an OS...</option>
                    <option value="Ubuntu 22.04">Ubuntu 22.04 LTS</option>
                    <option value="Ubuntu 24.04">Ubuntu 24.04 LTS</option>
                    <option value="CentOS 8">CentOS 8</option>
                    <option value="Debian 11">Debian 11</option>
                    <option value="Debian 12">Debian 12</option>
                    {selectedProduct.hosting_type === 'dedicated' && (
                      <>
                        <option value="Windows Server 2019">Windows Server 2019</option>
                        <option value="Windows Server 2022">Windows Server 2022</option>
                      </>
                    )}
                  </select>
                </div>
              )}

              {/* Billing Cycle */}
              <div className="mb-6">
                <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                  Billing Cycle
                </label>
                <select
                  value={billingCycle}
                  onChange={(e) => setBillingCycle(e.target.value)}
                  className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-800 text-gray-900 dark:text-white"
                >
                  <option value="monthly">Monthly</option>
                  {selectedProduct.quarterly_price && (
                    <option value="quarterly">Quarterly</option>
                  )}
                  {selectedProduct.semi_annual_price && (
                    <option value="semi_annual">Semi-Annual</option>
                  )}
                  {selectedProduct.annual_price && (
                    <option value="annual">Annual</option>
                  )}
                  {selectedProduct.biennial_price && (
                    <option value="biennial">Biennial (2 years)</option>
                  )}
                  {selectedProduct.triennial_price && (
                    <option value="triennial">Triennial (3 years)</option>
                  )}
                </select>
              </div>

              <div className="flex gap-4">
                <button
                  onClick={() => {
                    setSelectedProduct(null);
                    setSelectedRegion('');
                    setSelectedOS('');
                  }}
                  className="flex-1 py-2 border border-gray-300 dark:border-gray-600 rounded-lg text-gray-700 dark:text-gray-300 hover:bg-gray-100 dark:hover:bg-gray-800"
                >
                  Cancel
                </button>
                <button
                  onClick={() => handleAddToCart(selectedProduct)}
                  className="flex-1 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700"
                >
                  Add to Cart
                </button>
              </div>
            </div>
          </div>
        )}
      </div>
    </div>
  );
}
