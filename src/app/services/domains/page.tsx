'use client';

import { useState } from 'react';
import { FaGlobe, FaSearch, FaShieldAlt, FaLock, FaCheck } from 'react-icons/fa';
import { useCartStore } from '@/lib/stores/cart-store';
import { useAuthStore } from '@/lib/stores/auth-store';

const domainPricing = [
  { tld: '.com', price: 12.99, popular: true },
  { tld: '.net', price: 13.99, popular: false },
  { tld: '.org', price: 12.99, popular: false },
  { tld: '.co.zw', price: 15.00, popular: true },
  { tld: '.io', price: 39.99, popular: false },
  { tld: '.tech', price: 24.99, popular: false },
];

export default function DomainsPage() {
  const [searchQuery, setSearchQuery] = useState('');
  const [actionType, setActionType] = useState<'registration' | 'transfer'>('registration');
  const [domainName, setDomainName] = useState('');
  const [eppCode, setEppCode] = useState('');
  const [showModal, setShowModal] = useState(false);
  
  const { addItem } = useCartStore();
  const { token } = useAuthStore();

  const handleSearch = () => {
    if (searchQuery.trim()) {
      alert(`Checking availability for: ${searchQuery}`);
      // In production, this would check domain availability via API
    }
  };

  const handleAddToCart = async (tld: string, price: number) => {
    if (!domainName.trim()) {
      alert('Please enter a domain name');
      return;
    }

    if (actionType === 'transfer' && !eppCode.trim()) {
      alert('EPP/Auth code is required for domain transfers');
      return;
    }

    const cartItem = {
      service: 1, // Domain service ID - would be dynamic in production
      service_metadata: {
        action: actionType,
        domain_name: `${domainName}${tld}`,
        ...(actionType === 'transfer' && { epp_code: eppCode }),
      },
      quantity: 1,
      unit_price: price,
      billing_cycle: 'annual',
    };

    const result = await addItem(cartItem, token || undefined);
    
    if (result.success) {
      alert('Domain added to cart successfully!');
      setShowModal(false);
      setDomainName('');
      setEppCode('');
    } else {
      alert(`Failed to add to cart: ${result.error}`);
    }
  };

  return (
    <div className="min-h-screen py-12">
      <div className="max-w-7xl mx-auto px-4">
        {/* Hero Section */}
        <div className="text-center mb-16">
          <h1 className="text-4xl md:text-5xl font-bold text-gray-900 dark:text-white mb-4">
            Domain Services
          </h1>
          <p className="text-xl text-gray-600 dark:text-gray-300 max-w-3xl mx-auto mb-8">
            Register or transfer your domain name with competitive pricing and premium features
          </p>

          {/* Domain Search */}
          <div className="max-w-2xl mx-auto">
            <div className="flex gap-2">
              <input
                type="text"
                value={searchQuery}
                onChange={(e) => setSearchQuery(e.target.value)}
                placeholder="Search for your domain name..."
                className="flex-1 px-6 py-4 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-800 text-gray-900 dark:text-white text-lg"
                onKeyPress={(e) => e.key === 'Enter' && handleSearch()}
              />
              <button
                onClick={handleSearch}
                className="bg-blue-600 text-white px-8 py-4 rounded-lg font-bold hover:bg-blue-700 transition-colors flex items-center gap-2"
              >
                <FaSearch />
                Search
              </button>
            </div>
          </div>
        </div>

        {/* Features */}
        <div className="grid md:grid-cols-4 gap-6 mb-16">
          <div className="bg-white dark:bg-gray-800 p-6 rounded-lg shadow-lg text-center">
            <FaGlobe className="w-12 h-12 text-blue-600 mx-auto mb-4" />
            <h3 className="font-bold text-gray-900 dark:text-white mb-2">Wide Selection</h3>
            <p className="text-sm text-gray-600 dark:text-gray-400">100+ TLDs available</p>
          </div>
          <div className="bg-white dark:bg-gray-800 p-6 rounded-lg shadow-lg text-center">
            <FaShieldAlt className="w-12 h-12 text-blue-600 mx-auto mb-4" />
            <h3 className="font-bold text-gray-900 dark:text-white mb-2">Free WHOIS Privacy</h3>
            <p className="text-sm text-gray-600 dark:text-gray-400">Protect your information</p>
          </div>
          <div className="bg-white dark:bg-gray-800 p-6 rounded-lg shadow-lg text-center">
            <FaLock className="w-12 h-12 text-blue-600 mx-auto mb-4" />
            <h3 className="font-bold text-gray-900 dark:text-white mb-2">Domain Lock</h3>
            <p className="text-sm text-gray-600 dark:text-gray-400">Prevent unauthorized transfers</p>
          </div>
          <div className="bg-white dark:bg-gray-800 p-6 rounded-lg shadow-lg text-center">
            <FaCheck className="w-12 h-12 text-blue-600 mx-auto mb-4" />
            <h3 className="font-bold text-gray-900 dark:text-white mb-2">Easy Management</h3>
            <p className="text-sm text-gray-600 dark:text-gray-400">Intuitive control panel</p>
          </div>
        </div>

        {/* Action Type Selection */}
        <div className="text-center mb-8">
          <div className="inline-flex rounded-lg border border-gray-300 dark:border-gray-600 p-1">
            <button
              onClick={() => setActionType('registration')}
              className={`px-6 py-2 rounded-lg font-medium transition-colors ${
                actionType === 'registration'
                  ? 'bg-blue-600 text-white'
                  : 'text-gray-700 dark:text-gray-300 hover:bg-gray-100 dark:hover:bg-gray-800'
              }`}
            >
              Register Domain
            </button>
            <button
              onClick={() => setActionType('transfer')}
              className={`px-6 py-2 rounded-lg font-medium transition-colors ${
                actionType === 'transfer'
                  ? 'bg-blue-600 text-white'
                  : 'text-gray-700 dark:text-gray-300 hover:bg-gray-100 dark:hover:bg-gray-800'
              }`}
            >
              Transfer Domain
            </button>
          </div>
        </div>

        {/* Domain Pricing Table */}
        <div className="bg-white dark:bg-gray-800 rounded-xl shadow-xl overflow-hidden mb-16">
          <div className="p-8">
            <h2 className="text-2xl font-bold text-gray-900 dark:text-white mb-6">
              {actionType === 'registration' ? 'Registration' : 'Transfer'} Pricing
            </h2>
            
            <div className="grid md:grid-cols-2 lg:grid-cols-3 gap-4">
              {domainPricing.map((domain) => (
                <div
                  key={domain.tld}
                  className={`border rounded-lg p-6 ${
                    domain.popular
                      ? 'border-blue-600 bg-blue-50 dark:bg-blue-900/20'
                      : 'border-gray-300 dark:border-gray-600'
                  }`}
                >
                  <div className="flex justify-between items-center mb-4">
                    <span className="text-2xl font-bold text-gray-900 dark:text-white">
                      {domain.tld}
                    </span>
                    {domain.popular && (
                      <span className="bg-blue-600 text-white text-xs px-2 py-1 rounded">
                        Popular
                      </span>
                    )}
                  </div>
                  
                  <div className="mb-4">
                    <span className="text-3xl font-bold text-blue-600">
                      ${domain.price}
                    </span>
                    <span className="text-gray-600 dark:text-gray-400">/year</span>
                  </div>

                  <button
                    onClick={() => {
                      setShowModal(true);
                      setDomainName('');
                    }}
                    className="w-full bg-blue-600 text-white py-2 rounded-lg font-medium hover:bg-blue-700 transition-colors"
                  >
                    {actionType === 'registration' ? 'Register' : 'Transfer'}
                  </button>
                </div>
              ))}
            </div>
          </div>
        </div>

        {/* Domain Configuration Modal */}
        {showModal && (
          <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center p-4 z-50">
            <div className="bg-white dark:bg-gray-900 rounded-2xl p-8 max-w-md w-full">
              <h3 className="text-2xl font-bold text-gray-900 dark:text-white mb-6">
                {actionType === 'registration' ? 'Register Domain' : 'Transfer Domain'}
              </h3>

              {/* Domain Name Input */}
              <div className="mb-6">
                <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                  Domain Name
                </label>
                <input
                  type="text"
                  value={domainName}
                  onChange={(e) => setDomainName(e.target.value)}
                  placeholder="example"
                  className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-800 text-gray-900 dark:text-white"
                />
                <p className="text-sm text-gray-500 dark:text-gray-400 mt-1">
                  Enter domain without TLD (e.g., example, not example.com)
                </p>
              </div>

              {/* EPP Code for Transfer */}
              {actionType === 'transfer' && (
                <div className="mb-6">
                  <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                    EPP/Auth Code *
                  </label>
                  <input
                    type="text"
                    value={eppCode}
                    onChange={(e) => setEppCode(e.target.value)}
                    placeholder="Enter EPP/Auth code"
                    className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-800 text-gray-900 dark:text-white"
                  />
                  <p className="text-sm text-gray-500 dark:text-gray-400 mt-1">
                    Required for domain transfers. Contact your current registrar to obtain it.
                  </p>
                </div>
              )}

              <div className="flex gap-4">
                <button
                  onClick={() => {
                    setShowModal(false);
                    setDomainName('');
                    setEppCode('');
                  }}
                  className="flex-1 py-2 border border-gray-300 dark:border-gray-600 rounded-lg text-gray-700 dark:text-gray-300 hover:bg-gray-100 dark:hover:bg-gray-800"
                >
                  Cancel
                </button>
                <button
                  onClick={() => {
                    // For demo, use first domain
                    const selectedDomain = domainPricing[0];
                    handleAddToCart(selectedDomain.tld, selectedDomain.price);
                  }}
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
