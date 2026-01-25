'use client';

import { useState, useEffect } from 'react';
import { FaGlobe, FaSearch, FaShieldAlt, FaLock, FaCheck, FaArrowRight } from 'react-icons/fa';
import { useRouter } from 'next/navigation';
import Link from 'next/link';
import { apiService } from '@/lib/api-service';

interface DomainProduct {
  id: number;
  tld: string;
  description: string;
  registration_price_1yr: string;
  registration_price_2yr: string | null;
  registration_price_3yr: string | null;
  renewal_price: string;
  transfer_price: string;
  whois_privacy_price: string;
  is_featured: boolean;
  is_active: boolean;
}

interface WhoisSearchResult {
  domain: string;
  available: boolean;
  tld: string;
  whoisServer: string;
  message?: string;
  error?: string;
  cached?: boolean;
}

export default function DomainsPage() {
  const router = useRouter();
  const [domainProducts, setDomainProducts] = useState<DomainProduct[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [searchQuery, setSearchQuery] = useState('');
  const [searchResults, setSearchResults] = useState<WhoisSearchResult[]>([]);
  const [searching, setSearching] = useState(false);
  const [actionType, setActionType] = useState<'registration' | 'transfer'>('registration');
  const [supportedTLDs, setSupportedTLDs] = useState<string[]>([]);

  useEffect(() => {
    fetchDomainProducts();
    fetchSupportedTLDs();
  }, []);

  const fetchSupportedTLDs = async () => {
    try {
      const response = await fetch('/api/whois-tlds');
      if (response.ok) {
        const data = await response.json();
        setSupportedTLDs(data.tlds || []);
      }
    } catch (err) {
      console.error('Failed to fetch supported TLDs:', err);
      // Use a default list if fetch fails
      setSupportedTLDs(['com', 'net', 'org', 'io', 'co']);
    }
  };

  const fetchDomainProducts = async () => {
    try {
      setLoading(true);
      const response = await apiService.getDomainProducts();
      if (response.data) {
        const products = Array.isArray(response.data) ? response.data : response.data.results || [];
        setDomainProducts(products);
      } else {
        setError(response.error || 'Failed to load domain products');
      }
    } catch {
      setError('Failed to load domain products');
    } finally {
      setLoading(false);
    }
  };

  const handleSearch = async () => {
    if (!searchQuery.trim()) {
      alert('Please enter a domain name');
      return;
    }

    setSearching(true);
    setSearchResults([]);
    setError(null);

    try {
      // Check if domain has TLD, if not, check popular TLDs
      const domainsToCheck = searchQuery.includes('.') 
        ? [searchQuery.trim()]
        : ['.com', '.net', '.org', '.io', '.co'].map(tld => `${searchQuery.trim()}${tld}`);

      const response = await fetch('/api/domain-search', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({ domains: domainsToCheck }),
      });

      if (!response.ok) {
        const errorData = await response.json();
        throw new Error(errorData.error || 'Search failed');
      }

      const data = await response.json();
      setSearchResults(data.results || []);
    } catch (err) {
      setError(err instanceof Error ? err.message : 'An error occurred');
    } finally {
      setSearching(false);
    }
  };

  if (loading) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <div className="text-xl">Loading domain products...</div>
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
            Domain Services
          </h1>
          <p className="text-xl text-gray-600 dark:text-gray-300 max-w-3xl mx-auto mb-8">
            Register or transfer your domain name with competitive pricing and premium features
          </p>

          {/* Domain Search */}
          <div className="max-w-2xl mx-auto mb-6">
            <div className="flex gap-2">
              <input
                type="text"
                value={searchQuery}
                onChange={(e) => setSearchQuery(e.target.value)}
                placeholder="Search for your domain name (e.g., mysite or mysite.com)..."
                className="flex-1 px-6 py-4 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-800 text-gray-900 dark:text-white text-lg"
                onKeyPress={(e) => e.key === 'Enter' && !searching && handleSearch()}
                disabled={searching}
              />
              <button
                onClick={handleSearch}
                disabled={searching}
                className="bg-blue-600 text-white px-8 py-4 rounded-lg font-bold hover:bg-blue-700 transition-colors flex items-center gap-2 disabled:opacity-50 disabled:cursor-not-allowed"
              >
                {searching ? (
                  <>
                    <svg className="animate-spin h-5 w-5" viewBox="0 0 24 24">
                      <circle className="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" strokeWidth="4" fill="none" />
                      <path className="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z" />
                    </svg>
                    Checking...
                  </>
                ) : (
                  <>
                    <FaSearch />
                    Search
                  </>
                )}
              </button>
            </div>
            <div className="mt-2 text-sm text-gray-600 dark:text-gray-400 text-center">
              <button
                onClick={() => router.push('/services/domains/check')}
                className="text-blue-600 hover:text-blue-700 underline"
              >
                Advanced Search
              </button>
              {' '}| {supportedTLDs.length} TLDs supported via WHOIS
            </div>
          </div>

          {/* Search Results */}
          {searchResults.length > 0 && (
            <div className="max-w-4xl mx-auto mb-8 bg-white dark:bg-gray-800 rounded-lg shadow-lg p-6">
              <h3 className="text-xl font-bold text-gray-900 dark:text-white mb-4">
                Domain Availability Results
              </h3>
              <div className="space-y-2">
                {searchResults.map((result, idx) => {
                  // Find matching product for pricing
                  const tld = '.' + result.domain.split('.').slice(1).join('.');
                  const matchingProduct = domainProducts.find(p => p.tld.toLowerCase() === tld.toLowerCase());
                  
                  return (
                    <div
                      key={idx}
                      className="flex items-center justify-between p-4 border border-gray-200 dark:border-gray-700 rounded-lg"
                    >
                      <div className="flex-1">
                        <span className="font-mono text-lg font-medium text-gray-900 dark:text-white">
                          {result.domain}
                        </span>
                        {result.cached && (
                          <span className="ml-2 text-xs px-2 py-1 bg-blue-100 dark:bg-blue-900 text-blue-700 dark:text-blue-300 rounded">
                            Cached
                          </span>
                        )}
                      </div>
                      <div className="flex items-center gap-4">
                        {result.error ? (
                          <span className="text-sm text-red-600">{result.error}</span>
                        ) : result.available ? (
                          <>
                            <div className="text-right mr-2">
                              <span className="text-sm text-green-600 font-medium flex items-center gap-1">
                                <FaCheck className="text-green-600" />
                                Available
                              </span>
                              {matchingProduct && (
                                <span className="text-lg font-bold text-blue-600">
                                  ${parseFloat(matchingProduct.registration_price_1yr).toFixed(2)}/yr
                                </span>
                              )}
                            </div>
                            <button
                              onClick={() => router.push(`/services/domains/register?domain=${encodeURIComponent(result.domain)}`)}
                              className="px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors text-sm"
                            >
                              Register
                            </button>
                          </>
                        ) : (
                          <div className="flex flex-col items-end gap-1">
                            <span className="text-sm text-gray-600 dark:text-gray-400">
                              Already Registered
                            </span>
                            <Link
                              href={`/services/domains/transfer?domain=${encodeURIComponent(result.domain)}`}
                              className="flex items-center gap-1 text-xs text-blue-600 hover:text-blue-700 hover:underline"
                            >
                              Is it yours? Transfer now
                              <FaArrowRight size={10} />
                            </Link>
                          </div>
                        )}
                      </div>
                    </div>
                  );
                })}
              </div>
            </div>
          )}
        </div>

        {/* Supported TLDs Info */}
        <div className="mb-16 bg-gradient-to-r from-blue-50 to-indigo-50 dark:from-gray-800 dark:to-gray-700 rounded-xl p-8">
          <div className="text-center mb-6">
            <h2 className="text-2xl font-bold text-gray-900 dark:text-white mb-2">
              {supportedTLDs.length}+ TLDs Available via WHOIS
            </h2>
            <p className="text-gray-600 dark:text-gray-300">
              Check domain availability across popular TLDs instantly
            </p>
          </div>
          <div className="flex flex-wrap justify-center gap-2 max-w-4xl mx-auto">
            {supportedTLDs.slice(0, 30).map(tld => (
              <span
                key={tld}
                className="px-3 py-1 bg-white dark:bg-gray-800 text-gray-700 dark:text-gray-300 rounded-full text-sm font-medium shadow-sm"
              >
                .{tld}
              </span>
            ))}
            {supportedTLDs.length > 30 && (
              <span className="px-3 py-1 bg-blue-600 text-white rounded-full text-sm font-medium">
                +{supportedTLDs.length - 30} more
              </span>
            )}
          </div>
          <div className="text-center mt-4">
            <button
              onClick={() => router.push('/services/domains/check')}
              className="text-blue-600 dark:text-blue-400 hover:underline text-sm font-medium"
            >
              View all {supportedTLDs.length} supported TLDs →
            </button>
          </div>
        </div>

        {/* Features */}
        <div className="grid md:grid-cols-4 gap-6 mb-16">
          <div className="bg-white dark:bg-gray-800 p-6 rounded-lg shadow-lg text-center">
            <FaGlobe className="w-12 h-12 text-blue-600 mx-auto mb-4" />
            <h3 className="font-bold text-gray-900 dark:text-white mb-2">Wide Selection</h3>
            <p className="text-sm text-gray-600 dark:text-gray-400">{supportedTLDs.length} TLDs via WHOIS</p>
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
        {domainProducts.length === 0 ? (
          <div className="text-center py-12">
            <p className="text-xl text-gray-600 dark:text-gray-400">
              No domain products available yet.
            </p>
          </div>
        ) : (
          <div className="bg-white dark:bg-gray-800 rounded-xl shadow-xl overflow-hidden mb-16">
            <div className="p-8">
              <h2 className="text-2xl font-bold text-gray-900 dark:text-white mb-6">
                Domain Pricing - All Available TLDs
              </h2>
              
              {/* Pricing Table */}
              <div className="overflow-x-auto">
                <table className="w-full">
                  <thead className="bg-gray-50 dark:bg-gray-700">
                    <tr>
                      <th className="text-left px-4 py-3 font-semibold text-gray-900 dark:text-white">TLD</th>
                      <th className="text-left px-4 py-3 font-semibold text-gray-900 dark:text-white">Register (1yr)</th>
                      <th className="text-left px-4 py-3 font-semibold text-gray-900 dark:text-white">Register (2yr)</th>
                      <th className="text-left px-4 py-3 font-semibold text-gray-900 dark:text-white">Renewal</th>
                      <th className="text-left px-4 py-3 font-semibold text-gray-900 dark:text-white">Transfer</th>
                      <th className="text-center px-4 py-3 font-semibold text-gray-900 dark:text-white">Action</th>
                    </tr>
                  </thead>
                  <tbody className="divide-y divide-gray-200 dark:divide-gray-700">
                    {domainProducts.map((domain) => (
                      <tr 
                        key={domain.id} 
                        className={`hover:bg-gray-50 dark:hover:bg-gray-700 ${
                          domain.is_featured ? 'bg-blue-50 dark:bg-blue-900/10' : ''
                        }`}
                      >
                        <td className="px-4 py-4">
                          <div className="flex items-center gap-2">
                            <span className="text-lg font-bold text-gray-900 dark:text-white">
                              {domain.tld}
                            </span>
                            {domain.is_featured && (
                              <span className="bg-blue-600 text-white text-xs px-2 py-0.5 rounded">
                                Popular
                              </span>
                            )}
                          </div>
                          {domain.description && (
                            <p className="text-xs text-gray-500 dark:text-gray-400 mt-1">
                              {domain.description}
                            </p>
                          )}
                        </td>
                        <td className="px-4 py-4">
                          <span className="text-lg font-bold text-blue-600">
                            ${parseFloat(domain.registration_price_1yr).toFixed(2)}
                          </span>
                        </td>
                        <td className="px-4 py-4">
                          <span className="text-gray-900 dark:text-white">
                            {domain.registration_price_2yr 
                              ? `$${parseFloat(domain.registration_price_2yr).toFixed(2)}`
                              : '-'}
                          </span>
                        </td>
                        <td className="px-4 py-4">
                          <span className="text-gray-900 dark:text-white">
                            ${parseFloat(domain.renewal_price).toFixed(2)}
                          </span>
                        </td>
                        <td className="px-4 py-4">
                          <span className="text-gray-900 dark:text-white">
                            ${parseFloat(domain.transfer_price).toFixed(2)}
                          </span>
                        </td>
                        <td className="px-4 py-4 text-center">
                          <button
                            onClick={() => {
                              if (actionType === 'registration') {
                                router.push(`/services/domains/register?domain=example${domain.tld}`);
                              } else {
                                router.push(`/services/domains/transfer?domain=example${domain.tld}`);
                              }
                            }}
                            className="px-4 py-2 bg-blue-600 text-white rounded-lg font-medium hover:bg-blue-700 transition-colors text-sm"
                          >
                            {actionType === 'registration' ? 'Register' : 'Transfer'}
                          </button>
                        </td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            </div>
          </div>
        )}
      </div>
    </div>
  );
}
