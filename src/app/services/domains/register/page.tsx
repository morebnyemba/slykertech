'use client';

/**
 * Domain Registration Page
 * Allows users to register an available domain
 */

import { useState, useEffect, Suspense } from 'react';
import { useSearchParams } from 'next/navigation';
import Link from 'next/link';
import { FaGlobe, FaCheckCircle, FaArrowLeft, FaShoppingCart, FaShieldAlt, FaLock, FaServer } from 'react-icons/fa';
import { useAuthStore } from '@/lib/stores/auth-store';
import { useCartStore } from '@/lib/stores/cart-store';
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

function RegisterPageContent() {
  const searchParams = useSearchParams();
  const { token } = useAuthStore();
  const { addItem } = useCartStore();
  
  const [domainName, setDomainName] = useState('');
  const [registrationYears, setRegistrationYears] = useState<1 | 2 | 3>(1);
  const [whoisPrivacy, setWhoisPrivacy] = useState(false);
  const [autoRenew, setAutoRenew] = useState(true);
  const [nameserverOption, setNameserverOption] = useState<'default' | 'custom'>('default');
  const [customNameservers, setCustomNameservers] = useState(['', '']);
  const [isWithHosting, setIsWithHosting] = useState(false);

  const DEFAULT_NAMESERVERS = ['ns1.slykertech.co.zw', 'ns2.slykertech.co.zw'];
  
  const [domainProduct, setDomainProduct] = useState<DomainProduct | null>(null);
  const [allProducts, setAllProducts] = useState<DomainProduct[]>([]);
  const [loading, setLoading] = useState(true);
  const [checking, setChecking] = useState(false);
  const [addingToCart, setAddingToCart] = useState(false);
  const [addedToCart, setAddedToCart] = useState(false);
  const [availabilityStatus, setAvailabilityStatus] = useState<WhoisSearchResult | null>(null);
  const [error, setError] = useState<string | null>(null);

  // Pre-fill domain from URL query parameter and detect hosting context
  useEffect(() => {
    const domainParam = searchParams.get('domain');
    const tldParam = searchParams.get('tld');
    const hostingProductParam = searchParams.get('hosting_product');
    
    if (domainParam) {
      setDomainName(domainParam.toLowerCase());
    } else if (tldParam) {
      // If only TLD is provided, leave domain name empty for user to fill in
      // The domain will be validated when user enters it
    }

    // If coming from hosting purchase, mark as with-hosting and use default nameservers
    if (hostingProductParam) {
      setIsWithHosting(true);
      setNameserverOption('default');
    }
  }, [searchParams]);

  // Fetch domain products on mount
  useEffect(() => {
    const fetchProducts = async () => {
      try {
        setLoading(true);
        const response = await apiService.getDomainProducts();
        if (response.data) {
          const products = Array.isArray(response.data) ? response.data : response.data.results || [];
          setAllProducts(products);
        }
      } catch (err) {
        console.error('Failed to load domain products:', err);
        setError('Failed to load domain products. Please try again later.');
      } finally {
        setLoading(false);
      }
    };
    fetchProducts();
  }, []);

  // Check availability and find matching product when domain name changes
  useEffect(() => {
    if (!domainName || !domainName.includes('.') || allProducts.length === 0) {
      setDomainProduct(null);
      setAvailabilityStatus(null);
      return;
    }

    // Extract TLD from domain name
    const parts = domainName.split('.');
    const tld = '.' + parts.slice(1).join('.');
    
    // Find matching product
    const product = allProducts.find(p => p.tld.toLowerCase() === tld.toLowerCase());
    setDomainProduct(product || null);

    // Check domain availability
    const checkAvailability = async () => {
      setChecking(true);
      setError(null);
      setAvailabilityStatus(null);
      
      try {
        const response = await fetch('/api/domain-search', {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
          },
          body: JSON.stringify({ domains: [domainName] }),
        });

        if (!response.ok) {
          const errorData = await response.json();
          throw new Error(errorData.error || 'Failed to check availability');
        }

        const data = await response.json();
        if (data.results && data.results.length > 0) {
          setAvailabilityStatus(data.results[0]);
        }
      } catch (err) {
        setError(err instanceof Error ? err.message : 'Failed to check domain availability');
      } finally {
        setChecking(false);
      }
    };

    // Debounce the availability check
    const timeoutId = setTimeout(checkAvailability, 500);
    return () => clearTimeout(timeoutId);
  }, [domainName, allProducts]);

  // Helper function to calculate registration price for any number of years
  const calculatePriceForYears = (product: DomainProduct, years: number): number => {
    if (years === 1) {
      return parseFloat(product.registration_price_1yr);
    }
    if (years === 2 && product.registration_price_2yr) {
      return parseFloat(product.registration_price_2yr);
    }
    if (years === 3 && product.registration_price_3yr) {
      return parseFloat(product.registration_price_3yr);
    }
    // Default: multiply 1-year price by number of years
    return parseFloat(product.registration_price_1yr) * years;
  };

  const getPrice = () => {
    if (!domainProduct) return 0;
    return calculatePriceForYears(domainProduct, registrationYears);
  };

  const getWhoisPrivacyPrice = () => {
    if (!domainProduct || !whoisPrivacy) return 0;
    return parseFloat(domainProduct.whois_privacy_price) * registrationYears;
  };

  const getTotalPrice = () => {
    return getPrice() + getWhoisPrivacyPrice();
  };

  const handleAddToCart = async () => {
    if (!domainProduct || !availabilityStatus?.available) {
      setError('Please select an available domain');
      return;
    }

    // Validate custom nameservers if selected
    if (nameserverOption === 'custom' && !isWithHosting) {
      const filledNameservers = customNameservers.filter(ns => ns.trim() !== '');
      if (filledNameservers.length < 2) {
        setError('Please enter at least 2 nameservers');
        return;
      }
      const hostnameRegex = /^[a-z0-9]([a-z0-9-]*[a-z0-9])?(\.[a-z0-9]([a-z0-9-]*[a-z0-9])?)+$/;
      const invalidNs = filledNameservers.find(ns => !hostnameRegex.test(ns.trim()));
      if (invalidNs) {
        setError(`Invalid nameserver format: "${invalidNs.trim()}". Use a valid hostname like ns1.example.com`);
        return;
      }
    }

    setAddingToCart(true);
    setError(null);

    try {
      const nameservers = nameserverOption === 'custom' 
        ? customNameservers.filter(ns => ns.trim() !== '')
        : DEFAULT_NAMESERVERS;

      const cartItem = {
        domain_product: domainProduct.id,
        service_metadata: {
          action: 'registration',
          domain_name: domainName,
          registration_years: registrationYears,
          whois_privacy: whoisPrivacy,
          auto_renew: autoRenew,
          nameservers: nameservers,
          with_hosting: isWithHosting,
        },
        quantity: 1,
        unit_price: getTotalPrice(),
        billing_cycle: registrationYears === 1 ? 'annual' : `${registrationYears}yr`,
      };

      const result = await addItem(cartItem, token || undefined);
      
      if (result.success) {
        setAddedToCart(true);
      } else {
        setError(result.error || 'Failed to add to cart');
      }
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to add to cart');
    } finally {
      setAddingToCart(false);
    }
  };

  if (loading) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <div className="text-xl">Loading...</div>
      </div>
    );
  }

  if (addedToCart) {
    return (
      <div className="min-h-screen py-12 bg-gray-50 dark:bg-gray-900">
        <div className="max-w-2xl mx-auto px-4">
          <div className="bg-white dark:bg-gray-800 rounded-xl shadow-lg p-8 text-center">
            <div className="w-16 h-16 bg-green-100 dark:bg-green-900 rounded-full flex items-center justify-center mx-auto mb-6">
              <FaCheckCircle className="text-green-600 text-3xl" />
            </div>
            <h1 className="text-2xl font-bold text-gray-900 dark:text-white mb-4">
              Domain Added to Cart!
            </h1>
            <p className="text-gray-600 dark:text-gray-300 mb-6">
              <strong>{domainName}</strong> has been added to your cart.
            </p>
            <div className="bg-gray-50 dark:bg-gray-700 rounded-lg p-4 mb-6">
              <div className="flex justify-between text-sm mb-2">
                <span className="text-gray-600 dark:text-gray-400">Domain Registration ({registrationYears} year{registrationYears > 1 ? 's' : ''})</span>
                <span className="font-medium text-gray-900 dark:text-white">${getPrice().toFixed(2)}</span>
              </div>
              {whoisPrivacy && (
                <div className="flex justify-between text-sm mb-2">
                  <span className="text-gray-600 dark:text-gray-400">WHOIS Privacy</span>
                  <span className="font-medium text-gray-900 dark:text-white">${getWhoisPrivacyPrice().toFixed(2)}</span>
                </div>
              )}
              <div className="border-t border-gray-200 dark:border-gray-600 pt-2 mt-2">
                <div className="flex justify-between">
                  <span className="font-semibold text-gray-900 dark:text-white">Total</span>
                  <span className="font-bold text-blue-600">${getTotalPrice().toFixed(2)}</span>
                </div>
              </div>
            </div>
            <div className="flex gap-4 justify-center">
              <Link
                href="/cart"
                className="px-6 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors flex items-center gap-2"
              >
                <FaShoppingCart />
                View Cart
              </Link>
              <Link
                href="/services/domains"
                className="px-6 py-2 border border-gray-300 dark:border-gray-600 text-gray-700 dark:text-gray-300 rounded-lg hover:bg-gray-100 dark:hover:bg-gray-700 transition-colors"
              >
                Register Another
              </Link>
            </div>
          </div>
        </div>
      </div>
    );
  }

  return (
    <div className="min-h-screen py-12 bg-gray-50 dark:bg-gray-900">
      <div className="max-w-3xl mx-auto px-4">
        {/* Back Link */}
        <Link
          href="/services/domains"
          className="inline-flex items-center gap-2 text-gray-600 dark:text-gray-400 hover:text-gray-900 dark:hover:text-white mb-6"
        >
          <FaArrowLeft size={14} />
          Back to Domain Services
        </Link>

        {/* Header */}
        <div className="text-center mb-8">
          <div className="w-16 h-16 bg-blue-100 dark:bg-blue-900 rounded-full flex items-center justify-center mx-auto mb-4">
            <FaGlobe className="text-blue-600 text-2xl" />
          </div>
          <h1 className="text-3xl font-bold text-gray-900 dark:text-white mb-2">
            Register Your Domain
          </h1>
          <p className="text-gray-600 dark:text-gray-300 max-w-xl mx-auto">
            Complete your domain registration with premium features and protection.
          </p>
        </div>

        {/* Registration Form */}
        <div className="bg-white dark:bg-gray-800 rounded-xl shadow-lg p-8">
          {/* Domain Name Input */}
          <div className="mb-6">
            <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
              Domain Name
            </label>
            <div className="relative">
              <FaGlobe className="absolute left-3 top-1/2 transform -translate-y-1/2 text-gray-400" />
              <input
                type="text"
                value={domainName}
                onChange={(e) => setDomainName(e.target.value.toLowerCase())}
                placeholder="example.com"
                className="w-full pl-10 pr-4 py-3 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-white focus:ring-2 focus:ring-blue-500"
              />
            </div>
            
            {/* Availability Status */}
            {checking && (
              <div className="mt-2 flex items-center gap-2 text-gray-600 dark:text-gray-400">
                <svg className="animate-spin h-4 w-4" viewBox="0 0 24 24">
                  <circle className="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" strokeWidth="4" fill="none" />
                  <path className="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z" />
                </svg>
                Checking availability...
              </div>
            )}
            
            {!checking && availabilityStatus && (
              <div className={`mt-2 flex items-center gap-2 ${availabilityStatus.available ? 'text-green-600' : 'text-red-600'}`}>
                {availabilityStatus.available ? (
                  <>
                    <FaCheckCircle />
                    <span className="font-medium">Domain is available!</span>
                  </>
                ) : (
                  <>
                    <span>✗</span>
                    <span className="font-medium">Domain is already registered</span>
                    <Link 
                      href={`/services/domains/transfer?domain=${encodeURIComponent(domainName)}`}
                      className="text-blue-600 hover:underline ml-2"
                    >
                      Transfer instead?
                    </Link>
                  </>
                )}
              </div>
            )}

            {!domainProduct && domainName.includes('.') && !checking && (
              <p className="mt-2 text-yellow-600 dark:text-yellow-400 text-sm">
                This TLD is not available for registration through our service.
              </p>
            )}
          </div>

          {/* Pricing Display */}
          {domainProduct && availabilityStatus?.available && (
            <>
              <div className="bg-blue-50 dark:bg-blue-900/20 border border-blue-200 dark:border-blue-800 rounded-lg p-4 mb-6">
                <div className="flex items-center justify-between">
                  <div>
                    <span className="text-lg font-bold text-gray-900 dark:text-white">{domainName}</span>
                    {domainProduct.is_featured && (
                      <span className="ml-2 px-2 py-0.5 bg-blue-600 text-white text-xs rounded">Popular</span>
                    )}
                  </div>
                  <div className="text-right">
                    <span className="text-2xl font-bold text-blue-600">${getPrice().toFixed(2)}</span>
                    <span className="text-gray-600 dark:text-gray-400">/{registrationYears} year{registrationYears > 1 ? 's' : ''}</span>
                  </div>
                </div>
              </div>

              {/* Registration Period */}
              <div className="mb-6">
                <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                  Registration Period
                </label>
                <div className="grid grid-cols-3 gap-3">
                  {[1, 2, 3].map((years) => (
                    <button
                      key={years}
                      type="button"
                      onClick={() => setRegistrationYears(years as 1 | 2 | 3)}
                      className={`p-4 rounded-lg border-2 text-center transition-colors ${
                        registrationYears === years
                          ? 'border-blue-600 bg-blue-50 dark:bg-blue-900/20'
                          : 'border-gray-200 dark:border-gray-600 hover:border-blue-300'
                      }`}
                    >
                      <div className="font-bold text-gray-900 dark:text-white">{years} Year{years > 1 ? 's' : ''}</div>
                      <div className="text-sm text-gray-600 dark:text-gray-400">
                        ${calculatePriceForYears(domainProduct, years).toFixed(2)}
                      </div>
                    </button>
                  ))}
                </div>
              </div>

              {/* Add-ons */}
              <div className="mb-6 border-t border-gray-200 dark:border-gray-700 pt-6">
                <h3 className="text-lg font-semibold text-gray-900 dark:text-white mb-4">
                  Add-ons
                </h3>
                
                <div className="space-y-3">
                  <label className="flex items-center justify-between p-4 border border-gray-200 dark:border-gray-600 rounded-lg cursor-pointer hover:bg-gray-50 dark:hover:bg-gray-700">
                    <div className="flex items-center gap-3">
                      <input
                        type="checkbox"
                        checked={whoisPrivacy}
                        onChange={(e) => setWhoisPrivacy(e.target.checked)}
                        className="w-5 h-5 text-blue-600 border-gray-300 rounded focus:ring-blue-500"
                      />
                      <div className="flex items-center gap-2">
                        <FaShieldAlt className="text-blue-600" />
                        <div>
                          <span className="font-medium text-gray-900 dark:text-white">WHOIS Privacy Protection</span>
                          <p className="text-sm text-gray-500 dark:text-gray-400">Hide your personal info from public WHOIS records</p>
                        </div>
                      </div>
                    </div>
                    <span className="font-medium text-gray-900 dark:text-white">
                      +${(parseFloat(domainProduct.whois_privacy_price) * registrationYears).toFixed(2)}
                    </span>
                  </label>

                  <label className="flex items-center justify-between p-4 border border-gray-200 dark:border-gray-600 rounded-lg cursor-pointer hover:bg-gray-50 dark:hover:bg-gray-700">
                    <div className="flex items-center gap-3">
                      <input
                        type="checkbox"
                        checked={autoRenew}
                        onChange={(e) => setAutoRenew(e.target.checked)}
                        className="w-5 h-5 text-blue-600 border-gray-300 rounded focus:ring-blue-500"
                      />
                      <div className="flex items-center gap-2">
                        <FaLock className="text-blue-600" />
                        <div>
                          <span className="font-medium text-gray-900 dark:text-white">Auto-Renewal</span>
                          <p className="text-sm text-gray-500 dark:text-gray-400">Automatically renew before expiration</p>
                        </div>
                      </div>
                    </div>
                    <span className="text-green-600 font-medium">Free</span>
                  </label>
                </div>
              </div>

              {/* Nameserver Configuration - shown when not purchased with hosting */}
              {!isWithHosting && (
                <div className="mb-6 border-t border-gray-200 dark:border-gray-700 pt-6">
                  <h3 className="text-lg font-semibold text-gray-900 dark:text-white mb-4">
                    <FaServer className="inline mr-2" />
                    Nameservers
                  </h3>
                  <p className="text-sm text-gray-500 dark:text-gray-400 mb-4">
                    Nameservers control where your domain points. Use our defaults or enter your own.
                  </p>
                  
                  <div className="space-y-3">
                    <label className="flex items-center gap-3 p-3 border border-gray-200 dark:border-gray-600 rounded-lg cursor-pointer hover:bg-gray-50 dark:hover:bg-gray-700">
                      <input
                        type="radio"
                        name="nameserverOption"
                        value="default"
                        checked={nameserverOption === 'default'}
                        onChange={() => setNameserverOption('default')}
                        className="w-4 h-4 text-blue-600"
                      />
                      <div>
                        <span className="font-medium text-gray-900 dark:text-white">Use our default nameservers</span>
                        <p className="text-xs text-gray-500 dark:text-gray-400">
                          {DEFAULT_NAMESERVERS.join(', ')}
                        </p>
                      </div>
                    </label>
                    <label className="flex items-center gap-3 p-3 border border-gray-200 dark:border-gray-600 rounded-lg cursor-pointer hover:bg-gray-50 dark:hover:bg-gray-700">
                      <input
                        type="radio"
                        name="nameserverOption"
                        value="custom"
                        checked={nameserverOption === 'custom'}
                        onChange={() => setNameserverOption('custom')}
                        className="w-4 h-4 text-blue-600"
                      />
                      <div>
                        <span className="font-medium text-gray-900 dark:text-white">Use custom nameservers</span>
                        <p className="text-xs text-gray-500 dark:text-gray-400">Enter your own nameserver addresses</p>
                      </div>
                    </label>
                  </div>

                  {nameserverOption === 'custom' && (
                    <div className="mt-4 space-y-3">
                      {customNameservers.map((ns, index) => (
                        <div key={index}>
                          <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">
                            Nameserver {index + 1} {index < 2 && '*'}
                          </label>
                          <input
                            type="text"
                            value={ns}
                            onChange={(e) => {
                              const updated = [...customNameservers];
                              updated[index] = e.target.value.toLowerCase();
                              setCustomNameservers(updated);
                            }}
                            placeholder={`ns${index + 1}.example.com`}
                            className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-white"
                          />
                        </div>
                      ))}
                      {customNameservers.length < 4 && (
                        <button
                          type="button"
                          onClick={() => setCustomNameservers([...customNameservers, ''])}
                          className="text-sm text-blue-600 hover:text-blue-700"
                        >
                          + Add another nameserver
                        </button>
                      )}
                    </div>
                  )}
                </div>
              )}

              {/* Order Summary */}
              <div className="bg-gray-50 dark:bg-gray-700 rounded-lg p-4 mb-6">
                <h3 className="font-semibold text-gray-900 dark:text-white mb-3">Order Summary</h3>
                <div className="space-y-2 text-sm">
                  <div className="flex justify-between">
                    <span className="text-gray-600 dark:text-gray-400">
                      Domain Registration ({registrationYears} year{registrationYears > 1 ? 's' : ''})
                    </span>
                    <span className="font-medium text-gray-900 dark:text-white">${getPrice().toFixed(2)}</span>
                  </div>
                  {whoisPrivacy && (
                    <div className="flex justify-between">
                      <span className="text-gray-600 dark:text-gray-400">WHOIS Privacy</span>
                      <span className="font-medium text-gray-900 dark:text-white">${getWhoisPrivacyPrice().toFixed(2)}</span>
                    </div>
                  )}
                  <div className="border-t border-gray-200 dark:border-gray-600 pt-2 mt-2">
                    <div className="flex justify-between">
                      <span className="font-semibold text-gray-900 dark:text-white">Total</span>
                      <span className="font-bold text-xl text-blue-600">${getTotalPrice().toFixed(2)}</span>
                    </div>
                  </div>
                </div>
              </div>

              {/* Error Message */}
              {error && (
                <div className="bg-red-50 dark:bg-red-900/20 border border-red-200 dark:border-red-800 rounded-lg p-4 mb-6">
                  <p className="text-red-700 dark:text-red-300 text-sm">{error}</p>
                </div>
              )}

              {/* Add to Cart Button */}
              <button
                onClick={handleAddToCart}
                disabled={addingToCart || !availabilityStatus?.available}
                className="w-full py-4 bg-blue-600 text-white font-semibold rounded-lg hover:bg-blue-700 transition-colors disabled:opacity-50 disabled:cursor-not-allowed flex items-center justify-center gap-2"
              >
                {addingToCart ? (
                  <>
                    <svg className="animate-spin h-5 w-5" viewBox="0 0 24 24">
                      <circle className="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" strokeWidth="4" fill="none" />
                      <path className="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z" />
                    </svg>
                    Adding to Cart...
                  </>
                ) : (
                  <>
                    <FaShoppingCart />
                    Add to Cart - ${getTotalPrice().toFixed(2)}
                  </>
                )}
              </button>
            </>
          )}

          {/* Show message when no domain is entered */}
          {!domainName && (
            <div className="text-center py-8 text-gray-500 dark:text-gray-400">
              <FaGlobe className="mx-auto mb-4 text-4xl text-gray-300 dark:text-gray-600" />
              <p>Enter a domain name to check availability and pricing</p>
            </div>
          )}

          {/* Show message when domain is not available */}
          {domainName && !checking && availabilityStatus && !availabilityStatus.available && (
            <div className="text-center py-8">
              <p className="text-gray-600 dark:text-gray-400 mb-4">
                This domain is already registered. Would you like to transfer it instead?
              </p>
              <Link
                href={`/services/domains/transfer?domain=${encodeURIComponent(domainName)}`}
                className="inline-flex items-center gap-2 px-6 py-3 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors"
              >
                Transfer Domain
              </Link>
            </div>
          )}
        </div>

        {/* Help Section */}
        <div className="mt-8 text-center text-sm text-gray-600 dark:text-gray-400">
          <p>
            Need help choosing a domain? 
            <Link href="/support" className="text-blue-600 hover:underline ml-1">
              Contact our support team
            </Link>
          </p>
        </div>
      </div>
    </div>
  );
}

export default function DomainRegisterPage() {
  return (
    <Suspense fallback={
      <div className="min-h-screen flex items-center justify-center">
        <div className="text-xl">Loading registration form...</div>
      </div>
    }>
      <RegisterPageContent />
    </Suspense>
  );
}
