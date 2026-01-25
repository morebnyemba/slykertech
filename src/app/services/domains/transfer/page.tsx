'use client';

/**
 * Domain Transfer Request Page
 * Allows users to request transferring a domain they own to our service
 */

import { useState, useEffect, Suspense } from 'react';
import { useSearchParams } from 'next/navigation';
import Link from 'next/link';
import { FaGlobe, FaExchangeAlt, FaCheckCircle, FaInfoCircle, FaArrowLeft } from 'react-icons/fa';
import { useAuthStore } from '@/lib/stores/auth-store';

interface TransferFormData {
  domain_name: string;
  contact_email: string;
  contact_name: string;
  contact_phone: string;
  epp_code: string;
  current_registrar: string;
  admin_email: string;
  owns_domain: boolean;
  update_nameservers: boolean;
  whois_privacy: boolean;
  auto_renew: boolean;
}

function TransferPageContent() {
  const searchParams = useSearchParams();
  const { token, user } = useAuthStore();
  
  const [formData, setFormData] = useState<TransferFormData>({
    domain_name: '',
    contact_email: '',
    contact_name: '',
    contact_phone: '',
    epp_code: '',
    current_registrar: '',
    admin_email: '',
    owns_domain: false,
    update_nameservers: true,
    whois_privacy: false,
    auto_renew: true,
  });
  
  const [submitting, setSubmitting] = useState(false);
  const [submitted, setSubmitted] = useState(false);
  const [error, setError] = useState<string | null>(null);

  // Pre-fill domain from URL query parameter
  useEffect(() => {
    const domainParam = searchParams.get('domain');
    if (domainParam) {
      setFormData(prev => ({ ...prev, domain_name: domainParam }));
    }
  }, [searchParams]);

  // Pre-fill user information if logged in
  useEffect(() => {
    if (user) {
      setFormData(prev => ({
        ...prev,
        contact_email: user.email || prev.contact_email,
        contact_name: user.full_name || `${user.first_name || ''} ${user.last_name || ''}`.trim() || prev.contact_name,
      }));
    }
  }, [user]);

  const handleInputChange = (e: React.ChangeEvent<HTMLInputElement | HTMLSelectElement | HTMLTextAreaElement>) => {
    const { name, value, type } = e.target;
    const checked = (e.target as HTMLInputElement).checked;
    
    setFormData(prev => ({
      ...prev,
      [name]: type === 'checkbox' ? checked : value
    }));
  };

  const validateForm = (): string | null => {
    if (!formData.domain_name.trim() || !formData.domain_name.includes('.')) {
      return 'Please enter a valid domain name (e.g., example.com)';
    }
    if (!formData.contact_email.trim() || !formData.contact_email.includes('@')) {
      return 'Please enter a valid email address';
    }
    if (!formData.contact_name.trim()) {
      return 'Please enter your name';
    }
    if (!formData.owns_domain) {
      return 'You must confirm that you own this domain';
    }
    return null;
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setError(null);

    const validationError = validateForm();
    if (validationError) {
      setError(validationError);
      return;
    }

    setSubmitting(true);

    try {
      const backendUrl = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:8000';
      const response = await fetch(`${backendUrl}/api/services/domain-transfer-requests/`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          ...(token && { 'Authorization': `Bearer ${token}` }),
        },
        body: JSON.stringify(formData),
      });

      if (!response.ok) {
        const errorData = await response.json();
        throw new Error(errorData.detail || errorData.error || 'Failed to submit transfer request');
      }

      setSubmitted(true);
    } catch (err) {
      setError(err instanceof Error ? err.message : 'An error occurred');
    } finally {
      setSubmitting(false);
    }
  };

  if (submitted) {
    return (
      <div className="min-h-screen py-12 bg-gray-50 dark:bg-gray-900">
        <div className="max-w-2xl mx-auto px-4">
          <div className="bg-white dark:bg-gray-800 rounded-xl shadow-lg p-8 text-center">
            <div className="w-16 h-16 bg-green-100 dark:bg-green-900 rounded-full flex items-center justify-center mx-auto mb-6">
              <FaCheckCircle className="text-green-600 text-3xl" />
            </div>
            <h1 className="text-2xl font-bold text-gray-900 dark:text-white mb-4">
              Transfer Request Submitted!
            </h1>
            <p className="text-gray-600 dark:text-gray-300 mb-6">
              Thank you for your domain transfer request. Our team will review your request and 
              contact you at <strong>{formData.contact_email}</strong> with next steps.
            </p>
            <div className="bg-blue-50 dark:bg-blue-900/20 border border-blue-200 dark:border-blue-800 rounded-lg p-4 mb-6">
              <h3 className="font-semibold text-blue-800 dark:text-blue-300 mb-2">What&apos;s Next?</h3>
              <ul className="text-sm text-blue-700 dark:text-blue-300 text-left list-disc list-inside space-y-1">
                <li>We&apos;ll verify your domain ownership</li>
                <li>You&apos;ll receive instructions to unlock your domain at your current registrar</li>
                <li>If you haven&apos;t provided the EPP code, we&apos;ll request it from you</li>
                <li>Once confirmed, the transfer typically takes 5-7 days</li>
              </ul>
            </div>
            <div className="flex gap-4 justify-center">
              <Link
                href="/services/domains"
                className="px-6 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors"
              >
                Browse Domains
              </Link>
              <Link
                href="/"
                className="px-6 py-2 border border-gray-300 dark:border-gray-600 text-gray-700 dark:text-gray-300 rounded-lg hover:bg-gray-100 dark:hover:bg-gray-700 transition-colors"
              >
                Go Home
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
            <FaExchangeAlt className="text-blue-600 text-2xl" />
          </div>
          <h1 className="text-3xl font-bold text-gray-900 dark:text-white mb-2">
            Transfer Your Domain
          </h1>
          <p className="text-gray-600 dark:text-gray-300 max-w-xl mx-auto">
            Already own a domain? Transfer it to us for better management, competitive renewal rates, and premium features.
          </p>
        </div>

        {/* Info Box */}
        <div className="bg-blue-50 dark:bg-blue-900/20 border border-blue-200 dark:border-blue-800 rounded-lg p-4 mb-8">
          <div className="flex items-start gap-3">
            <FaInfoCircle className="text-blue-600 flex-shrink-0 mt-1" />
            <div className="text-sm text-blue-800 dark:text-blue-300">
              <p className="font-semibold mb-1">Before you transfer:</p>
              <ul className="list-disc list-inside space-y-1">
                <li>Unlock your domain at your current registrar</li>
                <li>Disable WHOIS privacy (temporarily)</li>
                <li>Obtain your EPP/Authorization code from your current registrar</li>
                <li>Ensure the domain won&apos;t expire within 15 days</li>
              </ul>
            </div>
          </div>
        </div>

        {/* Transfer Form */}
        <div className="bg-white dark:bg-gray-800 rounded-xl shadow-lg p-8">
          <form onSubmit={handleSubmit} className="space-y-6">
            {/* Domain Name */}
            <div>
              <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                Domain Name *
              </label>
              <div className="relative">
                <FaGlobe className="absolute left-3 top-1/2 transform -translate-y-1/2 text-gray-400" />
                <input
                  type="text"
                  name="domain_name"
                  value={formData.domain_name}
                  onChange={handleInputChange}
                  placeholder="example.com"
                  className="w-full pl-10 pr-4 py-3 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-white focus:ring-2 focus:ring-blue-500"
                  required
                />
              </div>
            </div>

            {/* Contact Information */}
            <div className="grid md:grid-cols-2 gap-4">
              <div>
                <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                  Your Name *
                </label>
                <input
                  type="text"
                  name="contact_name"
                  value={formData.contact_name}
                  onChange={handleInputChange}
                  placeholder="John Doe"
                  className="w-full px-4 py-3 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-white focus:ring-2 focus:ring-blue-500"
                  required
                />
              </div>
              <div>
                <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                  Email Address *
                </label>
                <input
                  type="email"
                  name="contact_email"
                  value={formData.contact_email}
                  onChange={handleInputChange}
                  placeholder="john@example.com"
                  className="w-full px-4 py-3 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-white focus:ring-2 focus:ring-blue-500"
                  required
                />
              </div>
            </div>

            <div>
              <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                Phone Number (Optional)
              </label>
              <input
                type="tel"
                name="contact_phone"
                value={formData.contact_phone}
                onChange={handleInputChange}
                placeholder="+1 234 567 8900"
                className="w-full px-4 py-3 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-white focus:ring-2 focus:ring-blue-500"
              />
            </div>

            {/* Transfer Details */}
            <div className="border-t border-gray-200 dark:border-gray-700 pt-6">
              <h3 className="text-lg font-semibold text-gray-900 dark:text-white mb-4">
                Transfer Details
              </h3>
              
              <div className="grid md:grid-cols-2 gap-4 mb-4">
                <div>
                  <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                    EPP/Authorization Code
                  </label>
                  <input
                    type="text"
                    name="epp_code"
                    value={formData.epp_code}
                    onChange={handleInputChange}
                    placeholder="Enter EPP code (if available)"
                    className="w-full px-4 py-3 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-white focus:ring-2 focus:ring-blue-500"
                  />
                  <p className="text-xs text-gray-500 dark:text-gray-400 mt-1">
                    Don&apos;t have it? We&apos;ll guide you on how to get it.
                  </p>
                </div>
                <div>
                  <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                    Current Registrar
                  </label>
                  <input
                    type="text"
                    name="current_registrar"
                    value={formData.current_registrar}
                    onChange={handleInputChange}
                    placeholder="e.g., GoDaddy, Namecheap"
                    className="w-full px-4 py-3 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-white focus:ring-2 focus:ring-blue-500"
                  />
                </div>
              </div>

              <div>
                <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                  Admin Email (from WHOIS)
                </label>
                <input
                  type="email"
                  name="admin_email"
                  value={formData.admin_email}
                  onChange={handleInputChange}
                  placeholder="admin@example.com"
                  className="w-full px-4 py-3 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-white focus:ring-2 focus:ring-blue-500"
                />
                <p className="text-xs text-gray-500 dark:text-gray-400 mt-1">
                  This helps us verify domain ownership. Check your domain&apos;s WHOIS record.
                </p>
              </div>
            </div>

            {/* Options */}
            <div className="border-t border-gray-200 dark:border-gray-700 pt-6">
              <h3 className="text-lg font-semibold text-gray-900 dark:text-white mb-4">
                Transfer Options
              </h3>
              
              <div className="space-y-3">
                <label className="flex items-center gap-3 cursor-pointer">
                  <input
                    type="checkbox"
                    name="update_nameservers"
                    checked={formData.update_nameservers}
                    onChange={handleInputChange}
                    className="w-5 h-5 text-blue-600 border-gray-300 rounded focus:ring-blue-500"
                  />
                  <span className="text-gray-700 dark:text-gray-300">
                    Update nameservers to our servers after transfer
                  </span>
                </label>

                <label className="flex items-center gap-3 cursor-pointer">
                  <input
                    type="checkbox"
                    name="whois_privacy"
                    checked={formData.whois_privacy}
                    onChange={handleInputChange}
                    className="w-5 h-5 text-blue-600 border-gray-300 rounded focus:ring-blue-500"
                  />
                  <span className="text-gray-700 dark:text-gray-300">
                    Enable WHOIS privacy protection after transfer
                  </span>
                </label>

                <label className="flex items-center gap-3 cursor-pointer">
                  <input
                    type="checkbox"
                    name="auto_renew"
                    checked={formData.auto_renew}
                    onChange={handleInputChange}
                    className="w-5 h-5 text-blue-600 border-gray-300 rounded focus:ring-blue-500"
                  />
                  <span className="text-gray-700 dark:text-gray-300">
                    Enable auto-renewal for this domain
                  </span>
                </label>
              </div>
            </div>

            {/* Ownership Confirmation */}
            <div className="bg-yellow-50 dark:bg-yellow-900/20 border border-yellow-200 dark:border-yellow-800 rounded-lg p-4">
              <label className="flex items-start gap-3 cursor-pointer">
                <input
                  type="checkbox"
                  name="owns_domain"
                  checked={formData.owns_domain}
                  onChange={handleInputChange}
                  className="w-5 h-5 text-blue-600 border-gray-300 rounded focus:ring-blue-500 mt-0.5"
                  required
                />
                <span className="text-sm text-yellow-800 dark:text-yellow-200">
                  <strong>I confirm that I am the rightful owner</strong> of this domain and have 
                  the authority to transfer it. I understand that providing false information may 
                  result in the cancellation of this request.
                </span>
              </label>
            </div>

            {/* Error Message */}
            {error && (
              <div className="bg-red-50 dark:bg-red-900/20 border border-red-200 dark:border-red-800 rounded-lg p-4">
                <p className="text-red-700 dark:text-red-300 text-sm">{error}</p>
              </div>
            )}

            {/* Submit Button */}
            <button
              type="submit"
              disabled={submitting}
              className="w-full py-4 bg-blue-600 text-white font-semibold rounded-lg hover:bg-blue-700 transition-colors disabled:opacity-50 disabled:cursor-not-allowed flex items-center justify-center gap-2"
            >
              {submitting ? (
                <>
                  <svg className="animate-spin h-5 w-5" viewBox="0 0 24 24">
                    <circle className="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" strokeWidth="4" fill="none" />
                    <path className="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z" />
                  </svg>
                  Submitting...
                </>
              ) : (
                <>
                  <FaExchangeAlt />
                  Submit Transfer Request
                </>
              )}
            </button>
          </form>
        </div>

        {/* Help Section */}
        <div className="mt-8 text-center text-sm text-gray-600 dark:text-gray-400">
          <p>
            Need help with your transfer? 
            <Link href="/support" className="text-blue-600 hover:underline ml-1">
              Contact our support team
            </Link>
          </p>
        </div>
      </div>
    </div>
  );
}

export default function DomainTransferPage() {
  return (
    <Suspense fallback={
      <div className="min-h-screen flex items-center justify-center">
        <div className="text-xl">Loading transfer form...</div>
      </div>
    }>
      <TransferPageContent />
    </Suspense>
  );
}
