'use client';

import { useState, useEffect } from 'react';
import { useRouter } from 'next/navigation';
import { FaTimes } from 'react-icons/fa';

interface InvestmentPackage {
  id: number;
  name: string;
  minimum_amount: string;
  expected_return: string;
  duration_months: number;
}

interface InvestmentApplicationFormProps {
  package: InvestmentPackage;
  onClose: () => void;
}

export default function InvestmentApplicationForm({ package: pkg, onClose }: InvestmentApplicationFormProps) {
  const router = useRouter();
  const [amount, setAmount] = useState(pkg.minimum_amount);
  const [bankName, setBankName] = useState('');
  const [accountNumber, setAccountNumber] = useState('');
  const [accountHolder, setAccountHolder] = useState('');
  const [swiftCode, setSwiftCode] = useState('');
  const [branchCode, setBranchCode] = useState('');
  const [loading, setLoading] = useState(false);
  const [message, setMessage] = useState('');
  const [isAuthenticated, setIsAuthenticated] = useState(false);

  useEffect(() => {
    if (typeof window !== 'undefined') {
      const token = localStorage.getItem('access_token');
      setIsAuthenticated(!!token);
    }
  }, []);

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();

    if (!isAuthenticated) {
      setMessage('Please login to invest');
      router.push('/login?redirect=/invest');
      return;
    }

    if (parseFloat(amount) < parseFloat(pkg.minimum_amount)) {
      setMessage(`Minimum investment amount is $${parseFloat(pkg.minimum_amount).toLocaleString()}`);
      return;
    }

    setLoading(true);
    setMessage('');

    try {
      const API_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:8000/api';
      const token = localStorage.getItem('access_token');

      // First, create banking details
      const bankingResponse = await fetch(`${API_URL}/investments/banking/`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'Authorization': `Bearer ${token}`
        },
        body: JSON.stringify({
          bank_name: bankName,
          account_number: accountNumber,
          account_holder_name: accountHolder,
          swift_code: swiftCode || null,
          branch_code: branchCode || null,
          is_primary: true
        })
      });

      if (!bankingResponse.ok) {
        const error = await bankingResponse.json();
        throw new Error(error.detail || 'Failed to save banking details');
      }

      // Then create the investment
      const investmentResponse = await fetch(`${API_URL}/investments/investments/`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'Authorization': `Bearer ${token}`
        },
        body: JSON.stringify({
          package: pkg.id,
          amount: parseFloat(amount)
        })
      });

      if (investmentResponse.ok) {
        setMessage('Investment application submitted successfully! Our team will review and contact you soon.');
        setTimeout(() => {
          router.push('/dashboard/investments');
        }, 2000);
      } else {
        const error = await investmentResponse.json();
        throw new Error(error.detail || 'Failed to submit investment');
      }
    } catch (error: unknown) {
      setMessage(error instanceof Error ? error.message : 'An error occurred. Please try again later.');
    } finally {
      setLoading(false);
    }
  };

  return (
    <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50 p-4">
      <div className="bg-white dark:bg-gray-900 rounded-2xl p-8 max-w-2xl w-full max-h-[90vh] overflow-y-auto">
        <div className="flex justify-between items-center mb-6">
          <h2 className="text-3xl font-bold text-blue-900 dark:text-blue-300">
            Investment Application - {pkg.name}
          </h2>
          <button
            onClick={onClose}
            className="text-gray-500 hover:text-gray-700 dark:text-gray-400 dark:hover:text-gray-200"
          >
            <FaTimes className="text-2xl" />
          </button>
        </div>

        {message && (
          <div className={`p-4 rounded-lg mb-6 ${
            message.includes('success')
              ? 'bg-green-100 dark:bg-green-900 text-green-900 dark:text-green-100'
              : 'bg-red-100 dark:bg-red-900 text-red-900 dark:text-red-100'
          }`}>
            {message}
          </div>
        )}

        <form onSubmit={handleSubmit} className="space-y-6">
          {/* Investment Details */}
          <div className="border-b border-gray-200 dark:border-gray-700 pb-6">
            <h3 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-4">
              Investment Details
            </h3>
            <div className="grid grid-cols-2 gap-4 mb-4 text-sm">
              <div>
                <span className="text-gray-600 dark:text-gray-400">Expected Return:</span>
                <span className="ml-2 font-semibold text-darkgoldenrod dark:text-yellow-400">
                  {pkg.expected_return}%
                </span>
              </div>
              <div>
                <span className="text-gray-600 dark:text-gray-400">Duration:</span>
                <span className="ml-2 font-semibold text-blue-900 dark:text-blue-300">
                  {pkg.duration_months} months
                </span>
              </div>
            </div>
            <div>
              <label className="block text-gray-700 dark:text-gray-300 mb-2 font-medium">
                Investment Amount ($) *
              </label>
              <input
                type="number"
                value={amount}
                onChange={(e) => setAmount(e.target.value)}
                required
                min={pkg.minimum_amount}
                step="1000"
                className="w-full px-4 py-3 rounded-lg border border-gray-300 dark:border-gray-700 bg-white dark:bg-gray-800 text-gray-900 dark:text-gray-100"
              />
              <p className="text-sm text-gray-500 dark:text-gray-500 mt-1">
                Minimum: ${parseFloat(pkg.minimum_amount).toLocaleString()}
              </p>
            </div>
          </div>

          {/* Banking Details */}
          <div>
            <h3 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-4">
              Banking Details (for disbursements)
            </h3>
            
            <div className="space-y-4">
              <div>
                <label className="block text-gray-700 dark:text-gray-300 mb-2">
                  Bank Name *
                </label>
                <input
                  type="text"
                  value={bankName}
                  onChange={(e) => setBankName(e.target.value)}
                  required
                  className="w-full px-4 py-3 rounded-lg border border-gray-300 dark:border-gray-700 bg-white dark:bg-gray-800 text-gray-900 dark:text-gray-100"
                />
              </div>

              <div>
                <label className="block text-gray-700 dark:text-gray-300 mb-2">
                  Account Number *
                </label>
                <input
                  type="text"
                  value={accountNumber}
                  onChange={(e) => setAccountNumber(e.target.value)}
                  required
                  className="w-full px-4 py-3 rounded-lg border border-gray-300 dark:border-gray-700 bg-white dark:bg-gray-800 text-gray-900 dark:text-gray-100"
                />
              </div>

              <div>
                <label className="block text-gray-700 dark:text-gray-300 mb-2">
                  Account Holder Name *
                </label>
                <input
                  type="text"
                  value={accountHolder}
                  onChange={(e) => setAccountHolder(e.target.value)}
                  required
                  className="w-full px-4 py-3 rounded-lg border border-gray-300 dark:border-gray-700 bg-white dark:bg-gray-800 text-gray-900 dark:text-gray-100"
                />
              </div>

              <div className="grid grid-cols-2 gap-4">
                <div>
                  <label className="block text-gray-700 dark:text-gray-300 mb-2">
                    SWIFT Code
                  </label>
                  <input
                    type="text"
                    value={swiftCode}
                    onChange={(e) => setSwiftCode(e.target.value)}
                    className="w-full px-4 py-3 rounded-lg border border-gray-300 dark:border-gray-700 bg-white dark:bg-gray-800 text-gray-900 dark:text-gray-100"
                  />
                </div>

                <div>
                  <label className="block text-gray-700 dark:text-gray-300 mb-2">
                    Branch Code
                  </label>
                  <input
                    type="text"
                    value={branchCode}
                    onChange={(e) => setBranchCode(e.target.value)}
                    className="w-full px-4 py-3 rounded-lg border border-gray-300 dark:border-gray-700 bg-white dark:bg-gray-800 text-gray-900 dark:text-gray-100"
                  />
                </div>
              </div>
            </div>
          </div>

          <div className="flex gap-4">
            <button
              type="button"
              onClick={onClose}
              className="flex-1 px-6 py-4 bg-gray-300 hover:bg-gray-400 dark:bg-gray-700 dark:hover:bg-gray-600 text-gray-900 dark:text-gray-100 rounded-lg font-semibold transition-colors"
            >
              Cancel
            </button>
            <button
              type="submit"
              disabled={loading}
              className="flex-1 px-6 py-4 bg-blue-600 hover:bg-blue-700 dark:bg-blue-700 dark:hover:bg-blue-600 text-white rounded-lg font-semibold transition-colors disabled:opacity-50 disabled:cursor-not-allowed"
            >
              {loading ? 'Submitting...' : 'Submit Application'}
            </button>
          </div>

          {!isAuthenticated && (
            <p className="text-center text-gray-600 dark:text-gray-400">
              You need to <a href="/login" className="text-blue-600 dark:text-blue-400 hover:underline">login</a> or <a href="/signup" className="text-blue-600 dark:text-blue-400 hover:underline">create an account</a> to invest
            </p>
          )}
        </form>
      </div>
    </div>
  );
}
