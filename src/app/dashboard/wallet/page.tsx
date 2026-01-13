'use client';

import { useState, useEffect } from 'react';
import { useRouter } from 'next/navigation';
import Link from 'next/link';
import { FaWallet, FaSync, FaArrowUp, FaArrowDown, FaPlus } from 'react-icons/fa';
import { useAuthStore } from '@/lib/stores/auth-store';
import { apiService } from '@/lib/api-service';

interface Wallet {
  id: number;
  balance: string;
  currency: string;
}

interface Transaction {
  id: number;
  type: string;
  amount: string;
  description: string;
  created_at: string;
  status: string;
}

export default function WalletPage() {
  const router = useRouter();
  const { isAuthenticated, isLoading: authLoading } = useAuthStore();
  const [wallet, setWallet] = useState<Wallet | null>(null);
  const [transactions, setTransactions] = useState<Transaction[]>([]);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    if (!authLoading && !isAuthenticated) {
      router.push('/login');
    }
  }, [isAuthenticated, authLoading, router]);

  useEffect(() => {
    if (isAuthenticated) {
      loadWalletData();
    }
  }, [isAuthenticated]);

  const loadWalletData = async () => {
    setLoading(true);
    try {
      const [walletResponse, transactionsResponse] = await Promise.all([
        apiService.getWallets(),
        apiService.getWalletTransactions(),
      ]);

      if (walletResponse.data) {
        const wallets = walletResponse.data.results || walletResponse.data;
        if (wallets.length > 0) {
          setWallet(wallets[0]);
        }
      }

      if (transactionsResponse.data) {
        setTransactions(transactionsResponse.data.results || transactionsResponse.data);
      }
    } catch (error) {
      console.error('Failed to load wallet data:', error);
    } finally {
      setLoading(false);
    }
  };

  if (authLoading || !isAuthenticated) {
    return (
      <div className="min-h-screen flex items-center justify-center bg-gray-50 dark:bg-gray-900">
        <div className="text-center">
          <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-blue-600 mx-auto"></div>
          <p className="mt-4 text-gray-600 dark:text-gray-400">Loading...</p>
        </div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-gray-50 dark:bg-gray-900 pt-24 pb-12 px-4">
      <div className="max-w-6xl mx-auto">
        <div className="mb-8">
          <div className="flex items-center justify-between">
            <div>
              <h1 className="text-3xl font-bold text-gray-900 dark:text-white mb-2 flex items-center gap-3">
                <FaWallet className="text-green-600" />
                My Wallet
              </h1>
              <p className="text-gray-600 dark:text-gray-400">Manage your balance and transactions</p>
            </div>
            <button
              onClick={loadWalletData}
              className="flex items-center gap-2 px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors"
            >
              <FaSync className={loading ? 'animate-spin' : ''} />
              Refresh
            </button>
          </div>
        </div>

        {/* Balance Card */}
        <div className="bg-gradient-to-br from-blue-600 to-blue-800 rounded-lg shadow-xl p-8 mb-8 text-white">
          <p className="text-sm opacity-90 mb-2">Available Balance</p>
          <p className="text-4xl font-bold mb-6">
            {wallet ? `$${parseFloat(wallet.balance).toFixed(2)}` : '$0.00'}
          </p>
          <div className="flex gap-4">
            <button className="flex-1 flex items-center justify-center gap-2 px-4 py-3 bg-white text-blue-600 rounded-lg hover:bg-gray-100 transition-colors font-medium">
              <FaPlus />
              Add Funds
            </button>
          </div>
        </div>

        {/* Transactions */}
        <div className="bg-white dark:bg-gray-800 rounded-lg shadow-lg p-6">
          <h2 className="text-xl font-bold text-gray-900 dark:text-white mb-4">Recent Transactions</h2>
          <div className="space-y-4">
            {loading ? (
              <div className="text-center py-8">
                <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-blue-600 mx-auto"></div>
              </div>
            ) : transactions.length === 0 ? (
              <p className="text-center text-gray-500 dark:text-gray-400 py-8">No transactions yet</p>
            ) : (
              transactions.map((transaction) => (
                <div key={transaction.id} className="flex items-center justify-between p-4 bg-gray-50 dark:bg-gray-700 rounded-lg">
                  <div className="flex items-center gap-4">
                    <div className={`p-3 rounded-full ${
                      transaction.type === 'credit' ? 'bg-green-100 dark:bg-green-900' : 'bg-red-100 dark:bg-red-900'
                    }`}>
                      {transaction.type === 'credit' ? (
                        <FaArrowDown className="text-green-600 dark:text-green-400" />
                      ) : (
                        <FaArrowUp className="text-red-600 dark:text-red-400" />
                      )}
                    </div>
                    <div>
                      <p className="font-medium text-gray-900 dark:text-white">{transaction.description}</p>
                      <p className="text-sm text-gray-500 dark:text-gray-400">
                        {new Date(transaction.created_at).toLocaleString()}
                      </p>
                    </div>
                  </div>
                  <p className={`text-lg font-semibold ${
                    transaction.type === 'credit' ? 'text-green-600 dark:text-green-400' : 'text-red-600 dark:text-red-400'
                  }`}>
                    {transaction.type === 'credit' ? '+' : '-'}${parseFloat(transaction.amount).toFixed(2)}
                  </p>
                </div>
              ))
            )}
          </div>
        </div>

        <div className="mt-8">
          <Link href="/dashboard" className="text-blue-600 hover:text-blue-700 dark:text-blue-400 dark:hover:text-blue-300">
            ← Back to Dashboard
          </Link>
        </div>
      </div>
    </div>
  );
}
