'use client';

import React, { useState, useEffect } from 'react';
import { apiService } from '@/lib/api-service';

interface Wallet {
  id: number;
  balance: number;
  currency: string;
  low_balance_threshold: number;
}

interface Transaction {
  id: number;
  transaction_type: string;
  category: string;
  amount: number;
  description: string;
  created_at: string;
}

export default function WalletManagement() {
  const [wallet, setWallet] = useState<Wallet | null>(null);
  const [transactions, setTransactions] = useState<Transaction[]>([]);
  const [showTopUp, setShowTopUp] = useState(false);
  const [topUpAmount, setTopUpAmount] = useState('');
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    fetchWalletData();
  }, []);

  const fetchWalletData = async () => {
    try {
      setLoading(true);
      const [walletRes, transactionsRes] = await Promise.all([
        apiService.get('/wallet/wallets/'),
        apiService.get('/wallet/transactions/')
      ]);
      setWallet(walletRes.data[0]);
      setTransactions(transactionsRes.data);
    } catch (error) {
      console.error('Failed to fetch wallet data:', error);
    } finally {
      setLoading(false);
    }
  };

  const handleTopUp = async () => {
    if (!wallet || !topUpAmount) return;
    
    try {
      await apiService.post(`/wallet/wallets/${wallet.id}/topup/`, {
        amount: parseFloat(topUpAmount),
        payment_method: 'paynow'
      });
      setShowTopUp(false);
      setTopUpAmount('');
      fetchWalletData();
    } catch (error) {
      console.error('Top-up failed:', error);
    }
  };

  if (loading) {
    return <div className="animate-pulse">Loading wallet...</div>;
  }

  return (
    <div className="p-6">
      <h1 className="text-3xl font-bold mb-6">Wallet Management</h1>
      
      {/* Wallet Balance Card */}
      <div className="bg-gradient-to-r from-blue-500 to-purple-600 rounded-lg p-8 text-white mb-6">
        <p className="text-lg mb-2">Current Balance</p>
        <p className="text-4xl font-bold">{wallet?.currency} {wallet?.balance.toFixed(2)}</p>
        <button
          onClick={() => setShowTopUp(true)}
          className="mt-4 py-2 px-6 bg-white text-blue-600 rounded-lg font-semibold hover:bg-gray-100"
        >
          Top Up Wallet
        </button>
      </div>

      {/* Top-Up Modal */}
      {showTopUp && (
        <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50">
          <div className="bg-white dark:bg-gray-800 rounded-lg p-6 w-96">
            <h2 className="text-2xl font-bold mb-4">Top Up Wallet</h2>
            <input
              type="number"
              value={topUpAmount}
              onChange={(e) => setTopUpAmount(e.target.value)}
              placeholder="Enter amount"
              className="w-full p-3 border rounded mb-4"
            />
            <div className="flex gap-3">
              <button
                onClick={handleTopUp}
                className="flex-1 py-2 bg-blue-600 text-white rounded hover:bg-blue-700"
              >
                Confirm
              </button>
              <button
                onClick={() => setShowTopUp(false)}
                className="flex-1 py-2 bg-gray-300 rounded hover:bg-gray-400"
              >
                Cancel
              </button>
            </div>
          </div>
        </div>
      )}

      {/* Transaction History */}
      <div className="bg-white dark:bg-gray-800 rounded-lg shadow">
        <div className="p-6 border-b">
          <h2 className="text-xl font-semibold">Transaction History</h2>
        </div>
        <div className="p-6">
          <div className="space-y-4">
            {transactions.map((tx) => (
              <div key={tx.id} className="flex justify-between items-center py-3 border-b">
                <div>
                  <p className="font-medium">{tx.category.replace('_', ' ').toUpperCase()}</p>
                  <p className="text-sm text-gray-500">{tx.description}</p>
                  <p className="text-xs text-gray-400">{new Date(tx.created_at).toLocaleString()}</p>
                </div>
                <div className={`text-lg font-bold ${tx.transaction_type === 'credit' ? 'text-green-600' : 'text-red-600'}`}>
                  {tx.transaction_type === 'credit' ? '+' : '-'}${tx.amount.toFixed(2)}
                </div>
              </div>
            ))}
          </div>
        </div>
      </div>
    </div>
  );
}
