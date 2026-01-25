'use client';

import React, { useState, useEffect } from 'react';
import { FaUsers, FaGift, FaLink, FaCopy, FaCheckCircle, FaClock, FaDollarSign } from 'react-icons/fa';

interface ReferralStats {
  total_referrals: number;
  pending_referrals: number;
  signed_up_referrals: number;
  converted_referrals: number;
  total_earnings: string;
  pending_earnings: string;
  referral_code: string;
  referral_link: string;
}

interface Referral {
  id: number;
  referred_email: string;
  status: string;
  status_display: string;
  created_at: string;
  reward_amount: string | null;
}

export default function ReferralDashboard() {
  const [stats, setStats] = useState<ReferralStats | null>(null);
  const [referrals, setReferrals] = useState<Referral[]>([]);
  const [loading, setLoading] = useState(true);
  const [copied, setCopied] = useState(false);
  const [newReferralEmail, setNewReferralEmail] = useState('');
  const [inviting, setInviting] = useState(false);
  const [message, setMessage] = useState('');

  useEffect(() => {
    fetchData();
  }, []);

  const fetchData = async () => {
    try {
      const API_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:8000/api';
      const token = localStorage.getItem('access_token');
      
      const [statsRes, referralsRes] = await Promise.all([
        fetch(`${API_URL}/referrals/profiles/stats/`, {
          headers: { 'Authorization': `Bearer ${token}` }
        }),
        fetch(`${API_URL}/referrals/referrals/my_referrals/`, {
          headers: { 'Authorization': `Bearer ${token}` }
        })
      ]);

      if (statsRes.ok) {
        const statsData = await statsRes.json();
        setStats(statsData);
      }

      if (referralsRes.ok) {
        const referralsData = await referralsRes.json();
        setReferrals(referralsData);
      }
    } catch (error) {
      console.error('Failed to fetch referral data:', error);
    } finally {
      setLoading(false);
    }
  };

  const copyToClipboard = async () => {
    if (stats?.referral_link) {
      const fullLink = `${window.location.origin}${stats.referral_link}`;
      await navigator.clipboard.writeText(fullLink);
      setCopied(true);
      setTimeout(() => setCopied(false), 2000);
    }
  };

  const handleInvite = async (e: React.FormEvent) => {
    e.preventDefault();
    setInviting(true);
    setMessage('');

    try {
      const API_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:8000/api';
      const token = localStorage.getItem('access_token');

      const response = await fetch(`${API_URL}/referrals/referrals/`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'Authorization': `Bearer ${token}`
        },
        body: JSON.stringify({ referred_email: newReferralEmail })
      });

      if (response.ok) {
        setMessage('Invitation sent successfully!');
        setNewReferralEmail('');
        fetchData();
      } else {
        const error = await response.json();
        setMessage(error.referred_email?.[0] || 'Failed to send invitation');
      }
    } catch {
      setMessage('An error occurred. Please try again.');
    } finally {
      setInviting(false);
    }
  };

  const getStatusColor = (status: string) => {
    switch (status) {
      case 'rewarded':
        return 'bg-green-100 text-green-800 dark:bg-green-900 dark:text-green-200';
      case 'converted':
        return 'bg-blue-100 text-blue-800 dark:bg-blue-900 dark:text-blue-200';
      case 'signed_up':
        return 'bg-purple-100 text-purple-800 dark:bg-purple-900 dark:text-purple-200';
      case 'pending':
        return 'bg-yellow-100 text-yellow-800 dark:bg-yellow-900 dark:text-yellow-200';
      default:
        return 'bg-gray-100 text-gray-800 dark:bg-gray-900 dark:text-gray-200';
    }
  };

  if (loading) {
    return (
      <div className="p-6">
        <div className="animate-pulse">
          <div className="h-8 bg-gray-200 dark:bg-gray-700 rounded w-1/4 mb-6"></div>
          <div className="grid grid-cols-1 md:grid-cols-4 gap-6 mb-8">
            {[1, 2, 3, 4].map(i => (
              <div key={i} className="h-32 bg-gray-200 dark:bg-gray-700 rounded-lg"></div>
            ))}
          </div>
        </div>
      </div>
    );
  }

  return (
    <div className="p-6">
      <h1 className="text-3xl font-bold text-blue-900 dark:text-blue-300 mb-6">
        Referral Program
      </h1>

      {/* Stats Cards */}
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-8">
        <div className="bg-blue-50 dark:bg-blue-900/30 rounded-xl p-6">
          <div className="flex items-center justify-between">
            <div>
              <p className="text-sm text-gray-600 dark:text-gray-400">Total Referrals</p>
              <p className="text-3xl font-bold text-blue-900 dark:text-blue-300 mt-1">
                {stats?.total_referrals || 0}
              </p>
            </div>
            <div className="p-3 bg-blue-100 dark:bg-blue-800 rounded-full">
              <FaUsers className="w-6 h-6 text-blue-600 dark:text-blue-300" />
            </div>
          </div>
        </div>

        <div className="bg-yellow-50 dark:bg-yellow-900/30 rounded-xl p-6">
          <div className="flex items-center justify-between">
            <div>
              <p className="text-sm text-gray-600 dark:text-gray-400">Pending</p>
              <p className="text-3xl font-bold text-yellow-700 dark:text-yellow-300 mt-1">
                {stats?.pending_referrals || 0}
              </p>
            </div>
            <div className="p-3 bg-yellow-100 dark:bg-yellow-800 rounded-full">
              <FaClock className="w-6 h-6 text-yellow-600 dark:text-yellow-300" />
            </div>
          </div>
        </div>

        <div className="bg-green-50 dark:bg-green-900/30 rounded-xl p-6">
          <div className="flex items-center justify-between">
            <div>
              <p className="text-sm text-gray-600 dark:text-gray-400">Converted</p>
              <p className="text-3xl font-bold text-green-700 dark:text-green-300 mt-1">
                {stats?.converted_referrals || 0}
              </p>
            </div>
            <div className="p-3 bg-green-100 dark:bg-green-800 rounded-full">
              <FaCheckCircle className="w-6 h-6 text-green-600 dark:text-green-300" />
            </div>
          </div>
        </div>

        <div className="bg-purple-50 dark:bg-purple-900/30 rounded-xl p-6">
          <div className="flex items-center justify-between">
            <div>
              <p className="text-sm text-gray-600 dark:text-gray-400">Total Earnings</p>
              <p className="text-3xl font-bold text-purple-700 dark:text-purple-300 mt-1">
                ${stats?.total_earnings || '0.00'}
              </p>
            </div>
            <div className="p-3 bg-purple-100 dark:bg-purple-800 rounded-full">
              <FaDollarSign className="w-6 h-6 text-purple-600 dark:text-purple-300" />
            </div>
          </div>
        </div>
      </div>

      {/* Referral Link Section */}
      <div className="bg-white dark:bg-gray-800 rounded-xl shadow-sm p-6 mb-8">
        <h2 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-4 flex items-center gap-2">
          <FaLink className="text-darkgoldenrod dark:text-yellow-400" />
          Your Referral Link
        </h2>
        
        <div className="flex flex-col sm:flex-row gap-4">
          <div className="flex-1 bg-gray-100 dark:bg-gray-700 rounded-lg px-4 py-3 font-mono text-sm overflow-x-auto">
            {stats?.referral_link ? `${typeof window !== 'undefined' ? window.location.origin : ''}${stats.referral_link}` : 'Loading...'}
          </div>
          <button
            onClick={copyToClipboard}
            className="flex items-center justify-center gap-2 px-6 py-3 bg-darkgoldenrod hover:bg-yellow-600 dark:bg-yellow-500 dark:hover:bg-yellow-400 text-white dark:text-gray-900 rounded-lg font-semibold transition-colors"
          >
            {copied ? (
              <>
                <FaCheckCircle />
                Copied!
              </>
            ) : (
              <>
                <FaCopy />
                Copy Link
              </>
            )}
          </button>
        </div>
        
        <p className="mt-3 text-sm text-gray-600 dark:text-gray-400">
          Your referral code: <span className="font-mono font-bold">{stats?.referral_code}</span>
        </p>
      </div>

      {/* Invite Form */}
      <div className="bg-white dark:bg-gray-800 rounded-xl shadow-sm p-6 mb-8">
        <h2 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-4 flex items-center gap-2">
          <FaGift className="text-darkgoldenrod dark:text-yellow-400" />
          Invite Friends
        </h2>

        {message && (
          <div className={`mb-4 p-3 rounded-lg ${
            message.includes('success') 
              ? 'bg-green-100 dark:bg-green-900 text-green-800 dark:text-green-200'
              : 'bg-red-100 dark:bg-red-900 text-red-800 dark:text-red-200'
          }`}>
            {message}
          </div>
        )}

        <form onSubmit={handleInvite} className="flex flex-col sm:flex-row gap-4">
          <input
            type="email"
            value={newReferralEmail}
            onChange={(e) => setNewReferralEmail(e.target.value)}
            placeholder="Enter friend's email address"
            required
            className="flex-1 px-4 py-3 rounded-lg border border-gray-300 dark:border-gray-600 bg-white dark:bg-gray-700 text-gray-900 dark:text-gray-100"
          />
          <button
            type="submit"
            disabled={inviting}
            className="px-6 py-3 bg-blue-600 hover:bg-blue-700 dark:bg-blue-700 dark:hover:bg-blue-600 text-white rounded-lg font-semibold transition-colors disabled:opacity-50"
          >
            {inviting ? 'Sending...' : 'Send Invite'}
          </button>
        </form>
      </div>

      {/* Referrals List */}
      <div className="bg-white dark:bg-gray-800 rounded-xl shadow-sm p-6">
        <h2 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-4">
          Your Referrals
        </h2>

        {referrals.length === 0 ? (
          <div className="text-center py-12 text-gray-500 dark:text-gray-400">
            <FaUsers className="w-16 h-16 mx-auto mb-4 opacity-50" />
            <p>No referrals yet. Share your link to start earning!</p>
          </div>
        ) : (
          <div className="overflow-x-auto">
            <table className="w-full">
              <thead>
                <tr className="text-left border-b border-gray-200 dark:border-gray-700">
                  <th className="pb-3 text-gray-600 dark:text-gray-400">Email</th>
                  <th className="pb-3 text-gray-600 dark:text-gray-400">Status</th>
                  <th className="pb-3 text-gray-600 dark:text-gray-400">Date</th>
                  <th className="pb-3 text-gray-600 dark:text-gray-400">Reward</th>
                </tr>
              </thead>
              <tbody>
                {referrals.map((referral) => (
                  <tr key={referral.id} className="border-b border-gray-100 dark:border-gray-700/50">
                    <td className="py-4">{referral.referred_email}</td>
                    <td className="py-4">
                      <span className={`px-3 py-1 rounded-full text-sm font-medium ${getStatusColor(referral.status)}`}>
                        {referral.status_display}
                      </span>
                    </td>
                    <td className="py-4 text-gray-600 dark:text-gray-400">
                      {new Date(referral.created_at).toLocaleDateString()}
                    </td>
                    <td className="py-4">
                      {referral.reward_amount ? `$${referral.reward_amount}` : '-'}
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        )}
      </div>
    </div>
  );
}
