'use client';

import React, { useState, useEffect } from 'react';
import { useRouter } from 'next/navigation';
import Link from 'next/link';
import { 
  FaUsers, FaDollarSign, FaChartLine, FaHandshake, 
  FaCog, FaSignOutAlt, FaHome, FaUserPlus, FaFileAlt 
} from 'react-icons/fa';
import { MdDashboard, MdTrendingUp } from 'react-icons/md';

interface PartnerStats {
  partner_type: string;
  tier: string;
  is_active: boolean;
  company_name: string;
  total_clients?: number;
  max_clients?: number;
  discount_percentage?: string;
  commission_percentage?: string;
  total_earnings?: string;
  pending_earnings?: string;
  specialization?: string;
  total_referrals?: number;
  active_referrals?: number;
  referral_bonus_percentage?: string;
  total_bonus_earned?: string;
  pending_bonus?: string;
}

export default function PartnerPortalPage() {
  const router = useRouter();
  const [stats, setStats] = useState<PartnerStats | null>(null);
  const [loading, setLoading] = useState(true);
  const [hasPartnerProfile, setHasPartnerProfile] = useState(false);
  const [isAuthenticated, setIsAuthenticated] = useState(false);

  useEffect(() => {
    checkAuthAndFetch();
  // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  const checkAuthAndFetch = async () => {
    const token = localStorage.getItem('access_token');
    if (!token) {
      router.push('/login?redirect=/portal/partner');
      return;
    }
    setIsAuthenticated(true);
    await fetchStats();
  };

  const fetchStats = async () => {
    try {
      const API_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:8000/api';
      const token = localStorage.getItem('access_token');

      const response = await fetch(`${API_URL}/reseller/partners/dashboard_stats/`, {
        headers: { 'Authorization': `Bearer ${token}` }
      });

      if (response.ok) {
        const data = await response.json();
        setStats(data);
        setHasPartnerProfile(true);
      } else if (response.status === 404) {
        setHasPartnerProfile(false);
      }
    } catch (error) {
      console.error('Failed to fetch partner stats:', error);
    } finally {
      setLoading(false);
    }
  };

  const handleLogout = () => {
    localStorage.removeItem('access_token');
    localStorage.removeItem('refresh_token');
    router.push('/login');
  };

  if (!isAuthenticated || loading) {
    return (
      <div className="min-h-screen flex items-center justify-center bg-gray-50 dark:bg-gray-900">
        <div className="animate-pulse text-gray-600 dark:text-gray-400">Loading partner portal...</div>
      </div>
    );
  }

  // Not a partner yet - show application form
  if (!hasPartnerProfile) {
    return <PartnerApplicationForm onSuccess={fetchStats} />;
  }

  // Partner profile exists but not active
  if (!stats?.is_active) {
    return (
      <div className="min-h-screen flex items-center justify-center bg-gray-50 dark:bg-gray-900 p-4">
        <div className="max-w-md text-center bg-white dark:bg-gray-800 rounded-2xl p-8 shadow-lg">
          <div className="text-6xl mb-6">⏳</div>
          <h1 className="text-2xl font-bold text-blue-900 dark:text-blue-300 mb-4">
            Application Under Review
          </h1>
          <p className="text-gray-600 dark:text-gray-400 mb-6">
            Thank you for applying to become a partner! Our team is reviewing your application and will get back to you soon.
          </p>
          <Link
            href="/"
            className="inline-flex items-center gap-2 text-blue-600 dark:text-blue-400 hover:underline"
          >
            <FaHome /> Back to Home
          </Link>
        </div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-gray-50 dark:bg-gray-900">
      {/* Sidebar */}
      <aside className="fixed left-0 top-0 h-full w-64 bg-white dark:bg-gray-800 shadow-lg z-40">
        <div className="p-6 border-b border-gray-200 dark:border-gray-700">
          <h2 className="text-xl font-bold text-blue-900 dark:text-blue-300">Partner Portal</h2>
          <p className="text-sm text-gray-600 dark:text-gray-400 mt-1">{stats?.company_name}</p>
          <span className={`inline-block mt-2 px-3 py-1 rounded-full text-xs font-semibold ${
            stats?.tier === 'platinum' ? 'bg-blue-100 text-blue-800 dark:bg-blue-900 dark:text-blue-200' :
            stats?.tier === 'gold' ? 'bg-yellow-100 text-yellow-800 dark:bg-yellow-900 dark:text-yellow-200' :
            stats?.tier === 'silver' ? 'bg-gray-100 text-gray-800 dark:bg-gray-700 dark:text-gray-200' :
            'bg-amber-100 text-amber-800 dark:bg-amber-900 dark:text-amber-200'
          }`}>
            {stats?.tier?.toUpperCase()} Tier
          </span>
        </div>

        <nav className="p-4">
          <ul className="space-y-2">
            <li>
              <a href="#" className="flex items-center gap-3 px-4 py-3 rounded-lg bg-blue-50 dark:bg-blue-900/30 text-blue-900 dark:text-blue-300">
                <MdDashboard />
                <span>Dashboard</span>
              </a>
            </li>
            {stats?.partner_type === 'reseller' && (
              <>
                <li>
                  <a href="#" className="flex items-center gap-3 px-4 py-3 rounded-lg text-gray-700 dark:text-gray-300 hover:bg-gray-100 dark:hover:bg-gray-700">
                    <FaUsers />
                    <span>Clients</span>
                  </a>
                </li>
                <li>
                  <a href="#" className="flex items-center gap-3 px-4 py-3 rounded-lg text-gray-700 dark:text-gray-300 hover:bg-gray-100 dark:hover:bg-gray-700">
                    <FaFileAlt />
                    <span>Services</span>
                  </a>
                </li>
              </>
            )}
            {stats?.partner_type === 'agency' && (
              <li>
                <a href="#" className="flex items-center gap-3 px-4 py-3 rounded-lg text-gray-700 dark:text-gray-300 hover:bg-gray-100 dark:hover:bg-gray-700">
                  <FaUserPlus />
                  <span>Referrals</span>
                </a>
              </li>
            )}
            <li>
              <a href="#" className="flex items-center gap-3 px-4 py-3 rounded-lg text-gray-700 dark:text-gray-300 hover:bg-gray-100 dark:hover:bg-gray-700">
                <FaDollarSign />
                <span>Earnings</span>
              </a>
            </li>
            <li>
              <a href="#" className="flex items-center gap-3 px-4 py-3 rounded-lg text-gray-700 dark:text-gray-300 hover:bg-gray-100 dark:hover:bg-gray-700">
                <FaCog />
                <span>Settings</span>
              </a>
            </li>
          </ul>
        </nav>

        <div className="absolute bottom-0 left-0 right-0 p-4 border-t border-gray-200 dark:border-gray-700">
          <button
            onClick={handleLogout}
            className="flex items-center gap-3 px-4 py-3 w-full rounded-lg text-red-600 dark:text-red-400 hover:bg-red-50 dark:hover:bg-red-900/20"
          >
            <FaSignOutAlt />
            <span>Logout</span>
          </button>
        </div>
      </aside>

      {/* Main Content */}
      <main className="ml-64 p-8">
        <h1 className="text-3xl font-bold text-blue-900 dark:text-blue-300 mb-8">
          Welcome back, {stats?.company_name}!
        </h1>

        {/* Stats Grid */}
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-8">
          {stats?.partner_type === 'reseller' && (
            <>
              <StatCard
                title="Total Clients"
                value={`${stats.total_clients || 0} / ${stats.max_clients || 50}`}
                icon={<FaUsers className="w-6 h-6" />}
                color="blue"
              />
              <StatCard
                title="Discount Rate"
                value={`${stats.discount_percentage || 0}%`}
                icon={<MdTrendingUp className="w-6 h-6" />}
                color="green"
              />
              <StatCard
                title="Commission Rate"
                value={`${stats.commission_percentage || 0}%`}
                icon={<FaChartLine className="w-6 h-6" />}
                color="yellow"
              />
              <StatCard
                title="Total Earnings"
                value={`$${stats.total_earnings || '0.00'}`}
                icon={<FaDollarSign className="w-6 h-6" />}
                color="purple"
              />
            </>
          )}

          {stats?.partner_type === 'agency' && (
            <>
              <StatCard
                title="Active Referrals"
                value={stats.active_referrals || 0}
                icon={<FaUsers className="w-6 h-6" />}
                color="blue"
              />
              <StatCard
                title="Total Referrals"
                value={stats.total_referrals || 0}
                icon={<FaHandshake className="w-6 h-6" />}
                color="green"
              />
              <StatCard
                title="Bonus Rate"
                value={`${stats.referral_bonus_percentage || 0}%`}
                icon={<MdTrendingUp className="w-6 h-6" />}
                color="yellow"
              />
              <StatCard
                title="Total Earned"
                value={`$${stats.total_bonus_earned || '0.00'}`}
                icon={<FaDollarSign className="w-6 h-6" />}
                color="purple"
              />
            </>
          )}
        </div>

        {/* Quick Actions */}
        <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
          <div className="bg-white dark:bg-gray-800 rounded-xl shadow-sm p-6">
            <h2 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-4">
              Quick Actions
            </h2>
            <div className="space-y-3">
              {stats?.partner_type === 'reseller' && (
                <>
                  <button className="w-full py-3 px-4 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors">
                    Add New Client
                  </button>
                  <button className="w-full py-3 px-4 bg-green-600 text-white rounded-lg hover:bg-green-700 transition-colors">
                    Provision Service
                  </button>
                </>
              )}
              {stats?.partner_type === 'agency' && (
                <>
                  <button className="w-full py-3 px-4 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors">
                    Submit New Referral
                  </button>
                  <button className="w-full py-3 px-4 bg-green-600 text-white rounded-lg hover:bg-green-700 transition-colors">
                    View Referral History
                  </button>
                </>
              )}
              <button className="w-full py-3 px-4 bg-purple-600 text-white rounded-lg hover:bg-purple-700 transition-colors">
                Request Payout
              </button>
            </div>
          </div>

          <div className="bg-white dark:bg-gray-800 rounded-xl shadow-sm p-6">
            <h2 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-4">
              Recent Activity
            </h2>
            <div className="space-y-3 text-sm">
              <div className="py-3 border-b border-gray-100 dark:border-gray-700">
                <span className="text-gray-600 dark:text-gray-400">Welcome to the partner program!</span>
              </div>
              <div className="py-3 border-b border-gray-100 dark:border-gray-700">
                <span className="text-gray-600 dark:text-gray-400">Your account has been activated</span>
              </div>
              <div className="py-3">
                <span className="text-gray-600 dark:text-gray-400">Complete your profile to get started</span>
              </div>
            </div>
          </div>
        </div>
      </main>
    </div>
  );
}

function StatCard({ title, value, icon, color }: { 
  title: string; 
  value: string | number; 
  icon: React.ReactNode;
  color: 'blue' | 'green' | 'yellow' | 'purple';
}) {
  const colors = {
    blue: 'bg-blue-50 dark:bg-blue-900/30 text-blue-600 dark:text-blue-300',
    green: 'bg-green-50 dark:bg-green-900/30 text-green-600 dark:text-green-300',
    yellow: 'bg-yellow-50 dark:bg-yellow-900/30 text-yellow-600 dark:text-yellow-300',
    purple: 'bg-purple-50 dark:bg-purple-900/30 text-purple-600 dark:text-purple-300',
  };

  return (
    <div className="bg-white dark:bg-gray-800 rounded-xl shadow-sm p-6">
      <div className="flex items-center justify-between">
        <div>
          <p className="text-sm text-gray-600 dark:text-gray-400">{title}</p>
          <p className="text-2xl font-bold text-gray-900 dark:text-white mt-1">{value}</p>
        </div>
        <div className={`p-3 rounded-full ${colors[color]}`}>
          {icon}
        </div>
      </div>
    </div>
  );
}

function PartnerApplicationForm({ onSuccess }: { onSuccess: () => void }) {
  const [formData, setFormData] = useState({
    partner_type: 'reseller',
    company_name: '',
    specialization: '',
    product_name: '',
    integration_type: 'API',
  });
  const [submitting, setSubmitting] = useState(false);
  const [message, setMessage] = useState('');

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setSubmitting(true);
    setMessage('');

    try {
      const API_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:8000/api';
      const token = localStorage.getItem('access_token');

      const response = await fetch(`${API_URL}/reseller/partners/apply/`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'Authorization': `Bearer ${token}`
        },
        body: JSON.stringify(formData)
      });

      if (response.ok) {
        setMessage('Application submitted successfully!');
        setTimeout(onSuccess, 1500);
      } else {
        const error = await response.json();
        setMessage(error.detail || 'Failed to submit application');
      }
    } catch {
      setMessage('An error occurred. Please try again.');
    } finally {
      setSubmitting(false);
    }
  };

  return (
    <div className="min-h-screen bg-gradient-to-b from-blue-50 to-white dark:from-gray-900 dark:to-gray-800 py-20 px-4">
      <div className="max-w-2xl mx-auto">
        <div className="text-center mb-12">
          <h1 className="text-4xl font-bold text-blue-900 dark:text-blue-300 mb-4">
            Become a Partner
          </h1>
          <p className="text-lg text-gray-600 dark:text-gray-400">
            Join our partner program and start earning with Slyker Tech
          </p>
        </div>

        <div className="bg-white dark:bg-gray-800 rounded-2xl shadow-xl p-8">
          {message && (
            <div className={`mb-6 p-4 rounded-lg ${
              message.includes('success')
                ? 'bg-green-100 dark:bg-green-900 text-green-800 dark:text-green-200'
                : 'bg-red-100 dark:bg-red-900 text-red-800 dark:text-red-200'
            }`}>
              {message}
            </div>
          )}

          <form onSubmit={handleSubmit} className="space-y-6">
            <div>
              <label className="block text-gray-700 dark:text-gray-300 mb-2 font-medium">
                Partnership Type *
              </label>
              <select
                value={formData.partner_type}
                onChange={(e) => setFormData({ ...formData, partner_type: e.target.value })}
                className="w-full px-4 py-3 rounded-lg border border-gray-300 dark:border-gray-600 bg-white dark:bg-gray-700 text-gray-900 dark:text-gray-100"
              >
                <option value="reseller">Reseller</option>
                <option value="agency">Agency Partner</option>
                <option value="technology">Technology Alliance</option>
              </select>
            </div>

            <div>
              <label className="block text-gray-700 dark:text-gray-300 mb-2 font-medium">
                Company Name *
              </label>
              <input
                type="text"
                value={formData.company_name}
                onChange={(e) => setFormData({ ...formData, company_name: e.target.value })}
                required
                className="w-full px-4 py-3 rounded-lg border border-gray-300 dark:border-gray-600 bg-white dark:bg-gray-700 text-gray-900 dark:text-gray-100"
              />
            </div>

            {formData.partner_type === 'agency' && (
              <div>
                <label className="block text-gray-700 dark:text-gray-300 mb-2 font-medium">
                  Specialization
                </label>
                <input
                  type="text"
                  value={formData.specialization}
                  onChange={(e) => setFormData({ ...formData, specialization: e.target.value })}
                  placeholder="e.g., Web Development, Marketing"
                  className="w-full px-4 py-3 rounded-lg border border-gray-300 dark:border-gray-600 bg-white dark:bg-gray-700 text-gray-900 dark:text-gray-100"
                />
              </div>
            )}

            {formData.partner_type === 'technology' && (
              <>
                <div>
                  <label className="block text-gray-700 dark:text-gray-300 mb-2 font-medium">
                    Product Name
                  </label>
                  <input
                    type="text"
                    value={formData.product_name}
                    onChange={(e) => setFormData({ ...formData, product_name: e.target.value })}
                    className="w-full px-4 py-3 rounded-lg border border-gray-300 dark:border-gray-600 bg-white dark:bg-gray-700 text-gray-900 dark:text-gray-100"
                  />
                </div>
                <div>
                  <label className="block text-gray-700 dark:text-gray-300 mb-2 font-medium">
                    Integration Type
                  </label>
                  <select
                    value={formData.integration_type}
                    onChange={(e) => setFormData({ ...formData, integration_type: e.target.value })}
                    className="w-full px-4 py-3 rounded-lg border border-gray-300 dark:border-gray-600 bg-white dark:bg-gray-700 text-gray-900 dark:text-gray-100"
                  >
                    <option value="API">API Integration</option>
                    <option value="Plugin">Plugin</option>
                    <option value="SDK">SDK</option>
                    <option value="Other">Other</option>
                  </select>
                </div>
              </>
            )}

            <button
              type="submit"
              disabled={submitting}
              className="w-full py-4 bg-blue-600 hover:bg-blue-700 text-white rounded-lg font-semibold transition-colors disabled:opacity-50"
            >
              {submitting ? 'Submitting...' : 'Submit Application'}
            </button>
          </form>
        </div>

        <div className="text-center mt-8">
          <Link href="/partner" className="text-blue-600 dark:text-blue-400 hover:underline">
            Learn more about our partnership programs →
          </Link>
        </div>
      </div>
    </div>
  );
}
