'use client';

import React, { useState, useEffect } from 'react';
import { useRouter } from 'next/navigation';
import { 
  FaUsers, FaDollarSign, FaServer, FaKey, FaCog, 
  FaSignOutAlt, FaPlus, FaWallet, FaChartLine 
} from 'react-icons/fa';
import { MdDashboard } from 'react-icons/md';

interface ResellerStats {
  totalClients: number;
  maxClients: number;
  activeServices: number;
  monthlyRevenue: number;
  pendingCommissions: number;
  walletBalance: number;
  discountPercentage: number;
  commissionPercentage: number;
  tier: string;
  apiRateLimit: number;
}

interface Client {
  id: number;
  client_email: string;
  notes: string;
  created_at: string;
}

export default function ResellerPortalPage() {
  const router = useRouter();
  const [stats, setStats] = useState<ResellerStats | null>(null);
  const [clients, setClients] = useState<Client[]>([]);
  const [loading, setLoading] = useState(true);
  const [activeTab, setActiveTab] = useState('dashboard');
  const [isAuthenticated, setIsAuthenticated] = useState(false);
  const [showAddClient, setShowAddClient] = useState(false);

  useEffect(() => {
    checkAuthAndFetch();
  // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  const checkAuthAndFetch = async () => {
    const token = localStorage.getItem('access_token');
    if (!token) {
      router.push('/login?redirect=/portal/reseller');
      return;
    }
    setIsAuthenticated(true);
    await fetchData();
  };

  const fetchData = async () => {
    try {
      const API_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:8000/api';
      const token = localStorage.getItem('access_token');

      const [statsRes, clientsRes] = await Promise.all([
        fetch(`${API_URL}/reseller/resellers/stats/`, {
          headers: { 'Authorization': `Bearer ${token}` }
        }),
        fetch(`${API_URL}/reseller/reseller-clients/`, {
          headers: { 'Authorization': `Bearer ${token}` }
        })
      ]);

      if (statsRes.ok) {
        const data = await statsRes.json();
        setStats(data);
      }

      if (clientsRes.ok) {
        const data = await clientsRes.json();
        setClients(data.results || data);
      }
    } catch (error) {
      console.error('Failed to fetch reseller data:', error);
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
        <div className="animate-pulse text-gray-600 dark:text-gray-400">Loading reseller portal...</div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-gray-50 dark:bg-gray-900">
      {/* Sidebar */}
      <aside className="fixed left-0 top-0 h-full w-64 bg-white dark:bg-gray-800 shadow-lg z-40">
        <div className="p-6 border-b border-gray-200 dark:border-gray-700">
          <h2 className="text-xl font-bold text-blue-900 dark:text-blue-300">Reseller Portal</h2>
          <span className={`inline-block mt-2 px-3 py-1 rounded-full text-xs font-semibold ${
            stats?.tier === 'platinum' ? 'bg-blue-100 text-blue-800 dark:bg-blue-900 dark:text-blue-200' :
            stats?.tier === 'gold' ? 'bg-yellow-100 text-yellow-800 dark:bg-yellow-900 dark:text-yellow-200' :
            stats?.tier === 'silver' ? 'bg-gray-100 text-gray-800 dark:bg-gray-700 dark:text-gray-200' :
            'bg-amber-100 text-amber-800 dark:bg-amber-900 dark:text-amber-200'
          }`}>
            {stats?.tier?.toUpperCase() || 'BRONZE'} Tier
          </span>
        </div>

        <nav className="p-4">
          <ul className="space-y-2">
            <li>
              <button
                onClick={() => setActiveTab('dashboard')}
                className={`flex items-center gap-3 px-4 py-3 w-full rounded-lg ${
                  activeTab === 'dashboard' 
                    ? 'bg-blue-50 dark:bg-blue-900/30 text-blue-900 dark:text-blue-300'
                    : 'text-gray-700 dark:text-gray-300 hover:bg-gray-100 dark:hover:bg-gray-700'
                }`}
              >
                <MdDashboard />
                <span>Dashboard</span>
              </button>
            </li>
            <li>
              <button
                onClick={() => setActiveTab('clients')}
                className={`flex items-center gap-3 px-4 py-3 w-full rounded-lg ${
                  activeTab === 'clients' 
                    ? 'bg-blue-50 dark:bg-blue-900/30 text-blue-900 dark:text-blue-300'
                    : 'text-gray-700 dark:text-gray-300 hover:bg-gray-100 dark:hover:bg-gray-700'
                }`}
              >
                <FaUsers />
                <span>Clients</span>
              </button>
            </li>
            <li>
              <button
                onClick={() => setActiveTab('services')}
                className={`flex items-center gap-3 px-4 py-3 w-full rounded-lg ${
                  activeTab === 'services' 
                    ? 'bg-blue-50 dark:bg-blue-900/30 text-blue-900 dark:text-blue-300'
                    : 'text-gray-700 dark:text-gray-300 hover:bg-gray-100 dark:hover:bg-gray-700'
                }`}
              >
                <FaServer />
                <span>Services</span>
              </button>
            </li>
            <li>
              <button
                onClick={() => setActiveTab('api')}
                className={`flex items-center gap-3 px-4 py-3 w-full rounded-lg ${
                  activeTab === 'api' 
                    ? 'bg-blue-50 dark:bg-blue-900/30 text-blue-900 dark:text-blue-300'
                    : 'text-gray-700 dark:text-gray-300 hover:bg-gray-100 dark:hover:bg-gray-700'
                }`}
              >
                <FaKey />
                <span>API</span>
              </button>
            </li>
            <li>
              <button
                onClick={() => setActiveTab('earnings')}
                className={`flex items-center gap-3 px-4 py-3 w-full rounded-lg ${
                  activeTab === 'earnings' 
                    ? 'bg-blue-50 dark:bg-blue-900/30 text-blue-900 dark:text-blue-300'
                    : 'text-gray-700 dark:text-gray-300 hover:bg-gray-100 dark:hover:bg-gray-700'
                }`}
              >
                <FaDollarSign />
                <span>Earnings</span>
              </button>
            </li>
            <li>
              <button
                onClick={() => setActiveTab('settings')}
                className={`flex items-center gap-3 px-4 py-3 w-full rounded-lg ${
                  activeTab === 'settings' 
                    ? 'bg-blue-50 dark:bg-blue-900/30 text-blue-900 dark:text-blue-300'
                    : 'text-gray-700 dark:text-gray-300 hover:bg-gray-100 dark:hover:bg-gray-700'
                }`}
              >
                <FaCog />
                <span>Settings</span>
              </button>
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
        {activeTab === 'dashboard' && (
          <DashboardTab stats={stats} />
        )}
        {activeTab === 'clients' && (
          <ClientsTab 
            clients={clients} 
            maxClients={stats?.maxClients || 50}
            showAddClient={showAddClient}
            setShowAddClient={setShowAddClient}
            onRefresh={fetchData}
          />
        )}
        {activeTab === 'services' && (
          <ServicesTab />
        )}
        {activeTab === 'api' && (
          <APITab apiRateLimit={stats?.apiRateLimit || 1000} />
        )}
        {activeTab === 'earnings' && (
          <EarningsTab stats={stats} />
        )}
        {activeTab === 'settings' && (
          <SettingsTab />
        )}
      </main>
    </div>
  );
}

function DashboardTab({ stats }: { stats: ResellerStats | null }) {
  return (
    <>
      <h1 className="text-3xl font-bold text-blue-900 dark:text-blue-300 mb-8">
        Reseller Dashboard
      </h1>

      {/* Stats Grid */}
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-8">
        <div className="bg-white dark:bg-gray-800 rounded-xl shadow-sm p-6">
          <div className="flex items-center justify-between">
            <div>
              <p className="text-sm text-gray-600 dark:text-gray-400">Total Clients</p>
              <p className="text-2xl font-bold text-gray-900 dark:text-white mt-1">
                {stats?.totalClients || 0} / {stats?.maxClients || 50}
              </p>
            </div>
            <div className="p-3 bg-blue-50 dark:bg-blue-900/30 rounded-full">
              <FaUsers className="w-6 h-6 text-blue-600 dark:text-blue-300" />
            </div>
          </div>
        </div>

        <div className="bg-white dark:bg-gray-800 rounded-xl shadow-sm p-6">
          <div className="flex items-center justify-between">
            <div>
              <p className="text-sm text-gray-600 dark:text-gray-400">Active Services</p>
              <p className="text-2xl font-bold text-gray-900 dark:text-white mt-1">
                {stats?.activeServices || 0}
              </p>
            </div>
            <div className="p-3 bg-green-50 dark:bg-green-900/30 rounded-full">
              <FaServer className="w-6 h-6 text-green-600 dark:text-green-300" />
            </div>
          </div>
        </div>

        <div className="bg-white dark:bg-gray-800 rounded-xl shadow-sm p-6">
          <div className="flex items-center justify-between">
            <div>
              <p className="text-sm text-gray-600 dark:text-gray-400">Monthly Revenue</p>
              <p className="text-2xl font-bold text-gray-900 dark:text-white mt-1">
                ${stats?.monthlyRevenue?.toFixed(2) || '0.00'}
              </p>
            </div>
            <div className="p-3 bg-yellow-50 dark:bg-yellow-900/30 rounded-full">
              <FaChartLine className="w-6 h-6 text-yellow-600 dark:text-yellow-300" />
            </div>
          </div>
        </div>

        <div className="bg-white dark:bg-gray-800 rounded-xl shadow-sm p-6">
          <div className="flex items-center justify-between">
            <div>
              <p className="text-sm text-gray-600 dark:text-gray-400">Wallet Balance</p>
              <p className="text-2xl font-bold text-gray-900 dark:text-white mt-1">
                ${stats?.walletBalance?.toFixed(2) || '0.00'}
              </p>
            </div>
            <div className="p-3 bg-purple-50 dark:bg-purple-900/30 rounded-full">
              <FaWallet className="w-6 h-6 text-purple-600 dark:text-purple-300" />
            </div>
          </div>
        </div>
      </div>

      {/* Quick Actions */}
      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        <div className="bg-white dark:bg-gray-800 rounded-xl shadow-sm p-6">
          <h2 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-4">
            Quick Actions
          </h2>
          <div className="space-y-3">
            <button className="w-full py-3 px-4 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors flex items-center justify-center gap-2">
              <FaPlus /> Create New Client
            </button>
            <button className="w-full py-3 px-4 bg-green-600 text-white rounded-lg hover:bg-green-700 transition-colors">
              Provision Service
            </button>
            <button className="w-full py-3 px-4 bg-purple-600 text-white rounded-lg hover:bg-purple-700 transition-colors">
              Top Up Wallet
            </button>
          </div>
        </div>

        <div className="bg-white dark:bg-gray-800 rounded-xl shadow-sm p-6">
          <h2 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-4">
            Your Rates
          </h2>
          <div className="space-y-4">
            <div className="flex justify-between items-center py-3 border-b border-gray-100 dark:border-gray-700">
              <span className="text-gray-600 dark:text-gray-400">Discount Rate</span>
              <span className="font-semibold text-green-600 dark:text-green-400">
                {stats?.discountPercentage || 10}%
              </span>
            </div>
            <div className="flex justify-between items-center py-3 border-b border-gray-100 dark:border-gray-700">
              <span className="text-gray-600 dark:text-gray-400">Commission Rate</span>
              <span className="font-semibold text-blue-600 dark:text-blue-400">
                {stats?.commissionPercentage || 5}%
              </span>
            </div>
            <div className="flex justify-between items-center py-3">
              <span className="text-gray-600 dark:text-gray-400">API Rate Limit</span>
              <span className="font-semibold text-purple-600 dark:text-purple-400">
                {stats?.apiRateLimit || 1000}/day
              </span>
            </div>
          </div>
        </div>
      </div>
    </>
  );
}

function ClientsTab({ 
  clients, 
  maxClients,
  showAddClient,
  setShowAddClient,
  onRefresh 
}: { 
  clients: Client[]; 
  maxClients: number;
  showAddClient: boolean;
  setShowAddClient: (show: boolean) => void;
  onRefresh: () => void;
}) {
  return (
    <>
      <div className="flex justify-between items-center mb-8">
        <div>
          <h1 className="text-3xl font-bold text-blue-900 dark:text-blue-300">
            Clients
          </h1>
          <p className="text-gray-600 dark:text-gray-400 mt-1">
            {clients.length} of {maxClients} clients
          </p>
        </div>
        <button 
          onClick={() => setShowAddClient(true)}
          className="px-6 py-3 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors flex items-center gap-2"
        >
          <FaPlus /> Add Client
        </button>
      </div>

      {clients.length === 0 ? (
        <div className="bg-white dark:bg-gray-800 rounded-xl shadow-sm p-12 text-center">
          <FaUsers className="w-16 h-16 mx-auto mb-4 text-gray-400" />
          <h3 className="text-xl font-semibold text-gray-700 dark:text-gray-300 mb-2">
            No clients yet
          </h3>
          <p className="text-gray-500 dark:text-gray-400 mb-6">
            Start by adding your first client to the portal
          </p>
          <button 
            onClick={() => setShowAddClient(true)}
            className="px-6 py-3 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors"
          >
            Add Your First Client
          </button>
        </div>
      ) : (
        <div className="bg-white dark:bg-gray-800 rounded-xl shadow-sm overflow-hidden">
          <table className="w-full">
            <thead className="bg-gray-50 dark:bg-gray-700">
              <tr>
                <th className="px-6 py-4 text-left text-sm font-semibold text-gray-700 dark:text-gray-300">Email</th>
                <th className="px-6 py-4 text-left text-sm font-semibold text-gray-700 dark:text-gray-300">Notes</th>
                <th className="px-6 py-4 text-left text-sm font-semibold text-gray-700 dark:text-gray-300">Added</th>
                <th className="px-6 py-4 text-left text-sm font-semibold text-gray-700 dark:text-gray-300">Actions</th>
              </tr>
            </thead>
            <tbody>
              {clients.map((client) => (
                <tr key={client.id} className="border-t border-gray-100 dark:border-gray-700">
                  <td className="px-6 py-4">{client.client_email}</td>
                  <td className="px-6 py-4 text-gray-600 dark:text-gray-400">{client.notes || '-'}</td>
                  <td className="px-6 py-4 text-gray-600 dark:text-gray-400">
                    {new Date(client.created_at).toLocaleDateString()}
                  </td>
                  <td className="px-6 py-4">
                    <button className="text-blue-600 dark:text-blue-400 hover:underline">
                      View
                    </button>
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      )}

      {showAddClient && (
        <AddClientModal onClose={() => setShowAddClient(false)} onSuccess={onRefresh} />
      )}
    </>
  );
}

function AddClientModal({ onClose, onSuccess }: { onClose: () => void; onSuccess: () => void }) {
  const [email, setEmail] = useState('');
  const [notes, setNotes] = useState('');
  const [submitting, setSubmitting] = useState(false);

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setSubmitting(true);

    try {
      const API_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:8000/api';
      const token = localStorage.getItem('access_token');

      // Note: This would need a proper endpoint to create clients
      // For now, just close the modal
      onClose();
      onSuccess();
    } catch {
      // Handle error
    } finally {
      setSubmitting(false);
    }
  };

  return (
    <div className="fixed inset-0 bg-black/50 flex items-center justify-center z-50">
      <div className="bg-white dark:bg-gray-800 rounded-xl p-8 max-w-md w-full mx-4">
        <h2 className="text-2xl font-bold text-blue-900 dark:text-blue-300 mb-6">
          Add New Client
        </h2>
        <form onSubmit={handleSubmit} className="space-y-4">
          <div>
            <label className="block text-gray-700 dark:text-gray-300 mb-2">Email</label>
            <input
              type="email"
              value={email}
              onChange={(e) => setEmail(e.target.value)}
              required
              className="w-full px-4 py-3 rounded-lg border border-gray-300 dark:border-gray-600 bg-white dark:bg-gray-700"
            />
          </div>
          <div>
            <label className="block text-gray-700 dark:text-gray-300 mb-2">Notes</label>
            <textarea
              value={notes}
              onChange={(e) => setNotes(e.target.value)}
              rows={3}
              className="w-full px-4 py-3 rounded-lg border border-gray-300 dark:border-gray-600 bg-white dark:bg-gray-700"
            />
          </div>
          <div className="flex gap-4">
            <button
              type="button"
              onClick={onClose}
              className="flex-1 py-3 border border-gray-300 dark:border-gray-600 rounded-lg hover:bg-gray-100 dark:hover:bg-gray-700"
            >
              Cancel
            </button>
            <button
              type="submit"
              disabled={submitting}
              className="flex-1 py-3 bg-blue-600 text-white rounded-lg hover:bg-blue-700 disabled:opacity-50"
            >
              {submitting ? 'Adding...' : 'Add Client'}
            </button>
          </div>
        </form>
      </div>
    </div>
  );
}

function ServicesTab() {
  return (
    <>
      <h1 className="text-3xl font-bold text-blue-900 dark:text-blue-300 mb-8">
        Services
      </h1>
      <div className="bg-white dark:bg-gray-800 rounded-xl shadow-sm p-12 text-center">
        <FaServer className="w-16 h-16 mx-auto mb-4 text-gray-400" />
        <h3 className="text-xl font-semibold text-gray-700 dark:text-gray-300 mb-2">
          Coming Soon
        </h3>
        <p className="text-gray-500 dark:text-gray-400">
          Service provisioning will be available soon
        </p>
      </div>
    </>
  );
}

function APITab({ apiRateLimit }: { apiRateLimit: number }) {
  return (
    <>
      <h1 className="text-3xl font-bold text-blue-900 dark:text-blue-300 mb-8">
        API Access
      </h1>
      <div className="bg-white dark:bg-gray-800 rounded-xl shadow-sm p-6 mb-6">
        <h2 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-4">
          API Credentials
        </h2>
        <div className="space-y-4">
          <div>
            <label className="block text-sm text-gray-600 dark:text-gray-400 mb-1">API Key</label>
            <div className="flex gap-2">
              <input
                type="password"
                value="••••••••••••••••••••••••••••••••"
                readOnly
                className="flex-1 px-4 py-3 rounded-lg border border-gray-300 dark:border-gray-600 bg-gray-50 dark:bg-gray-700"
              />
              <button className="px-4 py-2 bg-gray-200 dark:bg-gray-600 rounded-lg">
                Show
              </button>
            </div>
          </div>
          <div className="pt-4 border-t border-gray-200 dark:border-gray-700">
            <p className="text-sm text-gray-600 dark:text-gray-400">
              Rate Limit: <span className="font-semibold">{apiRateLimit} requests/day</span>
            </p>
          </div>
        </div>
      </div>
      <div className="bg-white dark:bg-gray-800 rounded-xl shadow-sm p-6">
        <h2 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-4">
          Documentation
        </h2>
        <p className="text-gray-600 dark:text-gray-400 mb-4">
          Use our REST API to automate service provisioning and client management.
        </p>
        <a 
          href="#" 
          className="text-blue-600 dark:text-blue-400 hover:underline"
        >
          View API Documentation →
        </a>
      </div>
    </>
  );
}

function EarningsTab({ stats }: { stats: ResellerStats | null }) {
  return (
    <>
      <h1 className="text-3xl font-bold text-blue-900 dark:text-blue-300 mb-8">
        Earnings
      </h1>
      <div className="grid grid-cols-1 md:grid-cols-2 gap-6 mb-8">
        <div className="bg-white dark:bg-gray-800 rounded-xl shadow-sm p-6">
          <h3 className="text-gray-600 dark:text-gray-400 mb-2">Total Earnings</h3>
          <p className="text-4xl font-bold text-green-600 dark:text-green-400">
            ${stats?.monthlyRevenue?.toFixed(2) || '0.00'}
          </p>
        </div>
        <div className="bg-white dark:bg-gray-800 rounded-xl shadow-sm p-6">
          <h3 className="text-gray-600 dark:text-gray-400 mb-2">Pending Commissions</h3>
          <p className="text-4xl font-bold text-yellow-600 dark:text-yellow-400">
            ${stats?.pendingCommissions?.toFixed(2) || '0.00'}
          </p>
        </div>
      </div>
      <div className="bg-white dark:bg-gray-800 rounded-xl shadow-sm p-6">
        <h2 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-4">
          Commission History
        </h2>
        <p className="text-gray-500 dark:text-gray-400 text-center py-8">
          No commission history yet
        </p>
      </div>
    </>
  );
}

function SettingsTab() {
  return (
    <>
      <h1 className="text-3xl font-bold text-blue-900 dark:text-blue-300 mb-8">
        Settings
      </h1>
      <div className="bg-white dark:bg-gray-800 rounded-xl shadow-sm p-6">
        <h2 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-4">
          Branding Settings
        </h2>
        <p className="text-gray-600 dark:text-gray-400 mb-4">
          Customize your white-label branding settings here.
        </p>
        <div className="space-y-4">
          <div>
            <label className="block text-gray-700 dark:text-gray-300 mb-2">Brand Name</label>
            <input
              type="text"
              placeholder="Your Company Name"
              className="w-full px-4 py-3 rounded-lg border border-gray-300 dark:border-gray-600 bg-white dark:bg-gray-700"
            />
          </div>
          <div>
            <label className="block text-gray-700 dark:text-gray-300 mb-2">Webhook URL</label>
            <input
              type="url"
              placeholder="https://your-domain.com/webhook"
              className="w-full px-4 py-3 rounded-lg border border-gray-300 dark:border-gray-600 bg-white dark:bg-gray-700"
            />
          </div>
          <button className="px-6 py-3 bg-blue-600 text-white rounded-lg hover:bg-blue-700">
            Save Settings
          </button>
        </div>
      </div>
    </>
  );
}
