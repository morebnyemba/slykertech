'use client';

import React, { useState, useEffect } from 'react';
import { apiService } from '@/lib/api-service';

interface ResellerStats {
  totalClients: number;
  activeServices: number;
  monthlyRevenue: number;
  pendingCommissions: number;
  walletBalance: number;
}

export default function ResellerDashboard() {
  const [stats, setStats] = useState<ResellerStats | null>(null);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    fetchStats();
  }, []);

  const fetchStats = async () => {
    try {
      setLoading(true);
      // Fetch reseller stats from API
      const response = await apiService.get('/reseller/profiles/stats/');
      setStats(response.data);
    } catch (error) {
      console.error('Failed to fetch reseller stats:', error);
    } finally {
      setLoading(false);
    }
  };

  if (loading) {
    return <div className="animate-pulse">Loading dashboard...</div>;
  }

  return (
    <div className="p-6">
      <h1 className="text-3xl font-bold mb-6">Reseller Dashboard</h1>
      
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-8">
        <StatCard
          title="Total Clients"
          value={stats?.totalClients || 0}
          icon="👥"
          color="blue"
        />
        <StatCard
          title="Active Services"
          value={stats?.activeServices || 0}
          icon="⚙️"
          color="green"
        />
        <StatCard
          title="Monthly Revenue"
          value={`$${stats?.monthlyRevenue || 0}`}
          icon="💰"
          color="yellow"
        />
        <StatCard
          title="Wallet Balance"
          value={`$${stats?.walletBalance || 0}`}
          icon="💳"
          color="purple"
        />
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        <div className="bg-white dark:bg-gray-800 rounded-lg shadow p-6">
          <h2 className="text-xl font-semibold mb-4">Quick Actions</h2>
          <div className="space-y-3">
            <button className="w-full py-2 px-4 bg-blue-600 text-white rounded hover:bg-blue-700">
              Create New Client
            </button>
            <button className="w-full py-2 px-4 bg-green-600 text-white rounded hover:bg-green-700">
              Provision Service
            </button>
            <button className="w-full py-2 px-4 bg-purple-600 text-white rounded hover:bg-purple-700">
              Top Up Wallet
            </button>
          </div>
        </div>

        <div className="bg-white dark:bg-gray-800 rounded-lg shadow p-6">
          <h2 className="text-xl font-semibold mb-4">Recent Activity</h2>
          <div className="space-y-2 text-sm">
            <div className="py-2 border-b">New client added</div>
            <div className="py-2 border-b">Service provisioned for client@example.com</div>
            <div className="py-2 border-b">Commission earned: $25.00</div>
          </div>
        </div>
      </div>
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
    blue: 'bg-blue-100 dark:bg-blue-900',
    green: 'bg-green-100 dark:bg-green-900',
    yellow: 'bg-yellow-100 dark:bg-yellow-900',
    purple: 'bg-purple-100 dark:bg-purple-900',
  };

  return (
    <div className={`${colors[color as keyof typeof colors]} rounded-lg p-6`}>
      <div className="flex items-center justify-between">
        <div>
          <p className="text-sm text-gray-600 dark:text-gray-300">{title}</p>
          <p className="text-2xl font-bold mt-1">{value}</p>
        </div>
        <div className="text-4xl">{icon}</div>
      </div>
    </div>
  );
}
