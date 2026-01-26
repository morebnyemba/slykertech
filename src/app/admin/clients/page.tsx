'use client';

import { useState, useEffect, useCallback } from 'react';
import AdminLayout from '@/components/admin/AdminLayout';
import { apiService } from '@/lib/api-service';
import { FaUsers, FaSpinner, FaSearch, FaEnvelope, FaPhone } from 'react-icons/fa';

interface Client {
  id: number;
  company_name: string;
  user?: {
    email: string;
    first_name?: string;
    last_name?: string;
  };
  phone?: string;
  created_at: string;
}

export default function ClientsPage() {
  const [clients, setClients] = useState<Client[]>([]);
  const [loading, setLoading] = useState(true);
  const [searchTerm, setSearchTerm] = useState('');

  const fetchClients = useCallback(async () => {
    try {
      const response = await apiService.getAllClients();
      if (response.data) {
        setClients(response.data as Client[]);
      }
    } catch (error) {
      console.error('Failed to fetch clients:', error);
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => {
    fetchClients();
  }, [fetchClients]);

  const filteredClients = clients.filter(client =>
    client.company_name?.toLowerCase().includes(searchTerm.toLowerCase()) ||
    client.user?.email?.toLowerCase().includes(searchTerm.toLowerCase())
  );

  return (
    <AdminLayout>
      <div className="space-y-6">
        {/* Header */}
        <div className="flex flex-col sm:flex-row sm:items-center sm:justify-between gap-4">
          <div>
            <h1 className="text-2xl font-bold text-gray-900 dark:text-white">
              Clients
            </h1>
            <p className="text-gray-500 dark:text-gray-400">
              Manage all registered clients
            </p>
          </div>

          {/* Search */}
          <div className="relative">
            <FaSearch className="absolute left-3 top-1/2 transform -translate-y-1/2 text-gray-400" />
            <input
              type="text"
              placeholder="Search clients..."
              value={searchTerm}
              onChange={(e) => setSearchTerm(e.target.value)}
              className="pl-10 pr-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-800 text-gray-900 dark:text-white"
            />
          </div>
        </div>

        {/* Loading */}
        {loading ? (
          <div className="flex items-center justify-center py-12">
            <FaSpinner className="animate-spin h-8 w-8 text-blue-600" />
          </div>
        ) : (
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
            {filteredClients.length === 0 ? (
              <div className="col-span-full text-center py-12 bg-white dark:bg-gray-800 rounded-xl">
                <FaUsers className="h-12 w-12 text-gray-400 mx-auto mb-4" />
                <p className="text-gray-500 dark:text-gray-400">No clients found</p>
              </div>
            ) : (
              filteredClients.map((client) => (
                <div
                  key={client.id}
                  className="bg-white dark:bg-gray-800 rounded-xl shadow-lg p-6 hover:shadow-xl transition-shadow"
                >
                  <div className="flex items-center gap-4 mb-4">
                    <div className="w-12 h-12 bg-blue-100 dark:bg-blue-900/30 rounded-full flex items-center justify-center">
                      <span className="text-blue-600 dark:text-blue-400 font-bold text-lg">
                        {client.company_name?.[0]?.toUpperCase() || 'C'}
                      </span>
                    </div>
                    <div>
                      <h3 className="font-semibold text-gray-900 dark:text-white">
                        {client.company_name}
                      </h3>
                      {client.user?.first_name && (
                        <p className="text-sm text-gray-500 dark:text-gray-400">
                          {client.user.first_name} {client.user.last_name}
                        </p>
                      )}
                    </div>
                  </div>
                  
                  <div className="space-y-2">
                    {client.user?.email && (
                      <div className="flex items-center gap-2 text-sm text-gray-600 dark:text-gray-400">
                        <FaEnvelope className="h-4 w-4" />
                        <span className="truncate">{client.user.email}</span>
                      </div>
                    )}
                    {client.phone && (
                      <div className="flex items-center gap-2 text-sm text-gray-600 dark:text-gray-400">
                        <FaPhone className="h-4 w-4" />
                        <span>{client.phone}</span>
                      </div>
                    )}
                  </div>
                  
                  <div className="mt-4 pt-4 border-t border-gray-200 dark:border-gray-700">
                    <p className="text-xs text-gray-500 dark:text-gray-400">
                      Client since: {new Date(client.created_at).toLocaleDateString()}
                    </p>
                  </div>
                </div>
              ))
            )}
          </div>
        )}
      </div>
    </AdminLayout>
  );
}
