'use client';

import { useState, useEffect, useCallback } from 'react';
import { useRouter } from 'next/navigation';
import { FaPlus, FaEdit, FaTrash, FaSync, FaWifi, FaGlobe, FaTimes } from 'react-icons/fa';
import { useAuthStore } from '@/lib/stores/auth-store';
import { useWebSocket } from '@/lib/hooks/use-websocket';

interface DNSRecord {
  id: number;
  record_type: string;
  name: string;
  content: string;
  ttl: number;
  priority: number | null;
  domain?: string;
  is_active?: boolean;
  subscription__service__name?: string;
}

const RECORD_TYPES = ['A', 'AAAA', 'CNAME', 'MX', 'TXT', 'NS', 'SRV', 'CAA'];

export default function DNSPanel() {
  const router = useRouter();
  const { isAuthenticated, isLoading: authLoading } = useAuthStore();
  const [records, setRecords] = useState<DNSRecord[]>([]);
  const [loading, setLoading] = useState(true);
  const [showAddModal, setShowAddModal] = useState(false);
  const [editingRecord, setEditingRecord] = useState<DNSRecord | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [successMessage, setSuccessMessage] = useState<string | null>(null);

  // WebSocket connection for real-time DNS updates
  const { isConnected, sendMessage } = useWebSocket({
    endpoint: '/ws/dns/',
    onMessage: (data: unknown) => {
      if (data && typeof data === 'object' && 'type' in data) {
        const message = data as { type: string; data: unknown };
        
        if (message.type === 'dns_records') {
          setRecords(message.data as DNSRecord[]);
          setLoading(false);
        } else if (message.type === 'dns_update') {
          // Refresh records on any mutation
          requestRecords();
        } else if (message.type === 'record_created') {
          const result = message.data as { success: boolean; error?: string };
          if (result.success) {
            setSuccessMessage('DNS record created successfully');
            setShowAddModal(false);
          } else {
            setError(result.error || 'Failed to create record');
          }
        } else if (message.type === 'record_updated') {
          const result = message.data as { success: boolean; error?: string };
          if (result.success) {
            setSuccessMessage('DNS record updated successfully');
            setEditingRecord(null);
          } else {
            setError(result.error || 'Failed to update record');
          }
        } else if (message.type === 'record_deleted') {
          const result = message.data as { success: boolean; error?: string };
          if (result.success) {
            setSuccessMessage('DNS record deleted successfully');
          } else {
            setError(result.error || 'Failed to delete record');
          }
        } else if (message.type === 'error') {
          const result = message.data as { message?: string };
          setError(result.message || 'An error occurred');
        }
      }
    },
  });

  // Auto-dismiss messages
  useEffect(() => {
    if (error || successMessage) {
      const timer = setTimeout(() => {
        setError(null);
        setSuccessMessage(null);
      }, 5000);
      return () => clearTimeout(timer);
    }
  }, [error, successMessage]);

  useEffect(() => {
    if (!authLoading && !isAuthenticated) {
      router.push('/login');
    }
  }, [isAuthenticated, authLoading, router]);

  const requestRecords = useCallback(() => {
    setLoading(true);
    sendMessage({ type: 'request_records' });
  }, [sendMessage]);

  useEffect(() => {
    if (isConnected) {
      requestRecords();
    }
  }, [isConnected, requestRecords]);

  const handleDelete = (id: number) => {
    if (confirm('Are you sure you want to delete this DNS record?')) {
      sendMessage({ type: 'delete_record', id });
    }
  };

  const handleCreate = (formData: Omit<DNSRecord, 'id'> & { subscription_id?: number }) => {
    sendMessage({ type: 'create_record', record: formData });
  };

  const handleUpdate = (id: number, formData: Partial<DNSRecord>) => {
    sendMessage({ type: 'update_record', id, record: formData });
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
      <div className="max-w-7xl mx-auto">
        {/* Notification banners */}
        {error && (
          <div className="mb-4 p-4 bg-red-50 dark:bg-red-900/20 border border-red-200 dark:border-red-800 rounded-lg flex items-center justify-between">
            <span className="text-red-800 dark:text-red-200">{error}</span>
            <button onClick={() => setError(null)} className="text-red-600 dark:text-red-400 hover:opacity-70">
              <FaTimes />
            </button>
          </div>
        )}
        {successMessage && (
          <div className="mb-4 p-4 bg-green-50 dark:bg-green-900/20 border border-green-200 dark:border-green-800 rounded-lg flex items-center justify-between">
            <span className="text-green-800 dark:text-green-200">{successMessage}</span>
            <button onClick={() => setSuccessMessage(null)} className="text-green-600 dark:text-green-400 hover:opacity-70">
              <FaTimes />
            </button>
          </div>
        )}

        {/* Header */}
        <div className="mb-8">
          <div className="flex items-center justify-between">
            <div>
              <h1 className="text-3xl font-bold text-gray-900 dark:text-white mb-2 flex items-center gap-3">
                <FaGlobe className="text-blue-600" />
                DNS Management Panel
              </h1>
              <p className="text-gray-600 dark:text-gray-400">
                Manage your DNS records with real-time updates
              </p>
            </div>
            <div className="flex items-center gap-4">
              {/* WebSocket Status */}
              <div className="flex items-center gap-2 px-3 py-2 bg-white dark:bg-gray-800 rounded-lg shadow">
                <FaWifi className={`${isConnected ? 'text-green-500' : 'text-red-500'}`} />
                <span className="text-sm text-gray-600 dark:text-gray-400">
                  {isConnected ? 'Live' : 'Offline'}
                </span>
              </div>
              
              <button
                onClick={requestRecords}
                className="flex items-center gap-2 px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors"
              >
                <FaSync className={loading ? 'animate-spin' : ''} />
                Refresh
              </button>
              
              <button
                onClick={() => setShowAddModal(true)}
                className="flex items-center gap-2 px-4 py-2 bg-green-600 text-white rounded-lg hover:bg-green-700 transition-colors"
              >
                <FaPlus />
                Add Record
              </button>
            </div>
          </div>
        </div>

        {/* DNS Records Table */}
        <div className="bg-white dark:bg-gray-800 rounded-lg shadow-lg overflow-hidden">
          <div className="overflow-x-auto">
            <table className="w-full">
              <thead className="bg-gray-50 dark:bg-gray-700">
                <tr>
                  <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 dark:text-gray-300 uppercase tracking-wider">
                    Type
                  </th>
                  <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 dark:text-gray-300 uppercase tracking-wider">
                    Name
                  </th>
                  <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 dark:text-gray-300 uppercase tracking-wider">
                    Content
                  </th>
                  <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 dark:text-gray-300 uppercase tracking-wider">
                    TTL
                  </th>
                  <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 dark:text-gray-300 uppercase tracking-wider">
                    Priority
                  </th>
                  <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 dark:text-gray-300 uppercase tracking-wider">
                    Domain
                  </th>
                  <th className="px-6 py-3 text-right text-xs font-medium text-gray-500 dark:text-gray-300 uppercase tracking-wider">
                    Actions
                  </th>
                </tr>
              </thead>
              <tbody className="bg-white dark:bg-gray-800 divide-y divide-gray-200 dark:divide-gray-700">
                {loading ? (
                  <tr>
                    <td colSpan={7} className="px-6 py-4 text-center text-gray-500 dark:text-gray-400">
                      Loading DNS records...
                    </td>
                  </tr>
                ) : records.length === 0 ? (
                  <tr>
                    <td colSpan={7} className="px-6 py-8 text-center text-gray-500 dark:text-gray-400">
                      <FaGlobe className="text-4xl mx-auto mb-2 opacity-50" />
                      <p>No DNS records found</p>
                      <p className="text-sm mt-2">Click &quot;Add Record&quot; to create your first DNS record</p>
                    </td>
                  </tr>
                ) : (
                  records.map((record) => (
                    <tr key={record.id} className="hover:bg-gray-50 dark:hover:bg-gray-700">
                      <td className="px-6 py-4 whitespace-nowrap">
                        <span className="px-2 inline-flex text-xs leading-5 font-semibold rounded-full bg-blue-100 text-blue-800 dark:bg-blue-900 dark:text-blue-200">
                          {record.record_type}
                        </span>
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-900 dark:text-gray-100">
                        {record.name}
                      </td>
                      <td className="px-6 py-4 text-sm text-gray-900 dark:text-gray-100 max-w-xs truncate">
                        {record.content}
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500 dark:text-gray-400">
                        {record.ttl}
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500 dark:text-gray-400">
                        {record.priority ?? '-'}
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500 dark:text-gray-400">
                        {record.domain || '-'}
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-right text-sm font-medium">
                        <button
                          onClick={() => setEditingRecord(record)}
                          className="text-blue-600 hover:text-blue-900 dark:text-blue-400 dark:hover:text-blue-300 mr-3"
                        >
                          <FaEdit />
                        </button>
                        <button
                          onClick={() => handleDelete(record.id)}
                          className="text-red-600 hover:text-red-900 dark:text-red-400 dark:hover:text-red-300"
                        >
                          <FaTrash />
                        </button>
                      </td>
                    </tr>
                  ))
                )}
              </tbody>
            </table>
          </div>
        </div>

        {/* Info Panel */}
        <div className="mt-8 bg-blue-50 dark:bg-blue-900/20 rounded-lg p-6 border border-blue-200 dark:border-blue-800">
          <h3 className="text-lg font-semibold text-blue-900 dark:text-blue-300 mb-2">
            DNS Record Types
          </h3>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4 text-sm text-blue-800 dark:text-blue-200">
            <div>
              <strong>A Record:</strong> Maps domain to IPv4 address
            </div>
            <div>
              <strong>AAAA Record:</strong> Maps domain to IPv6 address
            </div>
            <div>
              <strong>CNAME Record:</strong> Creates an alias for another domain
            </div>
            <div>
              <strong>MX Record:</strong> Specifies mail server for domain
            </div>
            <div>
              <strong>TXT Record:</strong> Stores text information
            </div>
            <div>
              <strong>NS Record:</strong> Delegates subdomain to nameservers
            </div>
          </div>
        </div>
      </div>

      {/* Add Record Modal */}
      {showAddModal && (
        <DNSRecordModal
          title="Add DNS Record"
          onClose={() => setShowAddModal(false)}
          onSubmit={(data) => handleCreate(data)}
        />
      )}

      {/* Edit Record Modal */}
      {editingRecord && (
        <DNSRecordModal
          title="Edit DNS Record"
          initialData={editingRecord}
          onClose={() => setEditingRecord(null)}
          onSubmit={(data) => handleUpdate(editingRecord.id, data)}
        />
      )}
    </div>
  );
}

interface DNSRecordModalProps {
  title: string;
  initialData?: Partial<DNSRecord>;
  onClose: () => void;
  onSubmit: (data: Record<string, unknown>) => void;
}

function DNSRecordModal({ title, initialData, onClose, onSubmit }: DNSRecordModalProps) {
  const [recordType, setRecordType] = useState(initialData?.record_type || 'A');
  const [name, setName] = useState(initialData?.name || '');
  const [content, setContent] = useState(initialData?.content || '');
  const [ttl, setTtl] = useState(String(initialData?.ttl ?? 3600));
  const [priority, setPriority] = useState(String(initialData?.priority ?? ''));
  const [domain, setDomain] = useState(initialData?.domain || '');

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    const data: Record<string, unknown> = {
      record_type: recordType,
      name,
      content,
      ttl: parseInt(ttl, 10),
      domain,
    };
    if (priority) {
      data.priority = parseInt(priority, 10);
    }
    onSubmit(data);
  };

  return (
    <div className="fixed inset-0 z-50 flex items-center justify-center bg-black/50">
      <div className="bg-white dark:bg-gray-800 rounded-lg shadow-xl w-full max-w-md p-6">
        <div className="flex items-center justify-between mb-4">
          <h2 className="text-xl font-semibold text-gray-900 dark:text-white">{title}</h2>
          <button onClick={onClose} className="text-gray-500 hover:text-gray-700 dark:text-gray-400 dark:hover:text-gray-200">
            <FaTimes />
          </button>
        </div>
        <form onSubmit={handleSubmit} className="space-y-4">
          <div>
            <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">Domain</label>
            <input
              type="text"
              value={domain}
              onChange={(e) => setDomain(e.target.value)}
              required
              placeholder="example.com"
              className="w-full px-3 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-gray-100"
            />
          </div>
          <div>
            <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">Record Type</label>
            <select
              value={recordType}
              onChange={(e) => setRecordType(e.target.value)}
              className="w-full px-3 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-gray-100"
            >
              {RECORD_TYPES.map((t) => (
                <option key={t} value={t}>{t}</option>
              ))}
            </select>
          </div>
          <div>
            <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">Name</label>
            <input
              type="text"
              value={name}
              onChange={(e) => setName(e.target.value)}
              required
              placeholder="@ or subdomain"
              className="w-full px-3 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-gray-100"
            />
          </div>
          <div>
            <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">Content</label>
            <input
              type="text"
              value={content}
              onChange={(e) => setContent(e.target.value)}
              required
              placeholder="IP address or value"
              className="w-full px-3 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-gray-100"
            />
          </div>
          <div>
            <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">TTL</label>
            <input
              type="number"
              value={ttl}
              onChange={(e) => setTtl(e.target.value)}
              min={60}
              max={86400}
              className="w-full px-3 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-gray-100"
            />
          </div>
          {(recordType === 'MX' || recordType === 'SRV') && (
            <div>
              <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-1">
                Priority {recordType === 'MX' && <span className="text-red-500">*</span>}
              </label>
              <input
                type="number"
                value={priority}
                onChange={(e) => setPriority(e.target.value)}
                required={recordType === 'MX'}
                min={0}
                placeholder="10"
                className="w-full px-3 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-gray-100"
              />
            </div>
          )}
          <div className="flex justify-end gap-3 pt-2">
            <button
              type="button"
              onClick={onClose}
              className="px-4 py-2 text-gray-700 dark:text-gray-300 bg-gray-100 dark:bg-gray-700 rounded-lg hover:bg-gray-200 dark:hover:bg-gray-600 transition-colors"
            >
              Cancel
            </button>
            <button
              type="submit"
              className="px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors"
            >
              {initialData ? 'Update' : 'Create'}
            </button>
          </div>
        </form>
      </div>
    </div>
  );
}
