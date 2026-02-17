'use client';

import { useState, useEffect } from 'react';
import Link from 'next/link';
import {
  FaGlobe, FaSync, FaCheckCircle, FaClock, FaExclamationTriangle,
  FaNetworkWired, FaChevronDown, FaChevronUp, FaPlus, FaEdit, FaTrash, FaTimes
} from 'react-icons/fa';
import { useAuthStore } from '@/lib/stores/auth-store';
import { apiService } from '@/lib/api-service';

interface DomainRegistration {
  id: number;
  domain_name: string;
  status: string;
  registration_date: string;
  expiry_date: string;
  auto_renew: boolean;
  nameserver1?: string;
  nameserver2?: string;
}

interface DNSRecord {
  id: number;
  domain: string;
  record_type: string;
  name: string;
  content: string;
  ttl: number;
  priority: number | null;
  is_active?: boolean;
}

const RECORD_TYPES = ['A', 'AAAA', 'CNAME', 'MX', 'TXT', 'NS', 'SRV', 'CAA'];

export default function DomainsPage() {
  const { isAuthenticated } = useAuthStore();
  const [domains, setDomains] = useState<DomainRegistration[]>([]);
  const [dnsRecords, setDnsRecords] = useState<DNSRecord[]>([]);
  const [loading, setLoading] = useState(true);
  const [expandedDomain, setExpandedDomain] = useState<string | null>(null);
  const [showAddDNS, setShowAddDNS] = useState<string | null>(null);
  const [editingRecord, setEditingRecord] = useState<DNSRecord | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [successMessage, setSuccessMessage] = useState<string | null>(null);

  useEffect(() => {
    if (isAuthenticated) {
      loadData();
    }
  }, [isAuthenticated]);

  useEffect(() => {
    if (error || successMessage) {
      const timer = setTimeout(() => {
        setError(null);
        setSuccessMessage(null);
      }, 5000);
      return () => clearTimeout(timer);
    }
  }, [error, successMessage]);

  const loadData = async () => {
    setLoading(true);
    try {
      const [domainsResponse, dnsResponse] = await Promise.all([
        apiService.getDomainRegistrations(),
        apiService.getDNSRecords(),
      ]);

      if (domainsResponse.data) {
        const domainData = domainsResponse.data.results || domainsResponse.data;
        setDomains(Array.isArray(domainData) ? domainData : []);
      }

      if (dnsResponse.data) {
        const dnsData = dnsResponse.data.results || dnsResponse.data;
        setDnsRecords(Array.isArray(dnsData) ? dnsData : []);
      }
    } catch (err) {
      console.error('Failed to load domain data:', err);
    } finally {
      setLoading(false);
    }
  };

  const getDNSForDomain = (domainName: string) => {
    return dnsRecords.filter(
      (record) => record.domain === domainName || record.domain === domainName.replace(/\.$/, '')
    );
  };

  const getStatusBadge = (status: string) => {
    const statusConfig: Record<string, { color: string; icon: JSX.Element }> = {
      active: { color: 'bg-green-100 text-green-800 dark:bg-green-900 dark:text-green-200', icon: <FaCheckCircle /> },
      pending: { color: 'bg-yellow-100 text-yellow-800 dark:bg-yellow-900 dark:text-yellow-200', icon: <FaClock /> },
      expired: { color: 'bg-red-100 text-red-800 dark:bg-red-900 dark:text-red-200', icon: <FaExclamationTriangle /> },
    };
    const config = statusConfig[status.toLowerCase()] || statusConfig.pending;
    return (
      <span className={`inline-flex items-center gap-1 px-2 py-1 rounded-full text-xs font-medium ${config.color}`}>
        {config.icon}
        {status}
      </span>
    );
  };

  const handleCreateDNS = async (domainName: string, formData: Record<string, unknown>) => {
    try {
      const response = await apiService.createDNSRecord({
        ...formData,
        domain: domainName,
      });
      if (response.error) {
        setError(response.error);
      } else {
        setSuccessMessage('DNS record created successfully');
        setShowAddDNS(null);
        loadData();
      }
    } catch {
      setError('Failed to create DNS record');
    }
  };

  const handleUpdateDNS = async (id: number, formData: Record<string, unknown>) => {
    try {
      const response = await apiService.updateDNSRecord(id, formData);
      if (response.error) {
        setError(response.error);
      } else {
        setSuccessMessage('DNS record updated successfully');
        setEditingRecord(null);
        loadData();
      }
    } catch {
      setError('Failed to update DNS record');
    }
  };

  const handleDeleteDNS = async (id: number) => {
    if (!confirm('Are you sure you want to delete this DNS record?')) return;
    try {
      const response = await apiService.deleteDNSRecord(id);
      if (response.error) {
        setError(response.error);
      } else {
        setSuccessMessage('DNS record deleted successfully');
        loadData();
      }
    } catch {
      setError('Failed to delete DNS record');
    }
  };

  if (!isAuthenticated) {
    return null;
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
                My Domains
              </h1>
              <p className="text-gray-600 dark:text-gray-400">
                Manage your domains and their DNS records
              </p>
            </div>
            <div className="flex items-center gap-4">
              <button
                onClick={loadData}
                className="flex items-center gap-2 px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors"
              >
                <FaSync className={loading ? 'animate-spin' : ''} />
                Refresh
              </button>
              <Link
                href="/services/domains"
                className="flex items-center gap-2 px-4 py-2 bg-green-600 text-white rounded-lg hover:bg-green-700 transition-colors"
              >
                <FaPlus />
                Register Domain
              </Link>
            </div>
          </div>
        </div>

        {/* Domains List */}
        <div className="space-y-4">
          {loading ? (
            <div className="text-center py-12">
              <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-blue-600 mx-auto"></div>
              <p className="mt-4 text-gray-600 dark:text-gray-400">Loading domains...</p>
            </div>
          ) : domains.length === 0 ? (
            <div className="text-center py-12 bg-white dark:bg-gray-800 rounded-lg shadow">
              <FaGlobe className="text-5xl text-gray-400 mx-auto mb-4" />
              <p className="text-gray-600 dark:text-gray-400 mb-2">No domains registered yet</p>
              <p className="text-sm text-gray-500 dark:text-gray-500 mb-6">
                Register a domain to get started with DNS management
              </p>
              <Link
                href="/services/domains"
                className="inline-flex items-center gap-2 px-6 py-3 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors"
              >
                <FaPlus />
                Register a Domain
              </Link>
            </div>
          ) : (
            domains.map((domain) => {
              const domainDNS = getDNSForDomain(domain.domain_name);
              const isExpanded = expandedDomain === domain.domain_name;

              return (
                <div
                  key={domain.id}
                  className="bg-white dark:bg-gray-800 rounded-lg shadow-lg overflow-hidden"
                >
                  {/* Domain Header */}
                  <div
                    className="p-6 cursor-pointer hover:bg-gray-50 dark:hover:bg-gray-750 transition-colors"
                    onClick={() => setExpandedDomain(isExpanded ? null : domain.domain_name)}
                  >
                    <div className="flex items-center justify-between">
                      <div className="flex items-center gap-4">
                        <div className="p-3 bg-blue-100 dark:bg-blue-900 rounded-lg">
                          <FaGlobe className="text-xl text-blue-600 dark:text-blue-400" />
                        </div>
                        <div>
                          <h3 className="text-lg font-semibold text-gray-900 dark:text-white">
                            {domain.domain_name}
                          </h3>
                          <div className="flex items-center gap-4 text-sm text-gray-500 dark:text-gray-400 mt-1">
                            <span>Expires: {new Date(domain.expiry_date).toLocaleDateString()}</span>
                            <span>DNS Records: {domainDNS.length}</span>
                            {domain.auto_renew && (
                              <span className="text-green-600 dark:text-green-400">Auto-renew</span>
                            )}
                          </div>
                        </div>
                      </div>
                      <div className="flex items-center gap-4">
                        {getStatusBadge(domain.status)}
                        {isExpanded ? (
                          <FaChevronUp className="text-gray-400" />
                        ) : (
                          <FaChevronDown className="text-gray-400" />
                        )}
                      </div>
                    </div>
                  </div>

                  {/* Expanded DNS Records Section */}
                  {isExpanded && (
                    <div className="border-t border-gray-200 dark:border-gray-700">
                      <div className="p-6">
                        <div className="flex items-center justify-between mb-4">
                          <h4 className="text-lg font-semibold text-gray-900 dark:text-white flex items-center gap-2">
                            <FaNetworkWired className="text-blue-500" />
                            DNS Records
                          </h4>
                          <button
                            onClick={(e) => {
                              e.stopPropagation();
                              setShowAddDNS(domain.domain_name);
                            }}
                            className="flex items-center gap-2 px-3 py-1.5 bg-green-600 text-white rounded-lg hover:bg-green-700 transition-colors text-sm"
                          >
                            <FaPlus />
                            Add Record
                          </button>
                        </div>

                        {domainDNS.length === 0 ? (
                          <div className="text-center py-8 bg-gray-50 dark:bg-gray-900 rounded-lg">
                            <FaNetworkWired className="text-3xl text-gray-400 mx-auto mb-2" />
                            <p className="text-gray-500 dark:text-gray-400">No DNS records for this domain</p>
                            <p className="text-sm text-gray-400 dark:text-gray-500 mt-1">
                              Click &quot;Add Record&quot; to create your first DNS record
                            </p>
                          </div>
                        ) : (
                          <div className="overflow-x-auto">
                            <table className="w-full">
                              <thead className="bg-gray-50 dark:bg-gray-700">
                                <tr>
                                  <th className="px-4 py-2 text-left text-xs font-medium text-gray-500 dark:text-gray-300 uppercase">Type</th>
                                  <th className="px-4 py-2 text-left text-xs font-medium text-gray-500 dark:text-gray-300 uppercase">Name</th>
                                  <th className="px-4 py-2 text-left text-xs font-medium text-gray-500 dark:text-gray-300 uppercase">Content</th>
                                  <th className="px-4 py-2 text-left text-xs font-medium text-gray-500 dark:text-gray-300 uppercase">TTL</th>
                                  <th className="px-4 py-2 text-left text-xs font-medium text-gray-500 dark:text-gray-300 uppercase">Priority</th>
                                  <th className="px-4 py-2 text-right text-xs font-medium text-gray-500 dark:text-gray-300 uppercase">Actions</th>
                                </tr>
                              </thead>
                              <tbody className="divide-y divide-gray-200 dark:divide-gray-700">
                                {domainDNS.map((record) => (
                                  <tr key={record.id} className="hover:bg-gray-50 dark:hover:bg-gray-700">
                                    <td className="px-4 py-3">
                                      <span className="px-2 py-0.5 text-xs font-semibold rounded-full bg-blue-100 text-blue-800 dark:bg-blue-900 dark:text-blue-200">
                                        {record.record_type}
                                      </span>
                                    </td>
                                    <td className="px-4 py-3 text-sm text-gray-900 dark:text-gray-100">{record.name}</td>
                                    <td className="px-4 py-3 text-sm text-gray-900 dark:text-gray-100 max-w-xs truncate">{record.content}</td>
                                    <td className="px-4 py-3 text-sm text-gray-500 dark:text-gray-400">{record.ttl}</td>
                                    <td className="px-4 py-3 text-sm text-gray-500 dark:text-gray-400">{record.priority ?? '-'}</td>
                                    <td className="px-4 py-3 text-right">
                                      <button
                                        onClick={(e) => {
                                          e.stopPropagation();
                                          setEditingRecord(record);
                                        }}
                                        className="text-blue-600 hover:text-blue-900 dark:text-blue-400 dark:hover:text-blue-300 mr-3"
                                      >
                                        <FaEdit />
                                      </button>
                                      <button
                                        onClick={(e) => {
                                          e.stopPropagation();
                                          handleDeleteDNS(record.id);
                                        }}
                                        className="text-red-600 hover:text-red-900 dark:text-red-400 dark:hover:text-red-300"
                                      >
                                        <FaTrash />
                                      </button>
                                    </td>
                                  </tr>
                                ))}
                              </tbody>
                            </table>
                          </div>
                        )}

                        {/* Domain Details */}
                        <div className="mt-6 grid grid-cols-1 md:grid-cols-2 gap-4 text-sm">
                          <div className="bg-gray-50 dark:bg-gray-900 rounded-lg p-4">
                            <p className="text-gray-500 dark:text-gray-400 mb-1">Registration Date</p>
                            <p className="font-medium text-gray-900 dark:text-white">
                              {new Date(domain.registration_date).toLocaleDateString()}
                            </p>
                          </div>
                          <div className="bg-gray-50 dark:bg-gray-900 rounded-lg p-4">
                            <p className="text-gray-500 dark:text-gray-400 mb-1">Expiry Date</p>
                            <p className="font-medium text-gray-900 dark:text-white">
                              {new Date(domain.expiry_date).toLocaleDateString()}
                            </p>
                          </div>
                          {domain.nameserver1 && (
                            <div className="bg-gray-50 dark:bg-gray-900 rounded-lg p-4 md:col-span-2">
                              <p className="text-gray-500 dark:text-gray-400 mb-1">Nameservers</p>
                              <p className="font-medium text-gray-900 dark:text-white">
                                {domain.nameserver1}
                                {domain.nameserver2 && `, ${domain.nameserver2}`}
                              </p>
                            </div>
                          )}
                        </div>
                      </div>
                    </div>
                  )}
                </div>
              );
            })
          )}
        </div>

        {/* Back to Dashboard */}
        <div className="mt-8">
          <Link
            href="/dashboard"
            className="text-blue-600 hover:text-blue-700 dark:text-blue-400 dark:hover:text-blue-300"
          >
            ← Back to Dashboard
          </Link>
        </div>
      </div>

      {/* Add DNS Record Modal */}
      {showAddDNS && (
        <DNSRecordModal
          title={`Add DNS Record for ${showAddDNS}`}
          domainName={showAddDNS}
          onClose={() => setShowAddDNS(null)}
          onSubmit={(data) => handleCreateDNS(showAddDNS, data)}
        />
      )}

      {/* Edit DNS Record Modal */}
      {editingRecord && (
        <DNSRecordModal
          title="Edit DNS Record"
          domainName={editingRecord.domain}
          initialData={editingRecord}
          onClose={() => setEditingRecord(null)}
          onSubmit={(data) => handleUpdateDNS(editingRecord.id, data)}
        />
      )}
    </div>
  );
}

interface DNSRecordModalProps {
  title: string;
  domainName: string;
  initialData?: Partial<DNSRecord>;
  onClose: () => void;
  onSubmit: (data: Record<string, unknown>) => void;
}

function DNSRecordModal({ title, domainName, initialData, onClose, onSubmit }: DNSRecordModalProps) {
  const [recordType, setRecordType] = useState(initialData?.record_type || 'A');
  const [name, setName] = useState(initialData?.name || '');
  const [content, setContent] = useState(initialData?.content || '');
  const [ttl, setTtl] = useState(String(initialData?.ttl ?? 3600));
  const [priority, setPriority] = useState(String(initialData?.priority ?? ''));

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    const data: Record<string, unknown> = {
      record_type: recordType,
      name,
      content,
      ttl: parseInt(ttl, 10),
      domain: domainName,
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
              value={domainName}
              disabled
              className="w-full px-3 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-gray-100 dark:bg-gray-700 text-gray-500 dark:text-gray-400"
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
