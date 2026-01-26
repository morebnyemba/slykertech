'use client';

import { useState, useEffect, useCallback } from 'react';
import { apiService } from '@/lib/api-service';
import { 
  FaExclamationTriangle, FaCheck, FaRedo, FaTimes, FaClipboard, 
  FaServer, FaGlobe, FaUser, FaSpinner, FaCheckCircle, FaTimesCircle 
} from 'react-icons/fa';

interface ProvisioningData {
  client_info?: {
    company_name?: string;
    email?: string;
    id?: number;
  };
  service_info?: {
    name?: string;
    provisioning_type?: string;
  };
  suggested_values?: {
    username?: string;
    domain?: string;
    disk_quota?: number;
    bandwidth_quota?: number;
    plan?: string;
  };
  subscription_metadata?: Record<string, unknown>;
}

interface ProvisioningFailure {
  id: number;
  subscription: {
    id: number;
    client: {
      company_name: string;
      user?: { email: string };
    };
    service: {
      name: string;
      provisioning_type?: string;
    };
    status: string;
  };
  error_message: string;
  error_details: Record<string, unknown>;
  status: string;
  provisioning_data: ProvisioningData;
  admin_notes?: string;
  resolved_by?: { email: string };
  resolved_at?: string;
  admin_notified: boolean;
  created_at: string;
}

interface CopyFieldProps {
  label: string;
  value: string;
  icon?: React.ReactNode;
}

function CopyField({ label, value, icon }: CopyFieldProps) {
  const [copied, setCopied] = useState(false);

  const handleCopy = async () => {
    try {
      await navigator.clipboard.writeText(value);
      setCopied(true);
      setTimeout(() => setCopied(false), 2000);
    } catch (err) {
      console.error('Failed to copy:', err);
    }
  };

  return (
    <div className="flex items-center justify-between bg-gray-50 dark:bg-gray-700 rounded-lg p-3">
      <div className="flex items-center gap-2">
        {icon && <span className="text-gray-500">{icon}</span>}
        <div>
          <p className="text-xs text-gray-500 dark:text-gray-400">{label}</p>
          <p className="font-mono text-sm text-gray-900 dark:text-white">{value}</p>
        </div>
      </div>
      <button
        onClick={handleCopy}
        className="p-2 hover:bg-gray-200 dark:hover:bg-gray-600 rounded transition-colors"
        title="Copy to clipboard"
      >
        {copied ? (
          <FaCheckCircle className="text-green-500" />
        ) : (
          <FaClipboard className="text-gray-500" />
        )}
      </button>
    </div>
  );
}

interface FailureCardProps {
  failure: ProvisioningFailure;
  onAction: (action: string, id: number, data?: Record<string, unknown>) => Promise<void>;
}

function FailureCard({ failure, onAction }: FailureCardProps) {
  const [expanded, setExpanded] = useState(false);
  const [notes, setNotes] = useState('');
  const [manualData, setManualData] = useState({
    provisioned_username: '',
    provisioned_domain: '',
    provisioned_password: '',
  });
  const [loading, setLoading] = useState<string | null>(null);

  const handleAction = async (action: string, data?: Record<string, unknown>) => {
    setLoading(action);
    try {
      await onAction(action, failure.id, data);
    } finally {
      setLoading(null);
    }
  };

  const statusColors = {
    pending: 'bg-yellow-100 text-yellow-800 dark:bg-yellow-900/30 dark:text-yellow-400',
    in_progress: 'bg-blue-100 text-blue-800 dark:bg-blue-900/30 dark:text-blue-400',
    resolved: 'bg-green-100 text-green-800 dark:bg-green-900/30 dark:text-green-400',
    dismissed: 'bg-gray-100 text-gray-800 dark:bg-gray-700 dark:text-gray-400',
  };

  const provisioningData = failure.provisioning_data || {};
  const suggestedValues = provisioningData.suggested_values || {};

  return (
    <div className="bg-white dark:bg-gray-800 rounded-xl shadow-lg overflow-hidden">
      {/* Header */}
      <div className="p-4 border-b border-gray-200 dark:border-gray-700">
        <div className="flex items-center justify-between">
          <div className="flex items-center gap-3">
            <div className="p-2 bg-red-100 dark:bg-red-900/30 rounded-lg">
              <FaExclamationTriangle className="h-5 w-5 text-red-500" />
            </div>
            <div>
              <h3 className="font-semibold text-gray-900 dark:text-white">
                {failure.subscription?.client?.company_name || 'Unknown Client'}
              </h3>
              <p className="text-sm text-gray-500 dark:text-gray-400">
                {failure.subscription?.service?.name || 'Unknown Service'}
              </p>
            </div>
          </div>
          <span className={`px-3 py-1 rounded-full text-xs font-medium ${statusColors[failure.status as keyof typeof statusColors] || statusColors.pending}`}>
            {failure.status.replace('_', ' ').toUpperCase()}
          </span>
        </div>
      </div>

      {/* Error Message */}
      <div className="p-4 bg-red-50 dark:bg-red-900/10 border-b border-gray-200 dark:border-gray-700">
        <p className="text-sm text-red-700 dark:text-red-400 font-mono">
          {failure.error_message}
        </p>
        <p className="text-xs text-gray-500 mt-2">
          Failed at: {new Date(failure.created_at).toLocaleString()}
        </p>
      </div>

      {/* Expandable Details */}
      <button
        onClick={() => setExpanded(!expanded)}
        className="w-full p-4 text-left hover:bg-gray-50 dark:hover:bg-gray-700/50 transition-colors flex items-center justify-between"
      >
        <span className="text-sm font-medium text-blue-600 dark:text-blue-400">
          {expanded ? 'Hide Details' : 'Show Manual Provisioning Details'}
        </span>
        <svg 
          className={`w-4 h-4 transform transition-transform ${expanded ? 'rotate-180' : ''}`}
          fill="none" 
          viewBox="0 0 24 24" 
          stroke="currentColor"
        >
          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M19 9l-7 7-7-7" />
        </svg>
      </button>

      {expanded && (
        <div className="p-4 space-y-4 border-t border-gray-200 dark:border-gray-700">
          {/* Client Info */}
          {provisioningData.client_info && (
            <div className="space-y-2">
              <h4 className="text-sm font-semibold text-gray-700 dark:text-gray-300">Client Information</h4>
              <div className="grid grid-cols-1 md:grid-cols-2 gap-2">
                <CopyField 
                  label="Company Name" 
                  value={provisioningData.client_info.company_name || ''} 
                  icon={<FaUser className="h-3 w-3" />}
                />
                <CopyField 
                  label="Email" 
                  value={provisioningData.client_info.email || ''} 
                />
              </div>
            </div>
          )}

          {/* Suggested Values for Manual Provisioning */}
          {Object.keys(suggestedValues).length > 0 && (
            <div className="space-y-2">
              <h4 className="text-sm font-semibold text-gray-700 dark:text-gray-300">
                Suggested Values for Manual Setup
              </h4>
              <div className="grid grid-cols-1 md:grid-cols-2 gap-2">
                {suggestedValues.username && (
                  <CopyField 
                    label="Username" 
                    value={suggestedValues.username} 
                    icon={<FaUser className="h-3 w-3" />}
                  />
                )}
                {suggestedValues.domain && (
                  <CopyField 
                    label="Domain" 
                    value={suggestedValues.domain} 
                    icon={<FaGlobe className="h-3 w-3" />}
                  />
                )}
                {suggestedValues.disk_quota && (
                  <CopyField 
                    label="Disk Quota (MB)" 
                    value={String(suggestedValues.disk_quota)} 
                    icon={<FaServer className="h-3 w-3" />}
                  />
                )}
                {suggestedValues.bandwidth_quota && (
                  <CopyField 
                    label="Bandwidth (MB)" 
                    value={String(suggestedValues.bandwidth_quota)} 
                  />
                )}
                {suggestedValues.plan && (
                  <CopyField 
                    label="Plan" 
                    value={suggestedValues.plan} 
                  />
                )}
              </div>
            </div>
          )}

          {/* Manual Provisioning Form */}
          {failure.status === 'pending' || failure.status === 'in_progress' ? (
            <div className="space-y-3 pt-4 border-t border-gray-200 dark:border-gray-700">
              <h4 className="text-sm font-semibold text-gray-700 dark:text-gray-300">
                Complete Manual Provisioning
              </h4>
              <div className="grid grid-cols-1 md:grid-cols-3 gap-3">
                <input
                  type="text"
                  placeholder="Created Username"
                  value={manualData.provisioned_username}
                  onChange={(e) => setManualData(prev => ({ ...prev, provisioned_username: e.target.value }))}
                  className="px-3 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-white text-sm"
                />
                <input
                  type="text"
                  placeholder="Created Domain"
                  value={manualData.provisioned_domain}
                  onChange={(e) => setManualData(prev => ({ ...prev, provisioned_domain: e.target.value }))}
                  className="px-3 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-white text-sm"
                />
                <input
                  type="password"
                  placeholder="Created Password"
                  value={manualData.provisioned_password}
                  onChange={(e) => setManualData(prev => ({ ...prev, provisioned_password: e.target.value }))}
                  className="px-3 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-white text-sm"
                />
              </div>
              <textarea
                placeholder="Admin notes..."
                value={notes}
                onChange={(e) => setNotes(e.target.value)}
                rows={2}
                className="w-full px-3 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-700 text-gray-900 dark:text-white text-sm"
              />
            </div>
          ) : null}

          {/* Resolved Info */}
          {failure.resolved_by && (
            <div className="p-3 bg-green-50 dark:bg-green-900/20 rounded-lg">
              <p className="text-sm text-green-700 dark:text-green-400">
                Resolved by {failure.resolved_by.email} on {new Date(failure.resolved_at || '').toLocaleString()}
              </p>
              {failure.admin_notes && (
                <p className="text-sm text-gray-600 dark:text-gray-400 mt-1">
                  Notes: {failure.admin_notes}
                </p>
              )}
            </div>
          )}
        </div>
      )}

      {/* Action Buttons */}
      {(failure.status === 'pending' || failure.status === 'in_progress') && (
        <div className="p-4 bg-gray-50 dark:bg-gray-700/50 border-t border-gray-200 dark:border-gray-700 flex flex-wrap gap-2">
          <button
            onClick={() => handleAction('retry')}
            disabled={loading !== null}
            className="flex items-center gap-2 px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors disabled:opacity-50"
          >
            {loading === 'retry' ? <FaSpinner className="animate-spin" /> : <FaRedo />}
            Retry Automatic
          </button>
          <button
            onClick={() => handleAction('complete', { ...manualData, notes })}
            disabled={loading !== null}
            className="flex items-center gap-2 px-4 py-2 bg-green-600 text-white rounded-lg hover:bg-green-700 transition-colors disabled:opacity-50"
          >
            {loading === 'complete' ? <FaSpinner className="animate-spin" /> : <FaCheck />}
            Mark Manually Complete
          </button>
          <button
            onClick={() => handleAction('dismiss', { notes })}
            disabled={loading !== null}
            className="flex items-center gap-2 px-4 py-2 bg-gray-600 text-white rounded-lg hover:bg-gray-700 transition-colors disabled:opacity-50"
          >
            {loading === 'dismiss' ? <FaSpinner className="animate-spin" /> : <FaTimes />}
            Dismiss
          </button>
        </div>
      )}
    </div>
  );
}

export default function ProvisioningFailures() {
  const [failures, setFailures] = useState<ProvisioningFailure[]>([]);
  const [loading, setLoading] = useState(true);
  const [filter, setFilter] = useState<string>('pending');
  const [message, setMessage] = useState<{ type: 'success' | 'error'; text: string } | null>(null);

  const fetchFailures = useCallback(async () => {
    setLoading(true);
    try {
      const response = await apiService.getProvisioningFailures(filter);
      if (response.data) {
        setFailures(response.data as ProvisioningFailure[]);
      }
    } catch (error) {
      console.error('Failed to fetch failures:', error);
    } finally {
      setLoading(false);
    }
  }, [filter]);

  useEffect(() => {
    fetchFailures();
  }, [fetchFailures]);

  const handleAction = async (action: string, id: number, data?: Record<string, unknown>) => {
    try {
      let response;
      
      switch (action) {
        case 'retry':
          response = await apiService.retryProvisioning(id);
          break;
        case 'complete':
          response = await apiService.completeManualProvisioning(id, data as {
            notes?: string;
            provisioned_username?: string;
            provisioned_domain?: string;
            provisioned_password?: string;
          });
          break;
        case 'dismiss':
          response = await apiService.dismissFailure(id, (data as { notes?: string })?.notes);
          break;
        default:
          return;
      }

      if (response.error) {
        setMessage({ type: 'error', text: response.error });
      } else {
        setMessage({ type: 'success', text: 'Action completed successfully' });
        fetchFailures(); // Refresh the list
      }

      // Clear message after 5 seconds
      setTimeout(() => setMessage(null), 5000);
    } catch (error) {
      setMessage({ type: 'error', text: 'An error occurred' });
      console.error('Action failed:', error);
    }
  };

  return (
    <div className="space-y-6">
      {/* Header */}
      <div className="flex flex-col sm:flex-row sm:items-center sm:justify-between gap-4">
        <div>
          <h2 className="text-2xl font-bold text-gray-900 dark:text-white">
            Provisioning Failures
          </h2>
          <p className="text-gray-500 dark:text-gray-400">
            Review and resolve failed automatic provisioning attempts
          </p>
        </div>

        {/* Filter Buttons */}
        <div className="flex gap-2">
          {['pending', 'in_progress', 'resolved', 'dismissed', ''].map((status) => (
            <button
              key={status || 'all'}
              onClick={() => setFilter(status)}
              className={`px-3 py-1.5 rounded-lg text-sm font-medium transition-colors ${
                filter === status
                  ? 'bg-blue-600 text-white'
                  : 'bg-gray-100 dark:bg-gray-700 text-gray-600 dark:text-gray-300 hover:bg-gray-200 dark:hover:bg-gray-600'
              }`}
            >
              {status ? status.replace('_', ' ').charAt(0).toUpperCase() + status.slice(1).replace('_', ' ') : 'All'}
            </button>
          ))}
        </div>
      </div>

      {/* Status Message */}
      {message && (
        <div className={`p-4 rounded-lg flex items-center gap-3 ${
          message.type === 'success' 
            ? 'bg-green-50 dark:bg-green-900/20 text-green-700 dark:text-green-400' 
            : 'bg-red-50 dark:bg-red-900/20 text-red-700 dark:text-red-400'
        }`}>
          {message.type === 'success' ? <FaCheckCircle /> : <FaTimesCircle />}
          {message.text}
        </div>
      )}

      {/* Loading State */}
      {loading ? (
        <div className="flex items-center justify-center py-12">
          <FaSpinner className="animate-spin h-8 w-8 text-blue-600" />
        </div>
      ) : failures.length === 0 ? (
        <div className="text-center py-12 bg-white dark:bg-gray-800 rounded-xl">
          <FaCheckCircle className="h-12 w-12 text-green-500 mx-auto mb-4" />
          <h3 className="text-lg font-medium text-gray-900 dark:text-white mb-2">
            No Failures Found
          </h3>
          <p className="text-gray-500 dark:text-gray-400">
            {filter ? `No ${filter.replace('_', ' ')} failures to display.` : 'All provisioning attempts are successful.'}
          </p>
        </div>
      ) : (
        <div className="space-y-4">
          {failures.map((failure) => (
            <FailureCard
              key={failure.id}
              failure={failure}
              onAction={handleAction}
            />
          ))}
        </div>
      )}
    </div>
  );
}
