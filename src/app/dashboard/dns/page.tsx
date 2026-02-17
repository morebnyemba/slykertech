import { Metadata } from 'next';
import DNSPanel from '@/components/dns/DNSPanel';
import Link from 'next/link';

export const metadata: Metadata = {
  title: 'DNS Management - Slyker Tech Web Services',
  description: 'Manage your DNS records with real-time updates',
};

export default function DNSManagementPage() {
  return (
    <div>
      <div className="max-w-7xl mx-auto px-4 pt-24 pb-4">
        <div className="bg-blue-50 dark:bg-blue-900/20 border border-blue-200 dark:border-blue-800 rounded-lg p-4 flex items-center justify-between">
          <p className="text-sm text-blue-800 dark:text-blue-200">
            💡 <strong>Tip:</strong> You can manage DNS records directly from your{' '}
            <Link href="/dashboard/domains" className="underline hover:text-blue-600 dark:hover:text-blue-300">
              My Domains
            </Link>{' '}
            page for a better experience.
          </p>
        </div>
      </div>
      <DNSPanel />
    </div>
  );
}
