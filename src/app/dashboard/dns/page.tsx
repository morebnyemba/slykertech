import { Metadata } from 'next';
import DNSPanel from '@/components/dns/DNSPanel';

export const metadata: Metadata = {
  title: 'DNS Management - Slyker Tech Web Services',
  description: 'Manage your DNS records with real-time updates',
};

export default function DNSManagementPage() {
  return <DNSPanel />;
}
