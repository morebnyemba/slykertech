import { Metadata } from 'next';
import EnhancedClientDashboard from '@/components/dashboard/EnhancedClientDashboard';

export const metadata: Metadata = {
  title: 'Client Dashboard - Slyker Tech',
  description: 'Manage your services, projects, and invoices with real-time updates',
};

export default function ClientDashboardPage() {
  return <EnhancedClientDashboard />;
}

