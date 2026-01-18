import { Metadata } from 'next';
import EnhancedClientDashboard from '@/components/dashboard/EnhancedClientDashboard';
import { generatePageMetadata } from '@/lib/seo-config';

export async function generateMetadata(): Promise<Metadata> {
  return generatePageMetadata({
    title: 'Client Dashboard',
    description: 'Manage your services, projects, and invoices with real-time updates',
    url: '/dashboard',
  });
}

export default function ClientDashboardPage() {
  return <EnhancedClientDashboard />;
}

