import { Metadata } from 'next';
import { redirect } from 'next/navigation';

export const metadata: Metadata = {
  title: 'Client Dashboard - Slyker Tech',
  description: 'Manage your services, projects, and invoices',
};

export default function DashboardPage() {
  // In production, check authentication here
  // For now, redirect to client dashboard
  redirect('/dashboard/client');
}
