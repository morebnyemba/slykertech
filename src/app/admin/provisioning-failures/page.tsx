import AdminLayout from '@/components/admin/AdminLayout';
import ProvisioningFailures from '@/components/admin/ProvisioningFailures';
import { Metadata } from 'next';

export const metadata: Metadata = {
  title: 'Provisioning Failures - Admin | Slyker Tech',
  description: 'Manage and resolve service provisioning failures',
};

export default function ProvisioningFailuresPage() {
  return (
    <AdminLayout>
      <ProvisioningFailures />
    </AdminLayout>
  );
}
