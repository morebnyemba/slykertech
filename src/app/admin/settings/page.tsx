'use client';

import AdminLayout from '@/components/admin/AdminLayout';
import { FaCog, FaDatabase, FaServer, FaGlobe, FaKey } from 'react-icons/fa';
import Link from 'next/link';

export default function SettingsPage() {
  const settingsCategories = [
    {
      title: 'API Configurations',
      description: 'Manage external API integrations (cPanel, DirectAdmin, Namecheap)',
      icon: FaKey,
      href: '/django-admin/integrations/apiconfiguration/',
      external: true,
    },
    {
      title: 'Integration Credentials',
      description: 'View and manage server credentials',
      icon: FaServer,
      href: '/django-admin/integrations/integrationcredential/',
      external: true,
    },
    {
      title: 'Services Configuration',
      description: 'Configure service offerings and pricing',
      icon: FaCog,
      href: '/django-admin/services/service/',
      external: true,
    },
    {
      title: 'Domain Products',
      description: 'Manage TLD pricing and availability',
      icon: FaGlobe,
      href: '/django-admin/services/domainproduct/',
      external: true,
    },
    {
      title: 'Database Admin',
      description: 'Access Django admin panel for full database management',
      icon: FaDatabase,
      href: '/django-admin/',
      external: true,
    },
  ];

  return (
    <AdminLayout>
      <div className="space-y-6">
        {/* Header */}
        <div>
          <h1 className="text-2xl font-bold text-gray-900 dark:text-white">
            Settings
          </h1>
          <p className="text-gray-500 dark:text-gray-400">
            Configure system settings and integrations
          </p>
        </div>

        {/* Settings Grid */}
        <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
          {settingsCategories.map((category) => (
            <Link
              key={category.title}
              href={category.href}
              target={category.external ? '_blank' : undefined}
              className="bg-white dark:bg-gray-800 rounded-xl shadow-lg p-6 hover:shadow-xl transition-shadow group"
            >
              <div className="flex items-start gap-4">
                <div className="p-3 bg-blue-100 dark:bg-blue-900/30 rounded-lg group-hover:bg-blue-200 dark:group-hover:bg-blue-900/50 transition-colors">
                  <category.icon className="h-6 w-6 text-blue-600 dark:text-blue-400" />
                </div>
                <div>
                  <h3 className="font-semibold text-gray-900 dark:text-white group-hover:text-blue-600 dark:group-hover:text-blue-400 transition-colors">
                    {category.title}
                    {category.external && (
                      <span className="ml-2 text-xs text-gray-400">↗</span>
                    )}
                  </h3>
                  <p className="text-sm text-gray-500 dark:text-gray-400 mt-1">
                    {category.description}
                  </p>
                </div>
              </div>
            </Link>
          ))}
        </div>

        {/* Info Box */}
        <div className="bg-blue-50 dark:bg-blue-900/20 border border-blue-200 dark:border-blue-800 rounded-xl p-6">
          <h3 className="font-semibold text-blue-800 dark:text-blue-200 mb-2">
            Django Admin Access
          </h3>
          <p className="text-blue-700 dark:text-blue-300 text-sm">
            For advanced configuration options, use the Django admin panel at{' '}
            <code className="bg-blue-100 dark:bg-blue-800 px-2 py-0.5 rounded">/django-admin/</code>.
            This provides full access to database models, user management, and system configuration.
          </p>
        </div>
      </div>
    </AdminLayout>
  );
}
