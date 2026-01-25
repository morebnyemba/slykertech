'use client';

/**
 * Domain Availability Check Page
 * Dedicated page for checking domain availability using WHOIS
 */

import DomainSearch from '@/components/domain-search';

export default function DomainCheckPage() {
  return (
    <div className="min-h-screen py-12 bg-gray-50 dark:bg-gray-900">
      <DomainSearch />
    </div>
  );
}
