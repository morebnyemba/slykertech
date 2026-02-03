'use client';

import Link from 'next/link';
import { useState, useRef, useEffect } from 'react';
import { useAuthStore } from '@/lib/stores/auth-store';
import { FaChevronDown, FaUserCircle, FaUserTie, FaHandshake, FaBriefcase, FaUserShield } from 'react-icons/fa';

/**
 * Portal Switcher Component
 * Allows users to navigate between different portals based on their roles
 */
export default function PortalSwitcher() {
  const { isAuthenticated, isStaff } = useAuthStore();
  const [isOpen, setIsOpen] = useState(false);
  const dropdownRef = useRef<HTMLDivElement>(null);

  // Close dropdown when clicking outside
  useEffect(() => {
    function handleClickOutside(event: MouseEvent) {
      if (dropdownRef.current && !dropdownRef.current.contains(event.target as Node)) {
        setIsOpen(false);
      }
    }
    document.addEventListener('mousedown', handleClickOutside);
    return () => document.removeEventListener('mousedown', handleClickOutside);
  }, []);

  if (!isAuthenticated) {
    return null;
  }

  const portals = [
    {
      name: 'Client Dashboard',
      href: '/dashboard',
      icon: FaUserCircle,
      description: 'Manage services & invoices',
      available: true,
    },
    {
      name: 'Reseller Portal',
      href: '/portal/reseller',
      icon: FaUserTie,
      description: 'Reseller management',
      available: true,
    },
    {
      name: 'Partner Portal',
      href: '/portal/partner',
      icon: FaHandshake,
      description: 'Partnership programs',
      available: true,
    },
    {
      name: 'Jobs Portal',
      href: '/portal/jobs',
      icon: FaBriefcase,
      description: 'Career opportunities',
      available: true,
    },
    {
      name: 'Admin Portal',
      href: '/admin',
      icon: FaUserShield,
      description: 'System administration',
      available: isStaff,
    },
  ];

  const availablePortals = portals.filter(portal => portal.available);

  return (
    <div className="relative" ref={dropdownRef}>
      <button
        onClick={() => setIsOpen(!isOpen)}
        className="flex items-center gap-2 px-3 py-2 text-gray-800 dark:text-gray-200 hover:text-blue-700 dark:hover:text-blue-400 transition-colors rounded-lg hover:bg-gray-100 dark:hover:bg-gray-800"
      >
        <span className="font-medium">Portals</span>
        <FaChevronDown className={`w-3 h-3 transition-transform ${isOpen ? 'rotate-180' : ''}`} />
      </button>

      {isOpen && (
        <div className="absolute top-full right-0 mt-2 w-72 bg-white dark:bg-gray-800 rounded-lg shadow-xl border border-gray-200 dark:border-gray-700 py-2 z-50">
          <div className="px-4 py-2 border-b border-gray-200 dark:border-gray-700">
            <p className="text-xs font-semibold text-gray-500 dark:text-gray-400 uppercase">
              Switch Portal
            </p>
          </div>
          {availablePortals.map((portal, idx) => (
            <div key={portal.name}>
              <Link
                href={portal.href}
                onClick={() => setIsOpen(false)}
                className="flex items-center gap-3 px-4 py-3 hover:bg-blue-50 dark:hover:bg-gray-700 transition-colors"
              >
                <div className="w-10 h-10 bg-blue-100 dark:bg-blue-900/30 rounded-lg flex items-center justify-center flex-shrink-0">
                  <portal.icon className="w-5 h-5 text-blue-600 dark:text-blue-400" />
                </div>
                <div className="flex-1 min-w-0">
                  <div className="font-medium text-gray-900 dark:text-gray-100">
                    {portal.name}
                  </div>
                  <div className="text-sm text-gray-600 dark:text-gray-400">
                    {portal.description}
                  </div>
                </div>
              </Link>
              {idx < availablePortals.length - 1 && (
                <div className="border-t border-gray-100 dark:border-gray-700 mx-4" />
              )}
            </div>
          ))}
        </div>
      )}
    </div>
  );
}
