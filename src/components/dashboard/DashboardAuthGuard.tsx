'use client';

import { useEffect } from 'react';
import { useRouter } from 'next/navigation';
import { useAuthStore } from '@/lib/stores/auth-store';
import { FaSpinner } from 'react-icons/fa';

interface DashboardAuthGuardProps {
  children: React.ReactNode;
}

/**
 * Auth guard for dashboard pages.
 * Waits for auth store hydration before making auth decisions,
 * preventing false "unauthorized" flashes on page load.
 */
export default function DashboardAuthGuard({ children }: DashboardAuthGuardProps) {
  const router = useRouter();
  const { isAuthenticated, hasHydrated } = useAuthStore();

  useEffect(() => {
    // Only redirect after hydration is complete to avoid false unauthorized states
    if (hasHydrated && !isAuthenticated) {
      router.push('/login');
    }
  }, [isAuthenticated, hasHydrated, router]);

  // Show loading while store is hydrating from localStorage
  if (!hasHydrated) {
    return (
      <div className="min-h-screen flex items-center justify-center bg-gray-50 dark:bg-gray-900">
        <div className="text-center">
          <FaSpinner className="animate-spin h-8 w-8 text-blue-600 mx-auto mb-4" />
          <p className="text-gray-600 dark:text-gray-400">Loading...</p>
        </div>
      </div>
    );
  }

  // After hydration, if not authenticated, show nothing (redirect is happening)
  if (!isAuthenticated) {
    return null;
  }

  return <>{children}</>;
}
