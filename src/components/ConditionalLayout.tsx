'use client';

import { usePathname } from 'next/navigation';
import Header from '@/components/Header';
import Footer from '@/components/Footer';
import LiveChatWidget from '@/components/LiveChatWidget';
import ClientBackgroundWrapper from '@/components/ClientBackgroundWrapper';

interface ConditionalLayoutProps {
  children: React.ReactNode;
}

export default function ConditionalLayout({ children }: ConditionalLayoutProps) {
  const pathname = usePathname();
  
  // Check if we're on an admin page (with null safety)
  const isAdminRoute = pathname !== null && pathname.startsWith('/admin');
  
  if (isAdminRoute) {
    // Admin layout - no header, footer, or main site decorations
    return (
      <div className="min-h-screen bg-gray-100 dark:bg-gray-900">
        {children}
      </div>
    );
  }
  
  // Regular site layout with header and footer
  return (
    <>
      {/* Client-side only background elements */}
      <ClientBackgroundWrapper />

      {/* Static background layers */}
      <div className="fixed inset-0 -z-50 overflow-hidden">
        <div className="background-grid-circuit" />
        <div className="background-overlay-gradient" />
        <div className="absolute inset-0 bg-[radial-gradient(ellipse_at_center,var(--tw-gradient-stops))] from-primary/5 via-transparent to-transparent dark:from-primary/10" />
      </div>

      {/* Main content container */}
      <div className="relative flex min-h-screen flex-col">
        <Header />
        {/* Spacer to prevent content from being cut off by fixed header */}
        <div className="h-32 sm:h-28 md:h-24 lg:h-24 flex-shrink-0" aria-hidden="true" />
        {/* Separator line between header and content */}
        <div className="w-full h-px bg-gradient-to-r from-transparent via-gray-300 dark:via-gray-600 to-transparent" aria-hidden="true" />
        <main className="flex-1">
          <div className="mx-auto w-full max-w-7xl px-3 sm:px-4 md:px-6 lg:px-8 py-4 sm:py-6 md:py-8">
            {children}
          </div>
        </main>
        <Footer />
      </div>

      {/* Live Chat Widget - Persistent across all pages */}
      <LiveChatWidget />
    </>
  );
}
