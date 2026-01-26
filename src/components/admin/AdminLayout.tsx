'use client';

import { useState, useEffect, useCallback } from 'react';
import Link from 'next/link';
import { usePathname } from 'next/navigation';
import AdminGuard from '@/components/admin/AdminGuard';
import { useAuthStore } from '@/lib/stores/auth-store';
import { apiService } from '@/lib/api-service';
import { 
  FaHome, FaExclamationTriangle, FaUsers, FaServer, FaCog, 
  FaBars, FaTimes, FaSignOutAlt, FaBell, FaTicketAlt, FaComments,
  FaChartBar, FaFileInvoiceDollar, FaMoneyBillWave, FaChevronLeft, FaChevronRight,
  FaTag
} from 'react-icons/fa';

interface AdminLayoutProps {
  children: React.ReactNode;
}

export default function AdminLayout({ children }: AdminLayoutProps) {
  const pathname = usePathname();
  const { user, logout } = useAuthStore();
  const [sidebarOpen, setSidebarOpen] = useState(false);
  const [sidebarCollapsed, setSidebarCollapsed] = useState(false);
  const [pendingFailures, setPendingFailures] = useState(0);
  const [openTickets, setOpenTickets] = useState(0);
  const [activeChats, setActiveChats] = useState(0);
  const [unpaidInvoices, setUnpaidInvoices] = useState(0);

  // Load sidebar state from localStorage
  useEffect(() => {
    const saved = localStorage.getItem('adminSidebarCollapsed');
    if (saved === 'true') {
      setSidebarCollapsed(true);
    }
  }, []);

  // Save sidebar state to localStorage
  const toggleSidebarCollapse = () => {
    const newState = !sidebarCollapsed;
    setSidebarCollapsed(newState);
    localStorage.setItem('adminSidebarCollapsed', String(newState));
  };

  const fetchCounts = useCallback(async () => {
    try {
      const [failuresRes, ticketStatsRes, chatStatsRes, invoiceStatsRes] = await Promise.all([
        apiService.getPendingFailuresCount(),
        apiService.getTicketStats().catch(() => ({ data: null })),
        apiService.getChatStats().catch(() => ({ data: null })),
        apiService.getInvoiceStats().catch(() => ({ data: null })),
      ]);
      
      if (failuresRes.data) {
        setPendingFailures((failuresRes.data as { pending_count: number }).pending_count || 0);
      }
      if (ticketStatsRes.data) {
        const stats = ticketStatsRes.data as { open?: number; in_progress?: number };
        setOpenTickets((stats.open || 0) + (stats.in_progress || 0));
      }
      if (chatStatsRes.data) {
        setActiveChats((chatStatsRes.data as { active_sessions?: number }).active_sessions || 0);
      }
      if (invoiceStatsRes.data) {
        const stats = invoiceStatsRes.data as { by_status?: { sent?: number; overdue?: number } };
        setUnpaidInvoices((stats.by_status?.sent || 0) + (stats.by_status?.overdue || 0));
      }
    } catch (error) {
      console.error('Failed to fetch counts:', error);
    }
  }, []);

  useEffect(() => {
    fetchCounts();
    // Refresh every 30 seconds
    const interval = setInterval(fetchCounts, 30000);
    return () => clearInterval(interval);
  }, [fetchCounts]);

  const navigation = [
    { name: 'Dashboard', href: '/admin', icon: FaHome },
    { 
      name: 'Tickets', 
      href: '/admin/tickets', 
      icon: FaTicketAlt,
      badge: openTickets > 0 ? openTickets : undefined
    },
    { 
      name: 'Live Chat', 
      href: '/admin/livechat', 
      icon: FaComments,
      badge: activeChats > 0 ? activeChats : undefined
    },
    { 
      name: 'Invoices', 
      href: '/admin/invoices', 
      icon: FaFileInvoiceDollar,
      badge: unpaidInvoices > 0 ? unpaidInvoices : undefined
    },
    { 
      name: 'Expenses', 
      href: '/admin/expenses', 
      icon: FaMoneyBillWave
    },
    { 
      name: 'Provisioning Failures', 
      href: '/admin/provisioning-failures', 
      icon: FaExclamationTriangle,
      badge: pendingFailures > 0 ? pendingFailures : undefined
    },
    { name: 'Subscriptions', href: '/admin/subscriptions', icon: FaServer },
    { name: 'Clients', href: '/admin/clients', icon: FaUsers },
    { name: 'Promotions', href: '/admin/promotions', icon: FaTag },
    { name: 'Analytics', href: '/admin/analytics', icon: FaChartBar },
    { name: 'Settings', href: '/admin/settings', icon: FaCog },
  ];

  const totalAlerts = pendingFailures + openTickets + activeChats + unpaidInvoices;

  const sidebarWidth = sidebarCollapsed ? 'w-20' : 'w-64';
  const mainMargin = sidebarCollapsed ? 'lg:ml-20' : 'lg:ml-64';

  return (
    <AdminGuard>
      <div className="min-h-screen bg-gray-100 dark:bg-gray-900">
        {/* Mobile sidebar toggle */}
        <div className="lg:hidden fixed top-4 left-4 z-50">
          <button
            onClick={() => setSidebarOpen(!sidebarOpen)}
            className="p-2.5 bg-white dark:bg-gray-800 rounded-lg shadow-lg hover:bg-gray-50 dark:hover:bg-gray-700 transition-colors"
            aria-label={sidebarOpen ? 'Close sidebar' : 'Open sidebar'}
          >
            {sidebarOpen ? <FaTimes className="h-5 w-5" /> : <FaBars className="h-5 w-5" />}
          </button>
        </div>

        {/* Sidebar */}
        <aside 
          className={`fixed inset-y-0 left-0 z-40 ${sidebarWidth} bg-white dark:bg-gray-800 shadow-lg transform transition-all duration-300 ease-in-out ${
            sidebarOpen ? 'translate-x-0' : '-translate-x-full'
          } lg:translate-x-0`}
        >
          <div className="flex flex-col h-full">
            {/* Logo */}
            <div className="p-4 border-b border-gray-200 dark:border-gray-700">
              <Link href="/admin" className="flex items-center gap-3">
                <div className="w-10 h-10 bg-gradient-to-br from-blue-500 to-blue-700 rounded-lg flex items-center justify-center flex-shrink-0">
                  <span className="text-white font-bold text-xl">S</span>
                </div>
                {!sidebarCollapsed && (
                  <div className="overflow-hidden">
                    <h1 className="font-bold text-gray-900 dark:text-white whitespace-nowrap">Slyker Admin</h1>
                    <p className="text-xs text-gray-500 dark:text-gray-400 whitespace-nowrap">Management Portal</p>
                  </div>
                )}
              </Link>
            </div>

            {/* Navigation */}
            <nav className="flex-1 p-2 space-y-1 overflow-y-auto">
              {navigation.map((item, index) => {
                const isActive = pathname === item.href || pathname?.startsWith(item.href + '/');
                const tooltipId = `nav-tooltip-${index}`;
                return (
                  <Link
                    key={item.name}
                    href={item.href}
                    onClick={() => setSidebarOpen(false)}
                    aria-describedby={sidebarCollapsed ? tooltipId : undefined}
                    className={`flex items-center gap-3 px-3 py-2.5 rounded-lg transition-colors relative group ${
                      isActive
                        ? 'bg-blue-50 dark:bg-blue-900/20 text-blue-600 dark:text-blue-400'
                        : 'text-gray-600 dark:text-gray-400 hover:bg-gray-100 dark:hover:bg-gray-700'
                    } ${sidebarCollapsed ? 'justify-center' : ''}`}
                  >
                    <item.icon className="h-5 w-5 flex-shrink-0" aria-hidden="true" />
                    {!sidebarCollapsed && (
                      <>
                        <span className="flex-1 truncate">{item.name}</span>
                        {item.badge && (
                          <span className="px-2 py-0.5 bg-red-500 text-white text-xs rounded-full" aria-label={`${item.badge} notifications`}>
                            {item.badge}
                          </span>
                        )}
                      </>
                    )}
                    {sidebarCollapsed && item.badge && (
                      <span className="absolute -top-1 -right-1 w-5 h-5 bg-red-500 text-white text-xs rounded-full flex items-center justify-center" aria-label={`${item.badge} notifications`}>
                        {item.badge > 9 ? '9+' : item.badge}
                      </span>
                    )}
                    {/* Tooltip for collapsed state */}
                    {sidebarCollapsed && (
                      <div 
                        id={tooltipId}
                        role="tooltip"
                        className="absolute left-full ml-2 px-2 py-1 bg-gray-900 text-white text-sm rounded opacity-0 group-hover:opacity-100 pointer-events-none whitespace-nowrap z-50 transition-opacity"
                      >
                        {item.name}
                        {item.badge && <span className="ml-1 text-red-300">({item.badge})</span>}
                      </div>
                    )}
                  </Link>
                );
              })}
            </nav>

            {/* Collapse toggle for desktop */}
            <div className="hidden lg:block p-2 border-t border-gray-200 dark:border-gray-700">
              <button
                onClick={toggleSidebarCollapse}
                className="w-full flex items-center justify-center gap-2 px-3 py-2 text-gray-500 hover:bg-gray-100 dark:hover:bg-gray-700 rounded-lg transition-colors"
                title={sidebarCollapsed ? 'Expand sidebar' : 'Collapse sidebar'}
              >
                {sidebarCollapsed ? (
                  <FaChevronRight className="h-4 w-4" />
                ) : (
                  <>
                    <FaChevronLeft className="h-4 w-4" />
                    <span className="text-sm">Collapse</span>
                  </>
                )}
              </button>
            </div>

            {/* User info & logout */}
            <div className="p-3 border-t border-gray-200 dark:border-gray-700">
              {!sidebarCollapsed ? (
                <>
                  <div className="flex items-center gap-3 mb-3">
                    <div className="w-10 h-10 bg-gray-200 dark:bg-gray-700 rounded-full flex items-center justify-center flex-shrink-0">
                      <span className="text-gray-600 dark:text-gray-300 font-medium">
                        {user?.first_name?.[0] || user?.email?.[0]?.toUpperCase() || 'A'}
                      </span>
                    </div>
                    <div className="flex-1 min-w-0">
                      <p className="font-medium text-gray-900 dark:text-white truncate text-sm">
                        {user?.first_name} {user?.last_name}
                      </p>
                      <p className="text-xs text-gray-500 dark:text-gray-400 truncate">
                        {user?.email}
                      </p>
                    </div>
                  </div>
                  <button
                    onClick={logout}
                    className="w-full flex items-center justify-center gap-2 px-4 py-2 text-red-600 dark:text-red-400 hover:bg-red-50 dark:hover:bg-red-900/20 rounded-lg transition-colors text-sm"
                  >
                    <FaSignOutAlt />
                    Sign Out
                  </button>
                </>
              ) : (
                <button
                  onClick={logout}
                  className="w-full flex items-center justify-center p-2 text-red-600 dark:text-red-400 hover:bg-red-50 dark:hover:bg-red-900/20 rounded-lg transition-colors"
                  title="Sign Out"
                >
                  <FaSignOutAlt className="h-5 w-5" />
                </button>
              )}
            </div>
          </div>
        </aside>

        {/* Overlay for mobile */}
        {sidebarOpen && (
          <div 
            className="fixed inset-0 bg-black/50 z-30 lg:hidden"
            onClick={() => setSidebarOpen(false)}
          />
        )}

        {/* Main content */}
        <div className={`${mainMargin} transition-all duration-300`}>
          {/* Top bar */}
          <header className="bg-white dark:bg-gray-800 shadow-sm sticky top-0 z-20">
            <div className="px-4 sm:px-6 lg:px-8 py-4 flex items-center justify-between">
              <h2 className="text-lg font-semibold text-gray-900 dark:text-white lg:hidden pl-12">
                Admin Portal
              </h2>
              <div className="flex items-center gap-4 ml-auto">
                <button className="relative p-2 text-gray-500 hover:text-gray-700 dark:hover:text-gray-300 hover:bg-gray-100 dark:hover:bg-gray-700 rounded-lg transition-colors">
                  <FaBell className="h-5 w-5" />
                  {totalAlerts > 0 && (
                    <span className="absolute -top-0.5 -right-0.5 w-5 h-5 bg-red-500 text-white text-xs rounded-full flex items-center justify-center">
                      {totalAlerts > 9 ? '9+' : totalAlerts}
                    </span>
                  )}
                </button>
              </div>
            </div>
          </header>

          {/* Page content */}
          <main className="p-4 sm:p-6 lg:p-8">
            {children}
          </main>
        </div>
      </div>
    </AdminGuard>
  );
}
