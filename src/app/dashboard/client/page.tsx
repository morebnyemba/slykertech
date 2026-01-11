'use client';

import { useState, useEffect } from 'react';
import Link from 'next/link';
import { 
  FaServer, FaFileInvoiceDollar, FaProjectDiagram, 
  FaBell, FaChartLine, FaCog 
} from 'react-icons/fa';

interface DashboardStats {
  activeServices: number;
  pendingInvoices: number;
  activeProjects: number;
  unreadNotifications: number;
}

interface Service {
  id: number;
  name: string;
  status: string;
  nextBillingDate: string;
}

interface Invoice {
  id: number;
  invoiceNumber: string;
  amount: number;
  dueDate: string;
  status: string;
}

interface Project {
  id: number;
  title: string;
  progress: number;
  status: string;
}

export default function ClientDashboard() {
  const [stats, setStats] = useState<DashboardStats>({
    activeServices: 0,
    pendingInvoices: 0,
    activeProjects: 0,
    unreadNotifications: 0,
  });

  const [services, setServices] = useState<Service[]>([]);
  const [invoices, setInvoices] = useState<Invoice[]>([]);
  const [projects, setProjects] = useState<Project[]>([]);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    // Fetch dashboard data from backend API
    fetchDashboardData();
  }, []);

  const fetchDashboardData = async () => {
    try {
      // TODO: Replace with actual API calls when backend is running
      // const token = localStorage.getItem('access_token');
      // const headers = { Authorization: `Bearer ${token}` };
      
      // Mock data for demonstration
      setStats({
        activeServices: 5,
        pendingInvoices: 2,
        activeProjects: 3,
        unreadNotifications: 7,
      });

      setServices([
        { id: 1, name: 'Web Hosting', status: 'active', nextBillingDate: '2024-02-15' },
        { id: 2, name: 'Domain Registration', status: 'active', nextBillingDate: '2024-03-01' },
        { id: 3, name: 'SEO Services', status: 'active', nextBillingDate: '2024-02-20' },
      ]);

      setInvoices([
        { id: 1, invoiceNumber: 'INV-000123', amount: 150.00, dueDate: '2024-02-10', status: 'pending' },
        { id: 2, invoiceNumber: 'INV-000124', amount: 299.99, dueDate: '2024-02-15', status: 'pending' },
      ]);

      setProjects([
        { id: 1, title: 'Website Redesign', progress: 65, status: 'in_progress' },
        { id: 2, title: 'Mobile App Development', progress: 30, status: 'in_progress' },
        { id: 3, title: 'SEO Campaign', progress: 85, status: 'in_progress' },
      ]);

      setLoading(false);
    } catch (error) {
      console.error('Error fetching dashboard data:', error);
      setLoading(false);
    }
  };

  if (loading) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <div className="text-xl">Loading dashboard...</div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-gray-50 dark:bg-gray-900 py-8">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
        {/* Header */}
        <div className="mb-8">
          <h1 className="text-3xl font-bold text-gray-900 dark:text-white">Client Dashboard</h1>
          <p className="text-gray-600 dark:text-gray-400 mt-2">Manage your services, projects, and billing</p>
        </div>

        {/* Stats Grid */}
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-8">
          <StatCard
            icon={<FaServer className="w-6 h-6" />}
            title="Active Services"
            value={stats.activeServices}
            link="/dashboard/client/services"
            color="blue"
          />
          <StatCard
            icon={<FaFileInvoiceDollar className="w-6 h-6" />}
            title="Pending Invoices"
            value={stats.pendingInvoices}
            link="/dashboard/client/invoices"
            color="yellow"
          />
          <StatCard
            icon={<FaProjectDiagram className="w-6 h-6" />}
            title="Active Projects"
            value={stats.activeProjects}
            link="/dashboard/client/projects"
            color="green"
          />
          <StatCard
            icon={<FaBell className="w-6 h-6" />}
            title="Notifications"
            value={stats.unreadNotifications}
            link="/dashboard/client/notifications"
            color="red"
          />
        </div>

        {/* Main Content Grid */}
        <div className="grid grid-cols-1 lg:grid-cols-2 gap-8">
          {/* Services Section */}
          <div className="bg-white dark:bg-gray-800 rounded-lg shadow-lg p-6">
            <div className="flex justify-between items-center mb-4">
              <h2 className="text-xl font-bold text-gray-900 dark:text-white">My Services</h2>
              <Link 
                href="/dashboard/client/services" 
                className="text-blue-600 hover:text-blue-800 text-sm"
              >
                View All
              </Link>
            </div>
            <div className="space-y-3">
              {services.slice(0, 3).map((service) => (
                <div key={service.id} className="flex justify-between items-center p-3 bg-gray-50 dark:bg-gray-700 rounded">
                  <div>
                    <div className="font-medium text-gray-900 dark:text-white">{service.name}</div>
                    <div className="text-sm text-gray-600 dark:text-gray-400">
                      Next billing: {service.nextBillingDate}
                    </div>
                  </div>
                  <span className="px-3 py-1 bg-green-100 text-green-800 text-sm rounded-full">
                    {service.status}
                  </span>
                </div>
              ))}
            </div>
          </div>

          {/* Invoices Section */}
          <div className="bg-white dark:bg-gray-800 rounded-lg shadow-lg p-6">
            <div className="flex justify-between items-center mb-4">
              <h2 className="text-xl font-bold text-gray-900 dark:text-white">Recent Invoices</h2>
              <Link 
                href="/dashboard/client/invoices" 
                className="text-blue-600 hover:text-blue-800 text-sm"
              >
                View All
              </Link>
            </div>
            <div className="space-y-3">
              {invoices.map((invoice) => (
                <div key={invoice.id} className="flex justify-between items-center p-3 bg-gray-50 dark:bg-gray-700 rounded">
                  <div>
                    <div className="font-medium text-gray-900 dark:text-white">{invoice.invoiceNumber}</div>
                    <div className="text-sm text-gray-600 dark:text-gray-400">
                      Due: {invoice.dueDate}
                    </div>
                  </div>
                  <div className="text-right">
                    <div className="font-bold text-gray-900 dark:text-white">${invoice.amount}</div>
                    <span className="px-3 py-1 bg-yellow-100 text-yellow-800 text-xs rounded-full">
                      {invoice.status}
                    </span>
                  </div>
                </div>
              ))}
            </div>
          </div>

          {/* Projects Section */}
          <div className="bg-white dark:bg-gray-800 rounded-lg shadow-lg p-6 lg:col-span-2">
            <div className="flex justify-between items-center mb-4">
              <h2 className="text-xl font-bold text-gray-900 dark:text-white">Active Projects</h2>
              <Link 
                href="/dashboard/client/projects" 
                className="text-blue-600 hover:text-blue-800 text-sm"
              >
                View All
              </Link>
            </div>
            <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
              {projects.map((project) => (
                <div key={project.id} className="p-4 bg-gray-50 dark:bg-gray-700 rounded">
                  <div className="font-medium text-gray-900 dark:text-white mb-2">{project.title}</div>
                  <div className="mb-2">
                    <div className="flex justify-between text-sm mb-1">
                      <span className="text-gray-600 dark:text-gray-400">Progress</span>
                      <span className="text-gray-900 dark:text-white font-medium">{project.progress}%</span>
                    </div>
                    <div className="w-full bg-gray-200 rounded-full h-2">
                      <div 
                        className="bg-blue-600 h-2 rounded-full" 
                        style={{ width: `${project.progress}%` }}
                      ></div>
                    </div>
                  </div>
                  <span className="px-3 py-1 bg-blue-100 text-blue-800 text-xs rounded-full">
                    {project.status.replace('_', ' ')}
                  </span>
                </div>
              ))}
            </div>
          </div>
        </div>

        {/* Quick Actions */}
        <div className="mt-8 bg-white dark:bg-gray-800 rounded-lg shadow-lg p-6">
          <h2 className="text-xl font-bold text-gray-900 dark:text-white mb-4">Quick Actions</h2>
          <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
            <ActionButton href="/services" icon={<FaChartLine />} label="Browse Services" />
            <ActionButton href="/dashboard/client/invoices" icon={<FaFileInvoiceDollar />} label="Pay Invoice" />
            <ActionButton href="/dashboard/client/projects" icon={<FaProjectDiagram />} label="View Projects" />
            <ActionButton href="/dashboard/client/settings" icon={<FaCog />} label="Settings" />
          </div>
        </div>
      </div>
    </div>
  );
}

function StatCard({ icon, title, value, link, color }: {
  icon: React.ReactNode;
  title: string;
  value: number;
  link: string;
  color: string;
}) {
  const colorClasses = {
    blue: 'bg-blue-100 text-blue-600 dark:bg-blue-900 dark:text-blue-300',
    yellow: 'bg-yellow-100 text-yellow-600 dark:bg-yellow-900 dark:text-yellow-300',
    green: 'bg-green-100 text-green-600 dark:bg-green-900 dark:text-green-300',
    red: 'bg-red-100 text-red-600 dark:bg-red-900 dark:text-red-300',
  };

  return (
    <Link href={link}>
      <div className="bg-white dark:bg-gray-800 rounded-lg shadow-lg p-6 hover:shadow-xl transition-shadow cursor-pointer">
        <div className="flex items-center justify-between">
          <div>
            <p className="text-sm text-gray-600 dark:text-gray-400 mb-1">{title}</p>
            <p className="text-3xl font-bold text-gray-900 dark:text-white">{value}</p>
          </div>
          <div className={`p-3 rounded-full ${colorClasses[color as keyof typeof colorClasses]}`}>
            {icon}
          </div>
        </div>
      </div>
    </Link>
  );
}

function ActionButton({ href, icon, label }: {
  href: string;
  icon: React.ReactNode;
  label: string;
}) {
  return (
    <Link href={href}>
      <div className="flex flex-col items-center justify-center p-4 bg-gray-50 dark:bg-gray-700 rounded-lg hover:bg-gray-100 dark:hover:bg-gray-600 transition-colors cursor-pointer">
        <div className="text-blue-600 dark:text-blue-400 mb-2">{icon}</div>
        <span className="text-sm text-gray-900 dark:text-white text-center">{label}</span>
      </div>
    </Link>
  );
}
