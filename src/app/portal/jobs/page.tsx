'use client';

import React, { useState, useEffect } from 'react';
import { useRouter } from 'next/navigation';
import Link from 'next/link';
import { 
  FaBriefcase, FaFileAlt, FaCheckCircle, FaClock, 
  FaTimesCircle, FaSignOutAlt, FaUser, FaArrowRight 
} from 'react-icons/fa';
import { MdDashboard } from 'react-icons/md';

interface JobApplication {
  id: number;
  job: number;
  job_title: string;
  resume_url: string;
  cover_letter: string;
  applied_date: string;
  status: string;
}

interface UserProfile {
  id: number;
  email: string;
  first_name: string;
  last_name: string;
  full_name: string;
  phone: string;
}

export default function JobPortalPage() {
  const router = useRouter();
  const [applications, setApplications] = useState<JobApplication[]>([]);
  const [profile, setProfile] = useState<UserProfile | null>(null);
  const [loading, setLoading] = useState(true);
  const [activeTab, setActiveTab] = useState('dashboard');
  const [isAuthenticated, setIsAuthenticated] = useState(false);

  useEffect(() => {
    checkAuthAndFetch();
  // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  const checkAuthAndFetch = async () => {
    const token = localStorage.getItem('access_token');
    if (!token) {
      router.push('/login?redirect=/portal/jobs');
      return;
    }
    setIsAuthenticated(true);
    await fetchData();
  };

  const fetchData = async () => {
    try {
      const API_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:8000/api';
      const token = localStorage.getItem('access_token');

      const [applicationsRes, profileRes] = await Promise.all([
        fetch(`${API_URL}/jobs/applications/my_applications/`, {
          headers: { 'Authorization': `Bearer ${token}` }
        }),
        fetch(`${API_URL}/accounts/users/me/`, {
          headers: { 'Authorization': `Bearer ${token}` }
        })
      ]);

      if (applicationsRes.ok) {
        const data = await applicationsRes.json();
        setApplications(data);
      }

      if (profileRes.ok) {
        const data = await profileRes.json();
        setProfile(data);
      }
    } catch (error) {
      console.error('Failed to fetch data:', error);
    } finally {
      setLoading(false);
    }
  };

  const handleLogout = () => {
    localStorage.removeItem('access_token');
    localStorage.removeItem('refresh_token');
    router.push('/login');
  };

  const getStatusIcon = (status: string) => {
    switch (status) {
      case 'accepted':
        return <FaCheckCircle className="text-green-500" />;
      case 'rejected':
        return <FaTimesCircle className="text-red-500" />;
      case 'reviewing':
      case 'interviewed':
        return <FaClock className="text-blue-500" />;
      default:
        return <FaClock className="text-yellow-500" />;
    }
  };

  const getStatusColor = (status: string) => {
    switch (status) {
      case 'accepted':
        return 'bg-green-100 text-green-800 dark:bg-green-900 dark:text-green-200';
      case 'rejected':
        return 'bg-red-100 text-red-800 dark:bg-red-900 dark:text-red-200';
      case 'reviewing':
        return 'bg-blue-100 text-blue-800 dark:bg-blue-900 dark:text-blue-200';
      case 'interviewed':
        return 'bg-purple-100 text-purple-800 dark:bg-purple-900 dark:text-purple-200';
      default:
        return 'bg-yellow-100 text-yellow-800 dark:bg-yellow-900 dark:text-yellow-200';
    }
  };

  if (!isAuthenticated || loading) {
    return (
      <div className="min-h-screen flex items-center justify-center bg-gray-50 dark:bg-gray-900">
        <div className="animate-pulse text-gray-600 dark:text-gray-400">Loading job portal...</div>
      </div>
    );
  }

  const pendingCount = applications.filter(a => a.status === 'pending').length;
  const reviewingCount = applications.filter(a => ['reviewing', 'interviewed'].includes(a.status)).length;
  const acceptedCount = applications.filter(a => a.status === 'accepted').length;

  return (
    <div className="min-h-screen bg-gray-50 dark:bg-gray-900">
      {/* Sidebar */}
      <aside className="fixed left-0 top-0 h-full w-64 bg-white dark:bg-gray-800 shadow-lg z-40">
        <div className="p-6 border-b border-gray-200 dark:border-gray-700">
          <h2 className="text-xl font-bold text-blue-900 dark:text-blue-300">Job Portal</h2>
          <p className="text-sm text-gray-600 dark:text-gray-400 mt-1">
            {profile?.full_name || profile?.email}
          </p>
        </div>

        <nav className="p-4">
          <ul className="space-y-2">
            <li>
              <button
                onClick={() => setActiveTab('dashboard')}
                className={`flex items-center gap-3 px-4 py-3 w-full rounded-lg ${
                  activeTab === 'dashboard' 
                    ? 'bg-blue-50 dark:bg-blue-900/30 text-blue-900 dark:text-blue-300'
                    : 'text-gray-700 dark:text-gray-300 hover:bg-gray-100 dark:hover:bg-gray-700'
                }`}
              >
                <MdDashboard />
                <span>Dashboard</span>
              </button>
            </li>
            <li>
              <button
                onClick={() => setActiveTab('applications')}
                className={`flex items-center gap-3 px-4 py-3 w-full rounded-lg ${
                  activeTab === 'applications' 
                    ? 'bg-blue-50 dark:bg-blue-900/30 text-blue-900 dark:text-blue-300'
                    : 'text-gray-700 dark:text-gray-300 hover:bg-gray-100 dark:hover:bg-gray-700'
                }`}
              >
                <FaFileAlt />
                <span>My Applications</span>
                {applications.length > 0 && (
                  <span className="ml-auto bg-blue-100 dark:bg-blue-900 text-blue-800 dark:text-blue-200 px-2 py-0.5 rounded-full text-xs">
                    {applications.length}
                  </span>
                )}
              </button>
            </li>
            <li>
              <Link
                href="/careers"
                className="flex items-center gap-3 px-4 py-3 w-full rounded-lg text-gray-700 dark:text-gray-300 hover:bg-gray-100 dark:hover:bg-gray-700"
              >
                <FaBriefcase />
                <span>Browse Jobs</span>
              </Link>
            </li>
            <li>
              <button
                onClick={() => setActiveTab('profile')}
                className={`flex items-center gap-3 px-4 py-3 w-full rounded-lg ${
                  activeTab === 'profile' 
                    ? 'bg-blue-50 dark:bg-blue-900/30 text-blue-900 dark:text-blue-300'
                    : 'text-gray-700 dark:text-gray-300 hover:bg-gray-100 dark:hover:bg-gray-700'
                }`}
              >
                <FaUser />
                <span>Profile</span>
              </button>
            </li>
          </ul>
        </nav>

        <div className="absolute bottom-0 left-0 right-0 p-4 border-t border-gray-200 dark:border-gray-700">
          <button
            onClick={handleLogout}
            className="flex items-center gap-3 px-4 py-3 w-full rounded-lg text-red-600 dark:text-red-400 hover:bg-red-50 dark:hover:bg-red-900/20"
          >
            <FaSignOutAlt />
            <span>Logout</span>
          </button>
        </div>
      </aside>

      {/* Main Content */}
      <main className="ml-64 p-8">
        {activeTab === 'dashboard' && (
          <>
            <h1 className="text-3xl font-bold text-blue-900 dark:text-blue-300 mb-8">
              Welcome, {profile?.first_name || 'Applicant'}!
            </h1>

            {/* Stats */}
            <div className="grid grid-cols-1 md:grid-cols-3 gap-6 mb-8">
              <div className="bg-white dark:bg-gray-800 rounded-xl shadow-sm p-6">
                <div className="flex items-center justify-between">
                  <div>
                    <p className="text-sm text-gray-600 dark:text-gray-400">Pending</p>
                    <p className="text-3xl font-bold text-yellow-600 dark:text-yellow-400 mt-1">
                      {pendingCount}
                    </p>
                  </div>
                  <div className="p-3 bg-yellow-50 dark:bg-yellow-900/30 rounded-full">
                    <FaClock className="w-6 h-6 text-yellow-600 dark:text-yellow-400" />
                  </div>
                </div>
              </div>

              <div className="bg-white dark:bg-gray-800 rounded-xl shadow-sm p-6">
                <div className="flex items-center justify-between">
                  <div>
                    <p className="text-sm text-gray-600 dark:text-gray-400">In Review</p>
                    <p className="text-3xl font-bold text-blue-600 dark:text-blue-400 mt-1">
                      {reviewingCount}
                    </p>
                  </div>
                  <div className="p-3 bg-blue-50 dark:bg-blue-900/30 rounded-full">
                    <FaFileAlt className="w-6 h-6 text-blue-600 dark:text-blue-400" />
                  </div>
                </div>
              </div>

              <div className="bg-white dark:bg-gray-800 rounded-xl shadow-sm p-6">
                <div className="flex items-center justify-between">
                  <div>
                    <p className="text-sm text-gray-600 dark:text-gray-400">Accepted</p>
                    <p className="text-3xl font-bold text-green-600 dark:text-green-400 mt-1">
                      {acceptedCount}
                    </p>
                  </div>
                  <div className="p-3 bg-green-50 dark:bg-green-900/30 rounded-full">
                    <FaCheckCircle className="w-6 h-6 text-green-600 dark:text-green-400" />
                  </div>
                </div>
              </div>
            </div>

            {/* Recent Applications */}
            <div className="bg-white dark:bg-gray-800 rounded-xl shadow-sm p-6 mb-8">
              <div className="flex justify-between items-center mb-4">
                <h2 className="text-xl font-semibold text-blue-900 dark:text-blue-300">
                  Recent Applications
                </h2>
                <button 
                  onClick={() => setActiveTab('applications')}
                  className="text-blue-600 dark:text-blue-400 hover:underline text-sm"
                >
                  View All →
                </button>
              </div>

              {applications.length === 0 ? (
                <div className="text-center py-8">
                  <FaBriefcase className="w-12 h-12 mx-auto mb-4 text-gray-400" />
                  <p className="text-gray-600 dark:text-gray-400 mb-4">No applications yet</p>
                  <Link
                    href="/careers"
                    className="inline-flex items-center gap-2 px-6 py-3 bg-blue-600 text-white rounded-lg hover:bg-blue-700"
                  >
                    Browse Open Positions <FaArrowRight />
                  </Link>
                </div>
              ) : (
                <div className="space-y-4">
                  {applications.slice(0, 3).map((app) => (
                    <div key={app.id} className="flex items-center justify-between py-4 border-b border-gray-100 dark:border-gray-700 last:border-0">
                      <div className="flex items-center gap-4">
                        {getStatusIcon(app.status)}
                        <div>
                          <h3 className="font-semibold text-gray-900 dark:text-white">
                            {app.job_title}
                          </h3>
                          <p className="text-sm text-gray-500 dark:text-gray-400">
                            Applied on {new Date(app.applied_date).toLocaleDateString()}
                          </p>
                        </div>
                      </div>
                      <span className={`px-3 py-1 rounded-full text-sm font-medium ${getStatusColor(app.status)}`}>
                        {app.status.charAt(0).toUpperCase() + app.status.slice(1)}
                      </span>
                    </div>
                  ))}
                </div>
              )}
            </div>

            {/* Quick Actions */}
            <div className="bg-white dark:bg-gray-800 rounded-xl shadow-sm p-6">
              <h2 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-4">
                Quick Actions
              </h2>
              <div className="grid grid-cols-1 sm:grid-cols-2 gap-4">
                <Link
                  href="/careers"
                  className="flex items-center gap-3 p-4 bg-blue-50 dark:bg-blue-900/30 rounded-lg hover:bg-blue-100 dark:hover:bg-blue-900/50 transition-colors"
                >
                  <FaBriefcase className="text-blue-600 dark:text-blue-400" />
                  <span className="font-medium text-blue-900 dark:text-blue-300">
                    Browse Open Positions
                  </span>
                </Link>
                <button
                  onClick={() => setActiveTab('profile')}
                  className="flex items-center gap-3 p-4 bg-purple-50 dark:bg-purple-900/30 rounded-lg hover:bg-purple-100 dark:hover:bg-purple-900/50 transition-colors"
                >
                  <FaUser className="text-purple-600 dark:text-purple-400" />
                  <span className="font-medium text-purple-900 dark:text-purple-300">
                    Update Your Profile
                  </span>
                </button>
              </div>
            </div>
          </>
        )}

        {activeTab === 'applications' && (
          <>
            <h1 className="text-3xl font-bold text-blue-900 dark:text-blue-300 mb-8">
              My Applications
            </h1>

            {applications.length === 0 ? (
              <div className="bg-white dark:bg-gray-800 rounded-xl shadow-sm p-12 text-center">
                <FaFileAlt className="w-16 h-16 mx-auto mb-4 text-gray-400" />
                <h3 className="text-xl font-semibold text-gray-700 dark:text-gray-300 mb-2">
                  No applications yet
                </h3>
                <p className="text-gray-500 dark:text-gray-400 mb-6">
                  Start applying for positions that match your skills
                </p>
                <Link
                  href="/careers"
                  className="inline-flex items-center gap-2 px-6 py-3 bg-blue-600 text-white rounded-lg hover:bg-blue-700"
                >
                  Browse Open Positions <FaArrowRight />
                </Link>
              </div>
            ) : (
              <div className="space-y-4">
                {applications.map((app) => (
                  <div key={app.id} className="bg-white dark:bg-gray-800 rounded-xl shadow-sm p-6">
                    <div className="flex flex-col md:flex-row md:items-center md:justify-between gap-4">
                      <div>
                        <div className="flex items-center gap-3 mb-2">
                          {getStatusIcon(app.status)}
                          <h3 className="text-xl font-semibold text-gray-900 dark:text-white">
                            {app.job_title}
                          </h3>
                        </div>
                        <p className="text-sm text-gray-500 dark:text-gray-400">
                          Applied on {new Date(app.applied_date).toLocaleDateString()}
                        </p>
                      </div>
                      <div className="flex items-center gap-4">
                        <span className={`px-4 py-2 rounded-full text-sm font-medium ${getStatusColor(app.status)}`}>
                          {app.status.charAt(0).toUpperCase() + app.status.slice(1)}
                        </span>
                        <a
                          href={app.resume_url}
                          target="_blank"
                          rel="noopener noreferrer"
                          className="text-blue-600 dark:text-blue-400 hover:underline"
                        >
                          View Resume
                        </a>
                      </div>
                    </div>
                    {app.cover_letter && (
                      <div className="mt-4 pt-4 border-t border-gray-100 dark:border-gray-700">
                        <p className="text-sm text-gray-600 dark:text-gray-400 line-clamp-2">
                          {app.cover_letter}
                        </p>
                      </div>
                    )}
                  </div>
                ))}
              </div>
            )}
          </>
        )}

        {activeTab === 'profile' && (
          <>
            <h1 className="text-3xl font-bold text-blue-900 dark:text-blue-300 mb-8">
              Your Profile
            </h1>

            <div className="bg-white dark:bg-gray-800 rounded-xl shadow-sm p-6">
              <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                <div>
                  <label className="block text-sm text-gray-600 dark:text-gray-400 mb-1">
                    First Name
                  </label>
                  <p className="text-lg font-medium text-gray-900 dark:text-white">
                    {profile?.first_name || '-'}
                  </p>
                </div>
                <div>
                  <label className="block text-sm text-gray-600 dark:text-gray-400 mb-1">
                    Last Name
                  </label>
                  <p className="text-lg font-medium text-gray-900 dark:text-white">
                    {profile?.last_name || '-'}
                  </p>
                </div>
                <div>
                  <label className="block text-sm text-gray-600 dark:text-gray-400 mb-1">
                    Email
                  </label>
                  <p className="text-lg font-medium text-gray-900 dark:text-white">
                    {profile?.email || '-'}
                  </p>
                </div>
                <div>
                  <label className="block text-sm text-gray-600 dark:text-gray-400 mb-1">
                    Phone
                  </label>
                  <p className="text-lg font-medium text-gray-900 dark:text-white">
                    {profile?.phone || '-'}
                  </p>
                </div>
              </div>
              <div className="mt-6 pt-6 border-t border-gray-200 dark:border-gray-700">
                <Link
                  href="/dashboard/profile"
                  className="px-6 py-3 bg-blue-600 text-white rounded-lg hover:bg-blue-700 inline-block"
                >
                  Edit Profile
                </Link>
              </div>
            </div>
          </>
        )}
      </main>
    </div>
  );
}
