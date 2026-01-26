'use client';

import { useState, useEffect } from 'react';
import { useRouter } from 'next/navigation';
import Link from 'next/link';
import { useAuthStore } from '@/lib/stores/auth-store';
import { FaLock, FaEnvelope, FaSpinner, FaEye, FaEyeSlash, FaShieldAlt, FaDesktop, FaGlobe } from 'react-icons/fa';

interface DeviceInfo {
  ip: string;
  browser: string;
  os: string;
  device: string;
}

export default function AdminLoginPage() {
  const router = useRouter();
  const { login, isLoading, isStaff, isAuthenticated } = useAuthStore();
  const [email, setEmail] = useState('');
  const [password, setPassword] = useState('');
  const [error, setError] = useState('');
  const [showPassword, setShowPassword] = useState(false);
  const [deviceInfo, setDeviceInfo] = useState<DeviceInfo>({
    ip: 'Detecting...',
    browser: 'Unknown',
    os: 'Unknown',
    device: 'Unknown'
  });

  useEffect(() => {
    // Detect browser and OS from user agent
    const detectDevice = () => {
      const ua = navigator.userAgent;
      
      // Detect browser
      let browser = 'Unknown';
      if (ua.includes('Firefox')) browser = 'Firefox';
      else if (ua.includes('Edg')) browser = 'Microsoft Edge';
      else if (ua.includes('Chrome')) browser = 'Chrome';
      else if (ua.includes('Safari')) browser = 'Safari';
      else if (ua.includes('Opera') || ua.includes('OPR')) browser = 'Opera';
      
      // Detect OS
      let os = 'Unknown';
      if (ua.includes('Windows NT 10')) os = 'Windows 10/11';
      else if (ua.includes('Windows')) os = 'Windows';
      else if (ua.includes('Mac OS X')) os = 'macOS';
      else if (ua.includes('Linux')) os = 'Linux';
      else if (ua.includes('Android')) os = 'Android';
      else if (ua.includes('iOS') || ua.includes('iPhone') || ua.includes('iPad')) os = 'iOS';
      
      // Detect device type
      let device = 'Desktop';
      if (/Mobi|Android/i.test(ua)) device = 'Mobile';
      else if (/Tablet|iPad/i.test(ua)) device = 'Tablet';
      
      setDeviceInfo(prev => ({ ...prev, browser, os, device }));
    };

    // Fetch IP address
    const fetchIP = async () => {
      try {
        const response = await fetch('https://api.ipify.org?format=json');
        const data = await response.json();
        setDeviceInfo(prev => ({ ...prev, ip: data.ip }));
      } catch {
        setDeviceInfo(prev => ({ ...prev, ip: 'Unable to detect' }));
      }
    };

    detectDevice();
    fetchIP();
  }, []);

  // If already authenticated as staff, redirect to admin
  if (isAuthenticated && isStaff) {
    router.push('/admin');
    return null;
  }

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setError('');

    if (!email || !password) {
      setError('Email and password are required');
      return;
    }

    const result = await login(email, password);

    if (result.success) {
      // Check if user is staff after login
      const authState = useAuthStore.getState();
      if (authState.isStaff) {
        router.push('/admin');
      } else {
        setError('Access denied. Staff credentials required.');
        useAuthStore.getState().logout();
      }
    } else {
      setError(result.error || 'Login failed');
    }
  };

  return (
    <div className="min-h-screen flex items-center justify-center bg-gradient-to-br from-gray-900 via-blue-900 to-gray-900 px-4 py-12">
      <div className="max-w-md w-full">
        {/* Logo */}
        <div className="text-center mb-8">
          <div className="inline-flex items-center justify-center w-16 h-16 bg-gradient-to-br from-blue-500 to-blue-700 rounded-xl mb-4">
            <FaShieldAlt className="h-8 w-8 text-white" />
          </div>
          <h1 className="text-3xl font-bold text-white">Admin Portal</h1>
          <p className="text-gray-400 mt-2">Slyker Tech Web Services</p>
        </div>

        {/* Card */}
        <div className="bg-gray-800 rounded-2xl shadow-2xl p-8 border border-gray-700">
          <div className="text-center mb-8">
            <h2 className="text-xl font-semibold text-white">Staff Login</h2>
            <p className="text-gray-400 text-sm mt-1">
              Access restricted to authorized personnel
            </p>
          </div>

          {/* Error Message */}
          {error && (
            <div className="mb-6 p-4 bg-red-900/50 border border-red-700 rounded-lg">
              <p className="text-red-200 text-sm">{error}</p>
            </div>
          )}

          {/* Form */}
          <form onSubmit={handleSubmit} className="space-y-6">
            <div>
              <label htmlFor="email" className="block text-sm font-medium text-gray-300 mb-2">
                Email Address
              </label>
              <div className="relative">
                <div className="absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none">
                  <FaEnvelope className="text-gray-500" />
                </div>
                <input
                  id="email"
                  type="email"
                  value={email}
                  onChange={(e) => setEmail(e.target.value)}
                  className="block w-full pl-10 pr-3 py-3 border border-gray-600 rounded-lg bg-gray-700 text-white placeholder-gray-400 focus:ring-2 focus:ring-blue-500 focus:border-transparent"
                  placeholder="admin@slykertech.co.zw"
                />
              </div>
            </div>

            <div>
              <label htmlFor="password" className="block text-sm font-medium text-gray-300 mb-2">
                Password
              </label>
              <div className="relative">
                <div className="absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none">
                  <FaLock className="text-gray-500" />
                </div>
                <input
                  id="password"
                  type={showPassword ? 'text' : 'password'}
                  value={password}
                  onChange={(e) => setPassword(e.target.value)}
                  className="block w-full pl-10 pr-10 py-3 border border-gray-600 rounded-lg bg-gray-700 text-white placeholder-gray-400 focus:ring-2 focus:ring-blue-500 focus:border-transparent"
                  placeholder="••••••••"
                />
                <button
                  type="button"
                  onClick={() => setShowPassword(!showPassword)}
                  className="absolute inset-y-0 right-0 pr-3 flex items-center"
                >
                  {showPassword ? (
                    <FaEyeSlash className="text-gray-400 hover:text-gray-300" />
                  ) : (
                    <FaEye className="text-gray-400 hover:text-gray-300" />
                  )}
                </button>
              </div>
            </div>

            <button
              type="submit"
              disabled={isLoading}
              className="w-full flex items-center justify-center py-3 px-4 border border-transparent rounded-lg shadow-sm text-white bg-gradient-to-r from-blue-600 to-blue-700 hover:from-blue-700 hover:to-blue-800 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500 disabled:opacity-50 disabled:cursor-not-allowed transition-all duration-200"
            >
              {isLoading ? (
                <>
                  <FaSpinner className="animate-spin mr-2" />
                  Authenticating...
                </>
              ) : (
                <>
                  <FaLock className="mr-2" />
                  Sign In to Admin
                </>
              )}
            </button>
          </form>

          {/* Security Monitoring Notice */}
          <div className="mt-6 pt-6 border-t border-gray-700">
            <div className="bg-red-900/30 border border-red-700/50 rounded-lg p-4 mb-4">
              <div className="flex items-center gap-2 mb-2">
                <FaShieldAlt className="text-red-500" />
                <span className="text-red-400 font-semibold text-sm">⚠️ WE ARE WATCHING YOU</span>
              </div>
              <p className="text-xs text-red-300 mb-3">
                This access is being monitored and recorded. Unauthorized access attempts will be prosecuted.
              </p>
              <div className="space-y-1.5 text-xs">
                <div className="flex items-center gap-2 text-gray-400">
                  <FaGlobe className="text-gray-500 w-3 h-3" />
                  <span>Your IP:</span>
                  <span className="text-white font-mono bg-gray-700/50 px-2 py-0.5 rounded">{deviceInfo.ip}</span>
                </div>
                <div className="flex items-center gap-2 text-gray-400">
                  <FaDesktop className="text-gray-500 w-3 h-3" />
                  <span>Device:</span>
                  <span className="text-white">{deviceInfo.device} • {deviceInfo.os} • {deviceInfo.browser}</span>
                </div>
              </div>
            </div>
            <p className="text-xs text-gray-500 text-center">
              All login attempts are logged with IP, timestamp, and device information.
            </p>
          </div>
        </div>

        {/* Back to Site */}
        <div className="mt-6 text-center">
          <Link href="/" className="text-sm text-gray-400 hover:text-white">
            ← Back to main site
          </Link>
        </div>
      </div>
    </div>
  );
}
