import Link from 'next/link';
import { FaHome, FaTools, FaExclamationTriangle } from 'react-icons/fa';
import type { Metadata } from 'next';

export const metadata: Metadata = {
  title: '404 - Page Not Found | Slyker Tech',
  description: 'The page you are looking for could not be found. Visit our homepage or contact support.',
  openGraph: {
    title: '404 - Page Not Found',
    description: 'The page you are looking for could not be found.',
    type: 'website',
    url: 'https://slykertech.co.zw/404',
    images: [
      {
        url: 'https://slykertech.co.zw/images/stws.png',
        width: 490,
        height: 112,
        alt: 'Slyker Tech Logo',
      },
    ],
  },
  twitter: {
    card: 'summary',
    title: '404 - Page Not Found',
    description: 'The page you are looking for could not be found.',
    images: ['https://slykertech.co.zw/images/stws.png'],
  },
};

export default function NotFound() {
  return (
    <div className="min-h-[70vh] flex items-center justify-center px-4">
      <div className="max-w-2xl w-full text-center space-y-8">
        {/* Icon */}
        <div className="flex justify-center">
          <div className="relative">
            <FaExclamationTriangle className="text-yellow-500 dark:text-yellow-400 text-8xl animate-pulse" />
            <div className="absolute -bottom-2 -right-2">
              <FaTools className="text-blue-600 dark:text-blue-400 text-4xl" />
            </div>
          </div>
        </div>

        {/* Error Code */}
        <div>
          <h1 className="text-7xl md:text-9xl font-bold text-gray-900 dark:text-white mb-2">
            404
          </h1>
          <p className="text-2xl md:text-3xl font-semibold text-gray-700 dark:text-gray-300">
            Page Not Found
          </p>
        </div>

        {/* Maintenance Notice */}
        <div className="bg-yellow-50 dark:bg-yellow-900/20 border-2 border-yellow-400 dark:border-yellow-600 rounded-lg p-6">
          <div className="flex items-center justify-center gap-3 mb-3">
            <FaTools className="text-yellow-600 dark:text-yellow-400 text-2xl" />
            <h2 className="text-xl font-bold text-yellow-900 dark:text-yellow-300">
              Site Under Maintenance
            </h2>
          </div>
          <p className="text-yellow-800 dark:text-yellow-200 leading-relaxed">
            We&apos;re currently performing maintenance and updates to improve your experience.
            Some pages and features may be temporarily unavailable. We apologize for any inconvenience.
          </p>
        </div>

        {/* Message */}
        <div className="space-y-4">
          <p className="text-lg text-gray-600 dark:text-gray-400">
            The page you&apos;re looking for might have been removed, had its name changed, 
            or is temporarily unavailable due to maintenance.
          </p>
        </div>

        {/* Action Buttons */}
        <div className="flex flex-col sm:flex-row gap-4 justify-center items-center pt-4">
          <Link
            href="/"
            className="flex items-center gap-2 bg-blue-600 hover:bg-blue-700 text-white font-medium py-3 px-8 rounded-lg transition-all duration-300 transform hover:-translate-y-0.5 hover:shadow-lg"
          >
            <FaHome />
            Go to Homepage
          </Link>
          <Link
            href="/contact"
            className="flex items-center gap-2 bg-gray-200 dark:bg-gray-700 hover:bg-gray-300 dark:hover:bg-gray-600 text-gray-800 dark:text-gray-200 font-medium py-3 px-8 rounded-lg transition-all duration-300 transform hover:-translate-y-0.5"
          >
            Contact Support
          </Link>
        </div>

        {/* Additional Help */}
        <div className="pt-8 text-sm text-gray-500 dark:text-gray-500">
          <p>If you believe this is an error, please contact our support team.</p>
        </div>
      </div>
    </div>
  );
}
