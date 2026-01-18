'use client';

import { FaServer, FaCloud, FaShieldAlt, FaClock, FaArrowRight } from 'react-icons/fa';
import Link from 'next/link';

export default function HostingPage() {
  const hostingCategories = [
    {
      name: 'Shared Hosting',
      slug: 'shared',
      icon: FaServer,
      description: 'Affordable hosting perfect for personal websites and small businesses',
      features: [
        'Easy to use cPanel',
        'Free SSL certificates',
        'Email accounts included',
        'One-click app installs',
        '99.9% uptime guarantee',
      ],
      startingPrice: '$10',
      color: 'from-blue-500 to-blue-600',
    },
    {
      name: 'VPS Hosting',
      slug: 'vps',
      icon: FaCloud,
      description: 'Scalable virtual servers with dedicated resources',
      features: [
        'Dedicated resources',
        'Root/SSH access',
        'Choose your OS',
        'Easy scaling',
        'SSD storage',
      ],
      startingPrice: '$30',
      color: 'from-purple-500 to-purple-600',
    },
    {
      name: 'Dedicated Servers',
      slug: 'dedicated',
      icon: FaShieldAlt,
      description: 'Maximum performance with enterprise-grade hardware',
      features: [
        'Bare metal performance',
        'Full server control',
        'Enterprise security',
        '100% dedicated resources',
        '99.99% uptime SLA',
      ],
      startingPrice: '$100',
      color: 'from-green-500 to-green-600',
    },
  ];

  return (
    <div className="min-h-screen py-12">
      <div className="max-w-7xl mx-auto px-4">
        {/* Hero Section */}
        <div className="text-center mb-16">
          <h1 className="text-4xl md:text-5xl font-bold text-gray-900 dark:text-white mb-4">
            Web Hosting Services
          </h1>
          <p className="text-xl text-gray-600 dark:text-gray-300 max-w-3xl mx-auto">
            Choose the perfect hosting solution for your needs - from shared hosting to dedicated servers
          </p>
        </div>

        {/* Features Grid */}
        <div className="grid md:grid-cols-4 gap-6 mb-16">
          <div className="bg-white dark:bg-gray-800 p-6 rounded-lg shadow-lg text-center">
            <FaServer className="w-12 h-12 text-blue-600 mx-auto mb-4" />
            <h3 className="font-bold text-gray-900 dark:text-white mb-2">Latest Hardware</h3>
            <p className="text-sm text-gray-600 dark:text-gray-400">Enterprise-grade servers</p>
          </div>
          <div className="bg-white dark:bg-gray-800 p-6 rounded-lg shadow-lg text-center">
            <FaShieldAlt className="w-12 h-12 text-blue-600 mx-auto mb-4" />
            <h3 className="font-bold text-gray-900 dark:text-white mb-2">DDoS Protection</h3>
            <p className="text-sm text-gray-600 dark:text-gray-400">Advanced security measures</p>
          </div>
          <div className="bg-white dark:bg-gray-800 p-6 rounded-lg shadow-lg text-center">
            <FaClock className="w-12 h-12 text-blue-600 mx-auto mb-4" />
            <h3 className="font-bold text-gray-900 dark:text-white mb-2">99.9% Uptime</h3>
            <p className="text-sm text-gray-600 dark:text-gray-400">Guaranteed availability</p>
          </div>
          <div className="bg-white dark:bg-gray-800 p-6 rounded-lg shadow-lg text-center">
            <FaCloud className="w-12 h-12 text-blue-600 mx-auto mb-4" />
            <h3 className="font-bold text-gray-900 dark:text-white mb-2">Daily Backups</h3>
            <p className="text-sm text-gray-600 dark:text-gray-400">Your data is safe</p>
          </div>
        </div>

        {/* Hosting Categories */}
        <div className="space-y-8 mb-16">
          {hostingCategories.map((category) => {
            const Icon = category.icon;
            return (
              <div
                key={category.slug}
                className="bg-white dark:bg-gray-800 rounded-2xl shadow-xl overflow-hidden border border-gray-200 dark:border-gray-700 hover:shadow-2xl transition-shadow"
              >
                <div className="md:flex">
                  {/* Left side - Category Info */}
                  <div className="md:w-1/2 p-8">
                    <div className="flex items-center gap-4 mb-4">
                      <div className={`bg-gradient-to-r ${category.color} p-4 rounded-lg`}>
                        <Icon className="w-8 h-8 text-white" />
                      </div>
                      <div>
                        <h2 className="text-3xl font-bold text-gray-900 dark:text-white">
                          {category.name}
                        </h2>
                        <p className="text-sm text-gray-600 dark:text-gray-400">
                          Starting from <span className="text-blue-600 font-bold text-lg">{category.startingPrice}/mo</span>
                        </p>
                      </div>
                    </div>
                    
                    <p className="text-gray-600 dark:text-gray-300 mb-6">
                      {category.description}
                    </p>

                    <ul className="space-y-3 mb-6">
                      {category.features.map((feature, index) => (
                        <li key={index} className="flex items-center gap-2 text-gray-700 dark:text-gray-300">
                          <div className="w-2 h-2 bg-blue-600 rounded-full"></div>
                          <span>{feature}</span>
                        </li>
                      ))}
                    </ul>

                    <Link
                      href={`/services/hosting/${category.slug}`}
                      className="inline-flex items-center gap-2 bg-blue-600 text-white px-8 py-3 rounded-lg font-bold hover:bg-blue-700 transition-colors"
                    >
                      View {category.name} Plans
                      <FaArrowRight />
                    </Link>
                  </div>

                  {/* Right side - Visual */}
                  <div className={`md:w-1/2 bg-gradient-to-br ${category.color} p-12 flex items-center justify-center`}>
                    <Icon className="w-48 h-48 text-white opacity-20" />
                  </div>
                </div>
              </div>
            );
          })}
        </div>

        {/* Why Choose Us Section */}
        <div className="bg-gradient-to-r from-blue-600 to-blue-700 rounded-2xl p-12 text-white">
          <h2 className="text-3xl font-bold mb-8 text-center">Why Choose Slyker Tech Web Services Hosting?</h2>
          <div className="grid md:grid-cols-3 gap-8">
            <div className="text-center">
              <div className="text-5xl font-bold mb-2">24/7</div>
              <div className="text-blue-100 text-lg">Expert Support</div>
              <p className="text-blue-200 text-sm mt-2">
                Our team is always available to help you succeed
              </p>
            </div>
            <div className="text-center">
              <div className="text-5xl font-bold mb-2">99.9%</div>
              <div className="text-blue-100 text-lg">Uptime Guarantee</div>
              <p className="text-blue-200 text-sm mt-2">
                Your website stays online when you need it most
              </p>
            </div>
            <div className="text-center">
              <div className="text-5xl font-bold mb-2">100+</div>
              <div className="text-blue-100 text-lg">Happy Clients</div>
              <p className="text-blue-200 text-sm mt-2">
                Trusted by businesses across Zimbabwe and beyond
              </p>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
