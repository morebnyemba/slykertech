'use client';

import { FaCode, FaMobile, FaDesktop, FaArrowRight } from 'react-icons/fa';
import Link from 'next/link';

export default function DevelopmentPage() {
  const developmentCategories = [
    {
      name: 'Web Development',
      slug: 'web',
      icon: FaCode,
      description: 'Custom web applications and responsive websites built with modern frameworks',
      features: [
        'Responsive design',
        'Modern frameworks (React, Next.js, Vue)',
        'RESTful API development',
        'Database design',
        'SEO optimization',
      ],
      startingPrice: '$2,000',
      color: 'from-blue-500 to-blue-600',
    },
    {
      name: 'Mobile Development',
      slug: 'mobile',
      icon: FaMobile,
      description: 'Native and cross-platform mobile applications for iOS and Android',
      features: [
        'iOS & Android support',
        'React Native / Flutter',
        'Push notifications',
        'Offline functionality',
        'App store deployment',
      ],
      startingPrice: '$3,000',
      color: 'from-purple-500 to-purple-600',
    },
    {
      name: 'Desktop Development',
      slug: 'desktop',
      icon: FaDesktop,
      description: 'Cross-platform desktop applications for Windows, macOS, and Linux',
      features: [
        'Windows, macOS, Linux',
        'Electron or native',
        'Database integration',
        'Auto updates',
        'System tray integration',
      ],
      startingPrice: '$2,500',
      color: 'from-green-500 to-green-600',
    },
  ];

  return (
    <div className="min-h-screen py-12">
      <div className="max-w-7xl mx-auto px-4">
        {/* Hero Section */}
        <div className="text-center mb-16">
          <h1 className="text-4xl md:text-5xl font-bold text-gray-900 dark:text-white mb-4">
            Development Services
          </h1>
          <p className="text-xl text-gray-600 dark:text-gray-300 max-w-3xl mx-auto">
            Professional software development for web, mobile, and desktop platforms
          </p>
        </div>

        {/* Development Categories */}
        <div className="space-y-8 mb-16">
          {developmentCategories.map((category) => {
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
                          Starting from <span className="text-blue-600 font-bold text-lg">{category.startingPrice}</span>
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
                      href={`/services/development/${category.slug}`}
                      className="inline-flex items-center gap-2 bg-blue-600 text-white px-8 py-3 rounded-lg font-bold hover:bg-blue-700 transition-colors"
                    >
                      View {category.name} Packages
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
          <h2 className="text-3xl font-bold mb-8 text-center">Why Choose Slyker Tech Development?</h2>
          <div className="grid md:grid-cols-3 gap-8">
            <div className="text-center">
              <div className="text-5xl font-bold mb-2">10+</div>
              <div className="text-blue-100 text-lg">Years Experience</div>
              <p className="text-blue-200 text-sm mt-2">
                Proven track record in delivering quality software
              </p>
            </div>
            <div className="text-center">
              <div className="text-5xl font-bold mb-2">100+</div>
              <div className="text-blue-100 text-lg">Projects Delivered</div>
              <p className="text-blue-200 text-sm mt-2">
                Successfully completed projects across platforms
              </p>
            </div>
            <div className="text-center">
              <div className="text-5xl font-bold mb-2">98%</div>
              <div className="text-blue-100 text-lg">Client Satisfaction</div>
              <p className="text-blue-200 text-sm mt-2">
                High satisfaction rate and repeat business
              </p>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
