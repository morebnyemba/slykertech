'use client';

import { useState } from 'react';
import { FaCode, FaMobile, FaDesktop, FaLayerGroup, FaCheck } from 'react-icons/fa';
import { useCartStore } from '@/lib/stores/cart-store';
import { useAuthStore } from '@/lib/stores/auth-store';

const developmentServices = [
  {
    id: 1,
    type: 'web',
    name: 'Web Development',
    icon: FaCode,
    price: 2000,
    description: 'Custom web applications and websites',
    features: [
      'Responsive Design',
      'Modern Frameworks (React, Next.js, Vue)',
      'RESTful API Development',
      'Database Design',
      'Authentication & Security',
      'SEO Optimization',
      'Content Management System',
      'Maintenance & Support',
    ],
    popular: true,
  },
  {
    id: 2,
    type: 'mobile',
    name: 'Mobile Development',
    icon: FaMobile,
    price: 3000,
    description: 'Native and cross-platform mobile apps',
    features: [
      'iOS & Android Support',
      'React Native / Flutter',
      'Push Notifications',
      'Offline Functionality',
      'App Store Deployment',
      'Backend Integration',
      'Analytics Integration',
      'Ongoing Updates',
    ],
    popular: false,
  },
  {
    id: 3,
    type: 'desktop',
    name: 'Desktop Development',
    icon: FaDesktop,
    price: 2500,
    description: 'Cross-platform desktop applications',
    features: [
      'Windows, macOS, Linux',
      'Electron or Native',
      'Database Integration',
      'File System Access',
      'Auto Updates',
      'Offline Capabilities',
      'System Tray Integration',
      'Custom UI/UX',
    ],
    popular: false,
  },
  {
    id: 4,
    type: 'hybrid',
    name: 'Hybrid Solutions',
    icon: FaLayerGroup,
    price: 4500,
    description: 'Complete multi-platform solutions',
    features: [
      'Web + Mobile + Desktop',
      'Unified Codebase',
      'Consistent UX Across Platforms',
      'Cloud Backend',
      'Real-time Synchronization',
      'Comprehensive Testing',
      'Deployment Support',
      'Extended Support',
    ],
    popular: true,
  },
];

export default function DevelopmentPage() {
  const [selectedService, setSelectedService] = useState<any>(null);
  const [projectBrief, setProjectBrief] = useState('');
  const [projectName, setProjectName] = useState('');
  const [timeline, setTimeline] = useState('1-3 months');
  
  const { addItem } = useCartStore();
  const { token } = useAuthStore();

  const handleAddToCart = async (service: any) => {
    if (!projectName.trim()) {
      alert('Please enter a project name');
      return;
    }

    if (!projectBrief.trim()) {
      alert('Please provide a project brief');
      return;
    }

    const cartItem = {
      service: service.id,
      service_metadata: {
        type: service.type,
        project_name: projectName,
        brief: projectBrief,
        timeline: timeline,
      },
      quantity: 1,
      unit_price: service.price,
      billing_cycle: 'one_time',
    };

    const result = await addItem(cartItem, token || undefined);
    
    if (result.success) {
      alert('Development project added to cart successfully!');
      setSelectedService(null);
      setProjectName('');
      setProjectBrief('');
    } else {
      alert(`Failed to add to cart: ${result.error}`);
    }
  };

  return (
    <div className="min-h-screen py-12">
      <div className="max-w-7xl mx-auto px-4">
        {/* Hero Section */}
        <div className="text-center mb-16">
          <h1 className="text-4xl md:text-5xl font-bold text-gray-900 dark:text-white mb-4">
            Development Services
          </h1>
          <p className="text-xl text-gray-600 dark:text-gray-300 max-w-3xl mx-auto">
            Professional software development for web, mobile, desktop, and hybrid platforms
          </p>
        </div>

        {/* Development Services Grid */}
        <div className="grid md:grid-cols-2 gap-8 mb-16">
          {developmentServices.map((service) => {
            const Icon = service.icon;
            return (
              <div
                key={service.id}
                className={`bg-white dark:bg-gray-800 rounded-xl shadow-xl overflow-hidden border-2 ${
                  service.popular ? 'border-blue-600' : 'border-transparent'
                } transition-transform hover:scale-105`}
              >
                {service.popular && (
                  <div className="bg-blue-600 text-white text-center py-2 font-bold">
                    Most Popular
                  </div>
                )}
                
                <div className="p-8">
                  <div className="flex items-center gap-4 mb-4">
                    <div className="bg-blue-100 dark:bg-blue-900 p-4 rounded-lg">
                      <Icon className="w-8 h-8 text-blue-600" />
                    </div>
                    <div>
                      <h3 className="text-2xl font-bold text-gray-900 dark:text-white">
                        {service.name}
                      </h3>
                      <p className="text-gray-600 dark:text-gray-400">{service.description}</p>
                    </div>
                  </div>
                  
                  <div className="mb-6">
                    <span className="text-sm text-gray-600 dark:text-gray-400">Starting from</span>
                    <div>
                      <span className="text-4xl font-bold text-blue-600">${service.price}</span>
                      <span className="text-gray-600 dark:text-gray-400">+</span>
                    </div>
                  </div>

                  <ul className="space-y-3 mb-8">
                    {service.features.map((feature, index) => (
                      <li key={index} className="flex items-start gap-2">
                        <FaCheck className="w-5 h-5 text-green-500 flex-shrink-0 mt-0.5" />
                        <span className="text-gray-700 dark:text-gray-300">{feature}</span>
                      </li>
                    ))}
                  </ul>

                  <button
                    onClick={() => setSelectedService(service)}
                    className="w-full bg-blue-600 text-white py-3 rounded-lg font-bold hover:bg-blue-700 transition-colors"
                  >
                    Request Quote
                  </button>
                </div>
              </div>
            );
          })}
        </div>

        {/* Why Choose Us Section */}
        <div className="bg-gradient-to-r from-blue-600 to-blue-700 rounded-2xl p-12 text-white">
          <h2 className="text-3xl font-bold mb-8 text-center">Why Choose Slyker Tech?</h2>
          <div className="grid md:grid-cols-3 gap-8">
            <div className="text-center">
              <div className="text-4xl font-bold mb-2">10+</div>
              <div className="text-blue-100">Years Experience</div>
            </div>
            <div className="text-center">
              <div className="text-4xl font-bold mb-2">100+</div>
              <div className="text-blue-100">Projects Delivered</div>
            </div>
            <div className="text-center">
              <div className="text-4xl font-bold mb-2">98%</div>
              <div className="text-blue-100">Client Satisfaction</div>
            </div>
          </div>
        </div>

        {/* Project Brief Modal */}
        {selectedService && (
          <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center p-4 z-50 overflow-y-auto">
            <div className="bg-white dark:bg-gray-900 rounded-2xl p-8 max-w-2xl w-full my-8">
              <h3 className="text-2xl font-bold text-gray-900 dark:text-white mb-6">
                Request Quote for {selectedService.name}
              </h3>

              {/* Project Name */}
              <div className="mb-6">
                <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                  Project Name *
                </label>
                <input
                  type="text"
                  value={projectName}
                  onChange={(e) => setProjectName(e.target.value)}
                  placeholder="Enter your project name"
                  className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-800 text-gray-900 dark:text-white"
                />
              </div>

              {/* Timeline */}
              <div className="mb-6">
                <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                  Expected Timeline
                </label>
                <select
                  value={timeline}
                  onChange={(e) => setTimeline(e.target.value)}
                  className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-800 text-gray-900 dark:text-white"
                >
                  <option value="1-3 months">1-3 months</option>
                  <option value="3-6 months">3-6 months</option>
                  <option value="6+ months">6+ months</option>
                  <option value="flexible">Flexible</option>
                </select>
              </div>

              {/* Project Brief */}
              <div className="mb-6">
                <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                  Project Requirements *
                </label>
                <textarea
                  value={projectBrief}
                  onChange={(e) => setProjectBrief(e.target.value)}
                  placeholder="Describe your project requirements, features, target audience, and any specific technologies or integrations needed..."
                  rows={8}
                  className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-800 text-gray-900 dark:text-white resize-none"
                />
                <p className="text-sm text-gray-500 dark:text-gray-400 mt-1">
                  Provide as much detail as possible to help us give you an accurate quote
                </p>
              </div>

              <div className="flex gap-4">
                <button
                  onClick={() => {
                    setSelectedService(null);
                    setProjectName('');
                    setProjectBrief('');
                  }}
                  className="flex-1 py-2 border border-gray-300 dark:border-gray-600 rounded-lg text-gray-700 dark:text-gray-300 hover:bg-gray-100 dark:hover:bg-gray-800"
                >
                  Cancel
                </button>
                <button
                  onClick={() => handleAddToCart(selectedService)}
                  className="flex-1 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700"
                >
                  Add to Cart
                </button>
              </div>
            </div>
          </div>
        )}
      </div>
    </div>
  );
}
