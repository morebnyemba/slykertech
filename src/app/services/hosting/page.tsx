'use client';

import { useState } from 'react';
import { FaServer, FaCloud, FaShieldAlt, FaClock, FaCheck } from 'react-icons/fa';
import { useCartStore } from '@/lib/stores/cart-store';
import { useAuthStore } from '@/lib/stores/auth-store';

interface HostingPlan {
  id: number;
  type: string;
  name: string;
  price: number;
  description: string;
  features: string[];
  regions: string[];
  osOptions?: string[];
  popular: boolean;
}

const hostingPlans: HostingPlan[] = [
  {
    id: 1,
    type: 'shared',
    name: 'Shared Starter',
    price: 10,
    description: 'Perfect for small websites and blogs',
    features: [
      '10 GB SSD Storage',
      '100 GB Bandwidth',
      '5 Email Accounts',
      'Free SSL Certificate',
      'cPanel Access',
      'Daily Backups',
    ],
    regions: ['US', 'EU', 'Asia'],
    popular: false,
  },
  {
    id: 2,
    type: 'vps',
    name: 'VPS Business',
    price: 50,
    description: 'Scalable resources for growing businesses',
    features: [
      '4 CPU Cores',
      '8 GB RAM',
      '100 GB SSD Storage',
      'Unlimited Bandwidth',
      'Root Access',
      'DDoS Protection',
      'Free SSL',
      '99.9% Uptime',
    ],
    regions: ['US', 'EU', 'Asia'],
    osOptions: ['Ubuntu 22.04', 'CentOS 8', 'Debian 11', 'Windows Server 2019'],
    popular: true,
  },
  {
    id: 3,
    type: 'dedicated',
    name: 'Dedicated Enterprise',
    price: 200,
    description: 'Maximum performance and control',
    features: [
      '16 CPU Cores',
      '64 GB RAM',
      '2 TB SSD Storage',
      'Unlimited Bandwidth',
      'Full Root Access',
      'Advanced DDoS Protection',
      'Managed Services Available',
      '100% Uptime SLA',
    ],
    regions: ['US', 'EU', 'Asia'],
    osOptions: ['Ubuntu 22.04', 'CentOS 8', 'Debian 11', 'Windows Server 2019', 'Windows Server 2022'],
    popular: false,
  },
];

export default function HostingPage() {
  const [selectedPlan, setSelectedPlan] = useState<HostingPlan | null>(null);
  const [selectedRegion, setSelectedRegion] = useState('');
  const [selectedOS, setSelectedOS] = useState('');
  const [billingCycle, setBillingCycle] = useState('monthly');
  
  const { addItem } = useCartStore();
  const { token } = useAuthStore();

  const handleAddToCart = async (plan: HostingPlan) => {
    if (!selectedRegion) {
      alert('Please select a region');
      return;
    }

    if (plan.osOptions && !selectedOS) {
      alert('Please select an operating system');
      return;
    }

    const cartItem = {
      service: plan.id,
      service_metadata: {
        type: plan.type,
        region: selectedRegion,
        ...(plan.osOptions && { os: selectedOS }),
        ram: plan.type === 'vps' ? '8GB' : plan.type === 'dedicated' ? '64GB' : undefined,
        cpu: plan.type === 'vps' ? '4 cores' : plan.type === 'dedicated' ? '16 cores' : undefined,
      },
      quantity: 1,
      unit_price: plan.price,
      billing_cycle: billingCycle,
    };

    const result = await addItem(cartItem, token || undefined);
    
    if (result.success) {
      alert('Added to cart successfully!');
      setSelectedPlan(null);
      setSelectedRegion('');
      setSelectedOS('');
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
            Web Hosting Services
          </h1>
          <p className="text-xl text-gray-600 dark:text-gray-300 max-w-3xl mx-auto">
            Reliable, fast, and secure hosting solutions for your websites and applications
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

        {/* Hosting Plans */}
        <div className="grid md:grid-cols-3 gap-8 mb-16">
          {hostingPlans.map((plan) => (
            <div
              key={plan.id}
              className={`bg-white dark:bg-gray-800 rounded-xl shadow-xl overflow-hidden border-2 ${
                plan.popular ? 'border-blue-600' : 'border-transparent'
              } transition-transform hover:scale-105`}
            >
              {plan.popular && (
                <div className="bg-blue-600 text-white text-center py-2 font-bold">
                  Most Popular
                </div>
              )}
              
              <div className="p-8">
                <h3 className="text-2xl font-bold text-gray-900 dark:text-white mb-2">
                  {plan.name}
                </h3>
                <p className="text-gray-600 dark:text-gray-400 mb-6">{plan.description}</p>
                
                <div className="mb-6">
                  <span className="text-4xl font-bold text-blue-600">${plan.price}</span>
                  <span className="text-gray-600 dark:text-gray-400">/month</span>
                </div>

                <ul className="space-y-3 mb-8">
                  {plan.features.map((feature, index) => (
                    <li key={index} className="flex items-start gap-2">
                      <FaCheck className="w-5 h-5 text-green-500 flex-shrink-0 mt-0.5" />
                      <span className="text-gray-700 dark:text-gray-300">{feature}</span>
                    </li>
                  ))}
                </ul>

                <button
                  onClick={() => setSelectedPlan(plan)}
                  className="w-full bg-blue-600 text-white py-3 rounded-lg font-bold hover:bg-blue-700 transition-colors"
                >
                  Select Plan
                </button>
              </div>
            </div>
          ))}
        </div>

        {/* Configuration Modal */}
        {selectedPlan && (
          <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center p-4 z-50">
            <div className="bg-white dark:bg-gray-900 rounded-2xl p-8 max-w-md w-full">
              <h3 className="text-2xl font-bold text-gray-900 dark:text-white mb-6">
                Configure {selectedPlan.name}
              </h3>

              {/* Region Selection */}
              <div className="mb-6">
                <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                  Select Region
                </label>
                <select
                  value={selectedRegion}
                  onChange={(e) => setSelectedRegion(e.target.value)}
                  className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-800 text-gray-900 dark:text-white"
                >
                  <option value="">Choose a region...</option>
                  {selectedPlan.regions.map((region: string) => (
                    <option key={region} value={region}>{region}</option>
                  ))}
                </select>
              </div>

              {/* OS Selection (for VPS and Dedicated) */}
              {selectedPlan.osOptions && (
                <div className="mb-6">
                  <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                    Select Operating System
                  </label>
                  <select
                    value={selectedOS}
                    onChange={(e) => setSelectedOS(e.target.value)}
                    className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-800 text-gray-900 dark:text-white"
                  >
                    <option value="">Choose an OS...</option>
                    {selectedPlan.osOptions.map((os: string) => (
                      <option key={os} value={os}>{os}</option>
                    ))}
                  </select>
                </div>
              )}

              {/* Billing Cycle */}
              <div className="mb-6">
                <label className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                  Billing Cycle
                </label>
                <select
                  value={billingCycle}
                  onChange={(e) => setBillingCycle(e.target.value)}
                  className="w-full px-4 py-2 border border-gray-300 dark:border-gray-600 rounded-lg bg-white dark:bg-gray-800 text-gray-900 dark:text-white"
                >
                  <option value="monthly">Monthly</option>
                  <option value="quarterly">Quarterly (Save 10%)</option>
                  <option value="annual">Annual (Save 20%)</option>
                </select>
              </div>

              <div className="flex gap-4">
                <button
                  onClick={() => {
                    setSelectedPlan(null);
                    setSelectedRegion('');
                    setSelectedOS('');
                  }}
                  className="flex-1 py-2 border border-gray-300 dark:border-gray-600 rounded-lg text-gray-700 dark:text-gray-300 hover:bg-gray-100 dark:hover:bg-gray-800"
                >
                  Cancel
                </button>
                <button
                  onClick={() => handleAddToCart(selectedPlan)}
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
