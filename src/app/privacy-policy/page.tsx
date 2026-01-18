// app/privacy-policy/page.tsx
'use client';

import { useState } from 'react';
import Link from 'next/link';
import Head from 'next/head';
import { generatePageMetadata } from '@/lib/seo-config';

// Server metadata - exported before client component
export async function generateMetadata() {
  return generatePageMetadata({
    title: 'Privacy Policy',
    description: 'Learn how Slyker Tech protects your personal data and complies with global privacy standards.',
    url: '/privacy-policy',
  });
}

export default function PrivacyPolicy() {
  const [activeTab, setActiveTab] = useState('data-collection');
  const [expandedSection] = useState<string | null>(null);


  const dataFlows = [
    { name: 'Website Visitors', collectionPoints: ['Contact Forms', 'Analytics'], purposes: ['Service Improvement'], sharing: ['Google Analytics'] },
    { name: 'Service Clients', collectionPoints: ['Contracts', 'Payments'], purposes: ['Service Delivery', 'Compliance'], sharing: ['Payment Processors'] },
    { name: 'Newsletter Subscribers', collectionPoints: ['Signup Forms'], purposes: ['Marketing'], sharing: ['Mailchimp'] }
  ];

  const cookies = [
    { name: '_ga', category: 'Analytics', duration: '2 years', purpose: 'Distinguish users' },
    { name: '_gid', category: 'Analytics', duration: '24 hours', purpose: 'Distinguish users' },
    { name: 'cookie_consent', category: 'Essential', duration: '1 year', purpose: 'Store consent preferences' }
  ];

  return (
    <>
      <Head>
        <title>Comprehensive Privacy Policy | Slyker Tech</title>
        <meta name="description" content="Detailed yet beautifully presented privacy policy compliant with global standards" />
      </Head>

      <main className="min-h-screen bg-gray-50 dark:bg-gray-900">
        {/* Hero Section with Interactive Globe */}
        <section className="relative py-24 bg-gradient-to-br from-blue-700 to-blue-900 dark:from-blue-900 dark:to-blue-950 text-white overflow-hidden">
          <div className="absolute inset-0 opacity-20">
            <div className="absolute top-1/4 left-1/4 w-64 h-64 rounded-full bg-blue-400 mix-blend-screen filter blur-3xl"></div>
            <div className="absolute bottom-1/3 right-1/3 w-80 h-80 rounded-full bg-indigo-400 mix-blend-screen filter blur-3xl"></div>
          </div>
          <div className="container mx-auto px-6 text-center relative z-10">
            <div className="inline-flex items-center bg-white/10 backdrop-blur-sm rounded-full px-4 py-1 mb-4">
              <span className="h-2 w-2 rounded-full bg-green-400 mr-2 animate-pulse"></span>
              <span className="text-sm font-medium">GDPR • CCPA • CDPA Compliant</span>
            </div>
            <h1 className="text-4xl md:text-6xl font-bold mb-6 leading-tight">
              Transparent <span className="text-blue-300">Data Protection</span>
            </h1>
            <p className="text-xl md:text-2xl max-w-3xl mx-auto opacity-90">
              Detailed insights into how we handle your information globally
            </p>
          </div>
        </section>

        {/* Interactive Tabs */}
        <div className="container mx-auto px-6 -mt-12 relative z-20">
          <div className="flex overflow-x-auto scrollbar-hide space-x-2 pb-2">
            {['data-collection', 'your-rights', 'cookies', 'data-flows'].map((tab) => (
              <button
                key={tab}
                onClick={() => setActiveTab(tab)}
                className={`px-6 py-3 rounded-full whitespace-nowrap font-medium transition-all ${activeTab === tab ? 'bg-white dark:bg-gray-800 shadow-lg text-blue-600 dark:text-blue-400' : 'bg-white/50 dark:bg-gray-800/50 text-gray-600 dark:text-gray-300 hover:bg-white/80 dark:hover:bg-gray-800/80'}`}
              >
                {tab.replace('-', ' ').replace(/\b\w/g, l => l.toUpperCase())}
              </button>
            ))}
          </div>
        </div>

        {/* Main Content */}
        <section className="py-16 container mx-auto px-6">
          <div className="max-w-6xl mx-auto grid grid-cols-1 lg:grid-cols-4 gap-8">
            {/* Sidebar Navigation */}
            <div className="lg:col-span-1">
              <div className="bg-white dark:bg-gray-800 rounded-xl shadow-md p-6 sticky top-8">
                <h3 className="font-bold text-lg mb-4 flex items-center">
                  <svg className="w-5 h-5 mr-2 text-blue-600 dark:text-blue-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M9 5H7a2 2 0 00-2 2v12a2 2 0 002 2h10a2 2 0 002-2V7a2 2 0 00-2-2h-2M9 5a2 2 0 002 2h2a2 2 0 002-2M9 5a2 2 0 012-2h2a2 2 0 012 2" />
                  </svg>
                  Policy Contents
                </h3>
                <nav className="space-y-2">
                  {[
                    { id: 'data-collection', title: '1. Data Collection' },
                    { id: 'data-use', title: '2. Data Use' },
                    { id: 'legal-basis', title: '3. Legal Basis' },
                    { id: 'data-sharing', title: '4. Data Sharing' },
                    { id: 'international-transfers', title: '5. Global Transfers' },
                    { id: 'security', title: '6. Security Measures' },
                    { id: 'retention', title: '7. Data Retention' },
                    { id: 'your-rights', title: '8. Your Rights' },
                    { id: 'cookies', title: '9. Cookies' },
                    { id: 'changes', title: '10. Policy Updates' }
                  ].map((item) => (
                    <a
                      key={item.id}
                      href={`#${item.id}`}
                      onClick={(e) => {
                        e.preventDefault();
                        document.getElementById(item.id)?.scrollIntoView({ behavior: 'smooth' });
                      }}
                      className={`block px-3 py-2 rounded-lg transition-colors ${expandedSection === item.id ? 'bg-blue-50 dark:bg-blue-900/30 text-blue-600 dark:text-blue-400' : 'text-gray-600 dark:text-gray-300 hover:bg-gray-100 dark:hover:bg-gray-700'}`}
                    >
                      {item.title}
                    </a>
                  ))}
                </nav>
              </div>
            </div>

            {/* Main Policy Content */}
            <div className="lg:col-span-3 space-y-12">
              {/* Data Collection Section */}
              <section id="data-collection" className="bg-white dark:bg-gray-800 rounded-xl shadow-md overflow-hidden">
                <div className="p-8">
                  <div className="flex items-start justify-between">
                    <div>
                      <h2 className="text-2xl font-bold mb-2">1. Data Collection</h2>
                      <p className="text-gray-500 dark:text-gray-400">What information we collect and how</p>
                    </div>
                    <span className="inline-flex items-center px-3 py-1 rounded-full text-sm font-medium bg-blue-100 dark:bg-blue-900 text-blue-800 dark:text-blue-200">
                      <svg className="w-4 h-4 mr-1" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                        <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M13 7h8m0 0v8m0-8l-8 8-4-4-6 6" />
                      </svg>
                      Updated
                    </span>
                  </div>

                  <div className="mt-8 grid grid-cols-1 md:grid-cols-2 gap-6">
                    <div className="border border-gray-200 dark:border-gray-700 rounded-lg p-6">
                      <h3 className="font-semibold mb-3 flex items-center">
                        <svg className="w-5 h-5 mr-2 text-blue-600 dark:text-blue-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M19 11H5m14 0a2 2 0 012 2v6a2 2 0 01-2 2H5a2 2 0 01-2-2v-6a2 2 0 012-2m14 0V9a2 2 0 00-2-2M5 11V9a2 2 0 012-2m0 0V5a2 2 0 012-2h6a2 2 0 012 2v2M7 7h10" />
                        </svg>
                        Directly Collected
                      </h3>
                      <ul className="space-y-2">
                        <li className="flex items-start">
                          <span className="inline-block w-2 h-2 rounded-full bg-blue-600 mt-2 mr-2"></span>
                          <span>Contact information (name, email, phone)</span>
                        </li>
                        <li className="flex items-start">
                          <span className="inline-block w-2 h-2 rounded-full bg-blue-600 mt-2 mr-2"></span>
                          <span>Payment details (processed securely)</span>
                        </li>
                        <li className="flex items-start">
                          <span className="inline-block w-2 h-2 rounded-full bg-blue-600 mt-2 mr-2"></span>
                          <span>Service-specific data (project requirements)</span>
                        </li>
                      </ul>
                    </div>

                    <div className="border border-gray-200 dark:border-gray-700 rounded-lg p-6">
                      <h3 className="font-semibold mb-3 flex items-center">
                        <svg className="w-5 h-5 mr-2 text-purple-600 dark:text-purple-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M13.828 10.172a4 4 0 00-5.656 0l-4 4a4 4 0 105.656 5.656l1.102-1.101m-.758-4.899a4 4 0 005.656 0l4-4a4 4 0 00-5.656-5.656l-1.1 1.1" />
                        </svg>
                        Automatically Collected
                      </h3>
                      <ul className="space-y-2">
                        <li className="flex items-start">
                          <span className="inline-block w-2 h-2 rounded-full bg-purple-600 mt-2 mr-2"></span>
                          <span>Device information (browser, OS)</span>
                        </li>
                        <li className="flex items-start">
                          <span className="inline-block w-2 h-2 rounded-full bg-purple-600 mt-2 mr-2"></span>
                          <span>Usage data (pages visited, duration)</span>
                        </li>
                        <li className="flex items-start">
                          <span className="inline-block w-2 h-2 rounded-full bg-purple-600 mt-2 mr-2"></span>
                          <span>Approximate location (country-level)</span>
                        </li>
                      </ul>
                    </div>
                  </div>
                </div>
              </section>

              {/* Data Flow Visualization */}
              <section id="data-flows" className="bg-white dark:bg-gray-800 rounded-xl shadow-md overflow-hidden">
                <div className="p-8">
                  <h2 className="text-2xl font-bold mb-6">Data Flow Map</h2>
                  <p className="text-gray-600 dark:text-gray-300 mb-6">How information moves through our systems</p>
                  
                  <div className="overflow-x-auto">
                    <table className="min-w-full divide-y divide-gray-200 dark:divide-gray-700">
                      <thead className="bg-gray-50 dark:bg-gray-700">
                        <tr>
                          <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 dark:text-gray-300 uppercase tracking-wider">Data Type</th>
                          <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 dark:text-gray-300 uppercase tracking-wider">Collection Points</th>
                          <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 dark:text-gray-300 uppercase tracking-wider">Primary Purposes</th>
                          <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 dark:text-gray-300 uppercase tracking-wider">Third-Party Sharing</th>
                        </tr>
                      </thead>
                      <tbody className="bg-white dark:bg-gray-800 divide-y divide-gray-200 dark:divide-gray-700">
                        {dataFlows.map((flow, index) => (
                          <tr key={index} className={index % 2 === 0 ? 'bg-gray-50 dark:bg-gray-700/30' : ''}>
                            <td className="px-6 py-4 whitespace-nowrap font-medium">{flow.name}</td>
                            <td className="px-6 py-4">
                              <div className="flex flex-wrap gap-2">
                                {flow.collectionPoints.map((point, i) => (
                                  <span key={i} className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-blue-100 dark:bg-blue-900 text-blue-800 dark:text-blue-200">
                                    {point}
                                  </span>
                                ))}
                              </div>
                            </td>
                            <td className="px-6 py-4">
                              <div className="flex flex-wrap gap-2">
                                {flow.purposes.map((purpose, i) => (
                                  <span key={i} className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-green-100 dark:bg-green-900 text-green-800 dark:text-green-200">
                                    {purpose}
                                  </span>
                                ))}
                              </div>
                            </td>
                            <td className="px-6 py-4">
                              {flow.sharing.map((share, i) => (
                                <span key={i} className="inline-block px-2.5 py-0.5 rounded-full text-xs font-medium bg-purple-100 dark:bg-purple-900 text-purple-800 dark:text-purple-200 mr-2 mb-2">
                                  {share}
                                </span>
                              ))}
                            </td>
                          </tr>
                        ))}
                      </tbody>
                    </table>
                  </div>
                </div>
              </section>

              {/* Cookies Section */}
              <section id="cookies" className="bg-white dark:bg-gray-800 rounded-xl shadow-md overflow-hidden">
                <div className="p-8">
                  <h2 className="text-2xl font-bold mb-6">9. Cookie Policy</h2>
                  
                  <div className="mb-6">
                    <div className="flex items-center justify-between mb-4">
                      <h3 className="font-semibold">Cookie Consent Preferences</h3>
                      <div className="flex space-x-2">
                        <button className="px-3 py-1 text-xs bg-blue-600 text-white rounded-md">Accept All</button>
                        <button className="px-3 py-1 text-xs border border-gray-300 dark:border-gray-600 rounded-md">Configure</button>
                      </div>
                    </div>
                    <div className="h-2 bg-gray-200 dark:bg-gray-700 rounded-full overflow-hidden">
                      <div className="h-full bg-gradient-to-r from-blue-500 to-green-500" style={{ width: '75%' }}></div>
                    </div>
                    <div className="flex justify-between text-xs text-gray-500 dark:text-gray-400 mt-1">
                      <span>Essential: 100%</span>
                      <span>Analytics: 65%</span>
                      <span>Marketing: 0%</span>
                    </div>
                  </div>

                  <div className="overflow-x-auto">
                    <table className="min-w-full divide-y divide-gray-200 dark:divide-gray-700">
                      <thead className="bg-gray-50 dark:bg-gray-700">
                        <tr>
                          <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 dark:text-gray-300 uppercase tracking-wider">Name</th>
                          <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 dark:text-gray-300 uppercase tracking-wider">Category</th>
                          <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 dark:text-gray-300 uppercase tracking-wider">Duration</th>
                          <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 dark:text-gray-300 uppercase tracking-wider">Purpose</th>
                        </tr>
                      </thead>
                      <tbody className="bg-white dark:bg-gray-800 divide-y divide-gray-200 dark:divide-gray-700">
                        {cookies.map((cookie, index) => (
                          <tr key={index}>
                            <td className="px-6 py-4 whitespace-nowrap font-mono text-sm">{cookie.name}</td>
                            <td className="px-6 py-4 whitespace-nowrap">
                              <span className={`px-2 inline-flex text-xs leading-5 font-semibold rounded-full ${
                                cookie.category === 'Essential' 
                                  ? 'bg-green-100 dark:bg-green-900 text-green-800 dark:text-green-200' 
                                  : 'bg-blue-100 dark:bg-blue-900 text-blue-800 dark:text-blue-200'
                              }`}>
                                {cookie.category}
                              </span>
                            </td>
                            <td className="px-6 py-4 whitespace-nowrap text-sm">{cookie.duration}</td>
                            <td className="px-6 py-4 text-sm">{cookie.purpose}</td>
                          </tr>
                        ))}
                      </tbody>
                    </table>
                  </div>
                </div>
              </section>

              {/* Rights Section */}
              <section id="your-rights" className="bg-white dark:bg-gray-800 rounded-xl shadow-md overflow-hidden">
                <div className="p-8">
                  <h2 className="text-2xl font-bold mb-6">8. Your Data Rights</h2>
                  
                  <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
                    {[
                      { 
                        icon: 'M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z',
                        title: 'Access', 
                        description: 'Request copies of your personal data',
                        action: 'Request Data'
                      },
                      { 
                        icon: 'M19 7l-.867 12.142A2 2 0 0116.138 21H7.862a2 2 0 01-1.995-1.858L5 7m5 4v6m4-6v6m1-10V4a1 1 0 00-1-1h-4a1 1 0 00-1 1v3M4 7h16',
                        title: 'Deletion', 
                        description: 'Request erasure under certain conditions',
                        action: 'Request Erasure'
                      },
                      { 
                        icon: 'M4 16v1a3 3 0 003 3h10a3 3 0 003-3v-1m-4-8l-4-4m0 0L8 8m4-4v12',
                        title: 'Portability', 
                        description: 'Receive your data in a machine-readable format',
                        action: 'Export Data'
                      },
                      { 
                        icon: 'M18.364 5.636l-3.536 3.536m0 5.656l3.536 3.536M9.172 9.172L5.636 5.636m3.536 9.192l-3.536 3.536M21 12a9 9 0 11-18 0 9 9 0 0118 0zm-5 0a4 4 0 11-8 0 4 4 0 018 0z',
                        title: 'Objection', 
                        description: 'Object to certain processing activities',
                        action: 'Submit Objection'
                      },
                      { 
                        icon: 'M8 7V3m8 4V3m-9 8h10M5 21h14a2 2 0 002-2V7a2 2 0 00-2-2H5a2 2 0 00-2 2v12a2 2 0 002 2z',
                        title: 'Restriction', 
                        description: 'Limit how we use your data',
                        action: 'Request Restriction'
                      },
                      { 
                        icon: 'M3 10h18M7 15h1m4 0h1m-7 4h12a3 3 0 003-3V8a3 3 0 00-3-3H6a3 3 0 00-3 3v8a3 3 0 003 3z',
                        title: 'Withdraw Consent', 
                        description: 'Revoke previously given permissions',
                        action: 'Withdraw'
                      }
                    ].map((right, index) => (
                      <div key={index} className="border border-gray-200 dark:border-gray-700 rounded-lg p-6 hover:shadow-md transition-shadow">
                        <div className="w-10 h-10 rounded-full bg-blue-100 dark:bg-blue-900/50 flex items-center justify-center text-blue-600 dark:text-blue-400 mb-4">
                          <svg className="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d={right.icon} />
                          </svg>
                        </div>
                        <h3 className="font-semibold text-lg mb-2">{right.title}</h3>
                        <p className="text-gray-600 dark:text-gray-300 mb-4">{right.description}</p>
                        <button className="text-sm px-4 py-2 bg-blue-600 hover:bg-blue-700 text-white rounded-md transition-colors">
                          {right.action}
                        </button>
                      </div>
                    ))}
                  </div>
                </div>
              </section>

              {/* Contact Section */}
              <section className="bg-gradient-to-r from-blue-600 to-blue-800 dark:from-blue-700 dark:to-blue-900 rounded-xl shadow-md overflow-hidden">
                <div className="p-8 text-white">
                  <h2 className="text-2xl font-bold mb-2">Privacy Questions?</h2>
                  <p className="opacity-90 mb-6">Contact our Data Protection Officer</p>
                  
                  <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                    <div>
                      <h3 className="font-semibold mb-3 flex items-center">
                        <svg className="w-5 h-5 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M3 8l7.89 5.26a2 2 0 002.22 0L21 8M5 19h14a2 2 0 002-2V7a2 2 0 00-2-2H5a2 2 0 00-2 2v10a2 2 0 002 2z" />
                        </svg>
                        Email
                      </h3>
                      <Link href="mailto:dpo@slykertech.co.zw" className="text-blue-100 hover:text-white underline">dpo@slykertech.co.zw</Link>
                    </div>
                    <div>
                      <h3 className="font-semibold mb-3 flex items-center">
                        <svg className="w-5 h-5 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M3 5a2 2 0 012-2h3.28a1 1 0 01.948.684l1.498 4.493a1 1 0 01-.502 1.21l-2.257 1.13a11.042 11.042 0 005.516 5.516l1.13-2.257a1 1 0 011.21-.502l4.493 1.498a1 1 0 01.684.949V19a2 2 0 01-2 2h-1C9.716 21 3 14.284 3 6V5z" />
                        </svg>
                        Phone
                      </h3>
                      <Link href="tel:+263787211325" className="text-blue-100 hover:text-white underline">+263 78 721 1325</Link>
                    </div>
                  </div>
                  
                  <div className="mt-8 pt-6 border-t border-blue-500/30">
                    <h3 className="font-semibold mb-3">Postal Address</h3>
                    <address className="not-italic">
                      Data Protection Officer<br />
                      Slyker Tech Web Services<br />
                      [Your Physical Address]<br />
                      Harare, Zimbabwe
                    </address>
                  </div>
                </div>
              </section>
            </div>
          </div>
        </section>
      </main>
    </>
  );
}