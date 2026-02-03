// app/about/page.tsx

import Image from 'next/image';
import { FaGlobeAfrica, FaCode, FaCloud, FaHandshake } from 'react-icons/fa';
import { MdEngineering, MdPayments, MdSecurity, MdTrendingUp } from 'react-icons/md';
import metadata from './metadata';

export { metadata };

export default function AboutPage() {
  const stats = [
    { value: '50+', label: 'Active Clients', icon: <FaHandshake className="w-8 h-8" /> },
    { value: '100+', label: 'Projects Deployed', icon: <FaCode className="w-8 h-8" /> },
    { value: '8+', label: 'Countries Served', icon: <FaGlobeAfrica className="w-8 h-8" /> },
    { value: '99.9%', label: 'System Uptime', icon: <MdTrendingUp className="w-8 h-8" /> },
  ];

  const services = [
    {
      title: 'Full-Stack Development',
      icon: <FaCode className="w-12 h-12" />,
      items: [
        'Cross-platform web/mobile apps',
        'Cloud-native websites',
        'Fintech & e-commerce platforms',
        'Admin dashboards & portals',
      ],
    },
    {
      title: 'API & Integration',
      icon: <MdEngineering className="w-12 h-12" />,
      items: [
        'REST API development',
        'Payment gateway integration',
        'Mobile money solutions',
        'Webhook architecture',
      ],
    },
    {
      title: 'Cloud Infrastructure',
      icon: <FaCloud className="w-12 h-12" />,
      items: [
        'Docker & Kubernetes deployment',
        'DigitalOcean & AWS hosting',
        'Domain management (.co.zw)',
        'SSL & database security',
      ],
    },
    {
      title: 'Fintech Automation',
      icon: <MdPayments className="w-12 h-12" />,
      items: [
        'Automated payment systems',
        'Digital wallet solutions',
        'Transaction reconciliation',
        'KYC integration',
      ],
    },
  ];

  return (
    <div className="relative z-10">
      {/* Hero Section */}
      <section className="py-28 px-4 sm:px-8 md:px-16 lg:px-24 text-center bg-gradient-to-b from-blue-50 to-transparent dark:from-blue-950/50">
        <div className="max-w-5xl mx-auto">
          <h1 className="text-5xl sm:text-6xl md:text-7xl font-extrabold tracking-tight text-blue-900 dark:text-blue-300 leading-tight">
            Empowering Africa&apos;s <span className="text-darkgoldenrod dark:text-yellow-400">Digital Future</span>
          </h1>
          <p className="mt-8 text-lg sm:text-xl text-gray-700 dark:text-gray-300 max-w-3xl mx-auto">
            Slyker Tech Web Services - Building transformative digital solutions for African businesses since 2020
          </p>
        </div>
      </section>

      {/* Stats Section */}
      <section className="py-20 bg-white dark:bg-gray-950">
        <div className="max-w-6xl mx-auto grid grid-cols-1 md:grid-cols-4 gap-8 px-4 sm:px-8">
          {stats.map((stat, index) => (
            <div
              key={index}
              className="p-6 text-center bg-white dark:bg-gray-900 rounded-xl shadow-sm hover:shadow-md transition-shadow"
            >
              <div className="text-3xl text-darkgoldenrod dark:text-yellow-400 mb-4">{stat.icon}</div>
              <div className="text-4xl font-bold text-blue-900 dark:text-blue-300 mb-2">{stat.value}</div>
              <div className="text-gray-600 dark:text-gray-400 uppercase text-sm tracking-wide">{stat.label}</div>
            </div>
          ))}
        </div>
      </section>

      {/* Our Story */}
      <section className="py-24 px-4 sm:px-8 bg-gray-50 dark:bg-gray-950">
        <div className="max-w-6xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-darkgoldenrod dark:text-yellow-400 mb-12">
            Our African Tech Journey
          </h2>
          <div className="grid md:grid-cols-2 gap-12 items-center">
            <div className="space-y-6">
              <p className="text-lg text-gray-700 dark:text-gray-300">
                Founded in 2020 by Moreblessing Nyemba, Slyker Tech Web Services began as a Zimbabwean-focused web services provider.
                Through pandemic challenges and market evolution, we&apos;ve emerged as a pan-African digital transformation
                partner.
              </p>
              <div className="bg-gradient-to-br from-blue-100 to-yellow-50 dark:from-gray-800 dark:to-gray-700 p-8 rounded-2xl">
                <div className="flex items-center gap-6 p-6 bg-white dark:bg-gray-900 rounded-xl">
                  <MdSecurity className="text-4xl text-darkgoldenrod dark:text-yellow-400" />
                  <div>
                    <h3 className="text-xl font-semibold text-blue-900 dark:text-blue-300">Core Philosophy</h3>
                    <p className="text-gray-600 dark:text-gray-400 mt-2">
                      &quot;Build solutions that solve real African challenges&quot;
                    </p>
                  </div>
                </div>
              </div>
            </div>
            <div className="space-y-8">
              <div className="p-8 bg-white dark:bg-gray-900 rounded-2xl shadow-sm">
                <h3 className="text-2xl font-semibold text-blue-900 dark:text-blue-300 mb-4">Key Milestones</h3>
                <ul className="space-y-4">
                  <li className="flex items-start gap-4">
                    <div className="text-darkgoldenrod dark:text-yellow-400 mt-1">▹</div>
                    <div>
                      <span className="font-medium">2020:</span> Launched with web hosting & static sites
                    </div>
                  </li>
                  <li className="flex items-start gap-4">
                    <div className="text-darkgoldenrod dark:text-yellow-400 mt-1">▹</div>
                    <div>
                      <span className="font-medium">2022:</span> Developed first fintech prototype
                    </div>
                  </li>
                  <li className="flex items-start gap-4">
                    <div className="text-darkgoldenrod dark:text-yellow-400 mt-1">▹</div>
                    <div>
                      <span className="font-medium">2025:</span> Expanded to 8 African markets
                    </div>
                  </li>
                  <li className="flex items-start gap-4">
                    <div className="text-darkgoldenrod dark:text-yellow-400 mt-1">▹</div>
                    <div>
                      <span className="font-medium">2026:</span> Expanded services with enterprise solutions
                    </div>
                  </li>
                </ul>
              </div>
            </div>
          </div>
        </div>
      </section>

      {/* Services Showcase */}
      <section className="py-24 bg-white dark:bg-gray-900">
        <div className="max-w-6xl mx-auto px-4 sm:px-8">
          <h2 className="text-4xl font-bold text-center text-blue-900 dark:text-blue-300 mb-12">
            Our Digital Solutions
          </h2>
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-8">
            {services.map((service, index) => (
              <div
                key={index}
                className="p-8 bg-gray-50 dark:bg-gray-800 rounded-2xl hover:shadow-lg transition-shadow"
              >
                <div className="text-darkgoldenrod dark:text-yellow-400 mb-6">{service.icon}</div>
                <h3 className="text-2xl font-semibold text-blue-900 dark:text-blue-300 mb-4">{service.title}</h3>
                <ul className="space-y-3">
                  {service.items.map((item, i) => (
                    <li key={i} className="flex items-start gap-2 text-gray-600 dark:text-gray-400">
                      <span className="text-darkgoldenrod dark:text-yellow-400">•</span>
                      {item}
                    </li>
                  ))}
                </ul>
              </div>
            ))}
          </div>
        </div>
      </section>

      {/* Founder Section */}
      <section className="py-24 bg-gradient-to-r from-blue-50 to-purple-50 dark:from-gray-900 dark:to-blue-950">
        <div className="max-w-6xl mx-auto px-4 sm:px-8">
          <div className="bg-white dark:bg-gray-900 rounded-2xl p-8 shadow-xl">
            <div className="grid md:grid-cols-3 gap-12 items-center">
              <div className="relative group">
                <div className="absolute inset-0 bg-darkgoldenrod dark:bg-yellow-400 rounded-xl transform group-hover:rotate-2 transition-transform"></div>
                <Image
                  src="/founder-moreblessing.jpg"
                  alt="Moreblessing Nyemba"
                  width={400}
                  height={400}
                  className="relative z-10 rounded-xl w-full h-auto"
                />
              </div>
              <div className="md:col-span-2">
                <h2 className="text-3xl font-bold text-blue-900 dark:text-blue-300 mb-4">
                  Moreblessing Nyemba
                </h2>
                <p className="text-darkgoldenrod dark:text-yellow-400 text-lg mb-6">Founder & Lead Architect</p>
                <p className="text-gray-600 dark:text-gray-400 mb-6">
                  Visionary developer with deep expertise in African fintech infrastructure.
                  Founded Slyker Tech Web Services to bridge the digital divide through accessible,
                  enterprise-grade solutions tailored for African businesses.
                </p>
                <div className="flex gap-4">
                  <div className="p-4 bg-blue-50 dark:bg-gray-800 rounded-lg">
                    <p className="text-sm text-gray-600 dark:text-gray-300">
                      Slyker Tech Web Services is built on my passion for improving lives with technology.
                    </p>
                  </div>
                  <div className="p-4 bg-blue-50 dark:bg-gray-800 rounded-lg">
                    <p className="text-sm text-gray-600 dark:text-gray-300">
                      My ultimate goal is to create products that uplift communities and drive lasting impact.
                    </p>
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>
      </section>
    </div>
  );
}
