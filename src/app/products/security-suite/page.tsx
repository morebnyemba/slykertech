import type { Metadata } from 'next';
import { FaShieldAlt, FaLock, FaCertificate, FaWhatsapp, FaEnvelope } from 'react-icons/fa';
import { MdVerifiedUser, MdHttps, MdScanner } from 'react-icons/md';
import { generatePageMetadata } from '@/lib/seo-config';

export const metadata: Metadata = generatePageMetadata({
  title: 'SSL & Security Suite',
  description: 'Comprehensive website security with SSL certificates, malware protection, DDoS mitigation, and vulnerability scanning. Protect your African business online.',
  url: '/products/security-suite'
});

export default function SecuritySuitePage() {
  const whatsappMessage = encodeURIComponent(
    "Hi Slyker Tech! I'm interested in the SSL & Security Suite..."
  );

  const features = [
    {
      icon: <MdHttps className="w-10 h-10" />,
      title: 'SSL Certificates',
      description: 'Free and premium SSL certificates with automatic installation and renewal'
    },
    {
      icon: <MdScanner className="w-10 h-10" />,
      title: 'Malware Scanning',
      description: 'Daily automated scans with instant alerts and one-click malware removal'
    },
    {
      icon: <FaShieldAlt className="w-10 h-10" />,
      title: 'DDoS Protection',
      description: 'Enterprise-grade DDoS mitigation protecting against volumetric attacks'
    },
    {
      icon: <FaLock className="w-10 h-10" />,
      title: 'Web Firewall',
      description: 'Advanced WAF blocking SQL injection, XSS, and other web attacks'
    },
    {
      icon: <MdVerifiedUser className="w-10 h-10" />,
      title: 'Vulnerability Scanning',
      description: 'Continuous monitoring for security vulnerabilities and outdated software'
    },
    {
      icon: <FaCertificate className="w-10 h-10" />,
      title: 'Trust Seals',
      description: 'Display security badges to build customer trust and increase conversions'
    }
  ];

  const plans = [
    {
      name: 'Essential',
      price: '$9',
      period: '/month',
      features: [
        'Free SSL Certificate',
        'Basic DDoS Protection',
        'Weekly Malware Scans',
        'Security Monitoring',
        'Email Alerts',
        'Email Support'
      ]
    },
    {
      name: 'Professional',
      price: '$29',
      period: '/month',
      popular: true,
      features: [
        'Premium SSL Certificate',
        'Advanced DDoS Protection',
        'Daily Malware Scans',
        'Web Application Firewall',
        'Auto Malware Removal',
        'Security Dashboard',
        'Priority Support',
        'Trust Seals'
      ]
    },
    {
      name: 'Enterprise',
      price: '$99',
      period: '/month',
      features: [
        'Wildcard SSL Certificate',
        'Enterprise DDoS Protection',
        'Real-time Malware Protection',
        'Advanced WAF Rules',
        'Vulnerability Scanning',
        'Penetration Testing',
        '24/7 Security Monitoring',
        'Dedicated Security Team',
        'Compliance Support'
      ]
    }
  ];

  return (
    <div className="relative z-10">
      {/* Hero Section */}
      <section className="py-28 px-4 sm:px-8 md:px-16 lg:px-24 text-center bg-gradient-to-b from-blue-50 to-transparent dark:from-blue-950/50">
        <div className="max-w-5xl mx-auto">
          <FaShieldAlt className="w-20 h-20 mx-auto text-darkgoldenrod dark:text-yellow-400 mb-8" />
          <h1 className="text-5xl sm:text-6xl md:text-7xl font-extrabold tracking-tight text-blue-900 dark:text-blue-300 leading-tight">
            SSL & <span className="text-darkgoldenrod dark:text-yellow-400">Security</span> Suite
          </h1>
          <p className="mt-8 text-lg sm:text-xl text-gray-700 dark:text-gray-300 max-w-3xl mx-auto">
            Comprehensive website security protecting African businesses from cyber threats, malware, and attacks
          </p>
        </div>
      </section>

      {/* Features Section */}
      <section className="py-24 px-4 sm:px-8 bg-white dark:bg-gray-950">
        <div className="max-w-6xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-blue-900 dark:text-blue-300 mb-12">
            Complete Security Protection
          </h2>
          <div className="grid grid-cols-1 md:grid-cols-3 gap-8">
            {features.map((feature, index) => (
              <div
                key={index}
                className="p-8 bg-gray-50 dark:bg-gray-900 rounded-2xl hover:shadow-lg transition-shadow"
              >
                <div className="text-darkgoldenrod dark:text-yellow-400 mb-4">
                  {feature.icon}
                </div>
                <h3 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-3">
                  {feature.title}
                </h3>
                <p className="text-gray-600 dark:text-gray-400">
                  {feature.description}
                </p>
              </div>
            ))}
          </div>
        </div>
      </section>

      {/* Why Security Matters */}
      <section className="py-24 px-4 sm:px-8 bg-gray-50 dark:bg-gray-900">
        <div className="max-w-6xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-darkgoldenrod dark:text-yellow-400 mb-12">
            Why Website Security Matters
          </h2>
          <div className="grid md:grid-cols-2 gap-8">
            <div className="p-8 bg-white dark:bg-gray-800 rounded-2xl">
              <h3 className="text-2xl font-semibold text-blue-900 dark:text-blue-300 mb-6">
                Protect Your Business
              </h3>
              <ul className="space-y-3 text-gray-600 dark:text-gray-400">
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Prevent data breaches and customer information theft
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Stop malware infections that damage your reputation
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Avoid website downtime from cyber attacks
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Comply with data protection regulations
                </li>
              </ul>
            </div>
            <div className="p-8 bg-white dark:bg-gray-800 rounded-2xl">
              <h3 className="text-2xl font-semibold text-blue-900 dark:text-blue-300 mb-6">
                Build Customer Trust
              </h3>
              <ul className="space-y-3 text-gray-600 dark:text-gray-400">
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  SSL encryption shows &quot;Secure&quot; in browsers
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Trust seals increase conversions by up to 32%
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Google ranks secure sites higher in search
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Protect your brand reputation online
                </li>
              </ul>
            </div>
          </div>
        </div>
      </section>

      {/* Threat Landscape */}
      <section className="py-24 px-4 sm:px-8 bg-white dark:bg-gray-950">
        <div className="max-w-6xl mx-auto">
          <div className="bg-gradient-to-br from-blue-50 to-purple-50 dark:from-gray-800 dark:to-blue-900/50 rounded-3xl p-12">
            <h2 className="text-4xl font-bold text-blue-900 dark:text-blue-300 mb-8 text-center">
              Growing Cyber Threats in Africa
            </h2>
            <div className="grid md:grid-cols-3 gap-8">
              <div className="text-center">
                <div className="text-4xl font-bold text-darkgoldenrod dark:text-yellow-400 mb-2">
                  300%
                </div>
                <p className="text-gray-600 dark:text-gray-400">
                  Increase in cyber attacks targeting African businesses since 2020
                </p>
              </div>
              <div className="text-center">
                <div className="text-4xl font-bold text-darkgoldenrod dark:text-yellow-400 mb-2">
                  $4M
                </div>
                <p className="text-gray-600 dark:text-gray-400">
                  Average cost of a data breach for African enterprises
                </p>
              </div>
              <div className="text-center">
                <div className="text-4xl font-bold text-darkgoldenrod dark:text-yellow-400 mb-2">
                  43%
                </div>
                <p className="text-gray-600 dark:text-gray-400">
                  Of cyber attacks target small businesses with weak security
                </p>
              </div>
            </div>
          </div>
        </div>
      </section>

      {/* Pricing Section */}
      <section className="py-24 px-4 sm:px-8 bg-gray-50 dark:bg-gray-900">
        <div className="max-w-6xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-blue-900 dark:text-blue-300 mb-12">
            Security Plans
          </h2>
          <div className="grid grid-cols-1 md:grid-cols-3 gap-8">
            {plans.map((plan, index) => (
              <div
                key={index}
                className={`p-8 rounded-2xl ${
                  plan.popular
                    ? 'bg-gradient-to-br from-blue-600 to-purple-700 dark:from-blue-900 dark:to-purple-900 text-white shadow-xl scale-105'
                    : 'bg-white dark:bg-gray-800'
                }`}
              >
                {plan.popular && (
                  <div className="text-center mb-4">
                    <span className="px-4 py-1 bg-yellow-400 text-gray-900 rounded-full text-sm font-semibold">
                      Most Popular
                    </span>
                  </div>
                )}
                <h3
                  className={`text-2xl font-bold mb-4 ${
                    plan.popular ? 'text-white' : 'text-blue-900 dark:text-blue-300'
                  }`}
                >
                  {plan.name}
                </h3>
                <div className="mb-6">
                  <span className={`text-4xl font-bold ${plan.popular ? 'text-white' : 'text-darkgoldenrod dark:text-yellow-400'}`}>
                    {plan.price}
                  </span>
                  <span className={plan.popular ? 'text-blue-100' : 'text-gray-600 dark:text-gray-400'}>
                    {plan.period}
                  </span>
                </div>
                <ul className="space-y-3 mb-8">
                  {plan.features.map((feature, i) => (
                    <li key={i} className="flex items-start gap-2">
                      <span className={plan.popular ? 'text-yellow-400' : 'text-darkgoldenrod dark:text-yellow-400'}>
                        ✓
                      </span>
                      <span className={plan.popular ? 'text-blue-100' : 'text-gray-600 dark:text-gray-400'}>
                        {feature}
                      </span>
                    </li>
                  ))}
                </ul>
                <a
                  href={`https://wa.me/263787211325?text=${encodeURIComponent(`Hi! I'm interested in the ${plan.name} Security Suite plan...`)}`}
                  target="_blank"
                  rel="noopener noreferrer"
                  className={`block text-center px-6 py-3 rounded-lg font-semibold transition-colors ${
                    plan.popular
                      ? 'bg-white text-blue-900 hover:bg-gray-100'
                      : 'bg-blue-600 hover:bg-blue-700 dark:bg-blue-700 dark:hover:bg-blue-600 text-white'
                  }`}
                >
                  Get Started
                </a>
              </div>
            ))}
          </div>
        </div>
      </section>

      {/* CTA Section */}
      <section className="py-24 px-4 sm:px-8 bg-gradient-to-r from-blue-600 to-purple-700 dark:from-blue-900 dark:to-purple-900">
        <div className="max-w-4xl mx-auto text-center">
          <h2 className="text-4xl font-bold text-white mb-6">
            Protect Your Website Today
          </h2>
          <p className="text-xl text-blue-100 dark:text-blue-200 mb-12">
            Don&apos;t wait for a cyber attack—secure your business with our comprehensive security suite
          </p>
          <div className="flex flex-col sm:flex-row gap-6 justify-center">
            <a
              href={`https://wa.me/263787211325?text=${whatsappMessage}`}
              target="_blank"
              rel="noopener noreferrer"
              className="inline-flex items-center justify-center gap-3 px-8 py-4 bg-green-600 hover:bg-green-700 text-white rounded-lg font-semibold transition-colors shadow-lg"
            >
              <FaWhatsapp className="w-5 h-5" />
              WhatsApp Us: +263 78 721 1325
            </a>
            <a
              href="mailto:support@slykertech.co.zw"
              className="inline-flex items-center justify-center gap-3 px-8 py-4 bg-white hover:bg-gray-100 text-blue-900 rounded-lg font-semibold transition-colors shadow-lg"
            >
              <FaEnvelope className="w-5 h-5" />
              Email Security Team
            </a>
          </div>
        </div>
      </section>
    </div>
  );
}
