import type { Metadata } from 'next';
import { FaWordpress, FaRocket, FaShieldAlt, FaWhatsapp, FaEnvelope } from 'react-icons/fa';
import { MdSpeed, MdSecurity, MdBackup, MdUpdate } from 'react-icons/md';
import { generatePageMetadata } from '@/lib/seo-config';

export const metadata: Metadata = generatePageMetadata({
  title: 'Managed WordPress Hosting',
  description: 'Enterprise-grade managed WordPress hosting across Africa. Automatic updates, daily backups, advanced security, and lightning-fast performance with 99.9% uptime.',
  url: '/products/managed-wordpress'
});

export default function ManagedWordPressPage() {
  const whatsappMessage = encodeURIComponent(
    "Hi Slyker Tech! I'm interested in Managed WordPress hosting..."
  );

  const features = [
    {
      icon: <MdSpeed className="w-10 h-10" />,
      title: 'Lightning Fast',
      description: 'Optimized servers, CDN integration, and advanced caching for blazing-fast load times'
    },
    {
      icon: <MdSecurity className="w-10 h-10" />,
      title: 'Advanced Security',
      description: 'Real-time malware scanning, firewall protection, and SSL certificates included'
    },
    {
      icon: <MdBackup className="w-10 h-10" />,
      title: 'Daily Backups',
      description: 'Automated daily backups with one-click restore and 30-day retention'
    },
    {
      icon: <MdUpdate className="w-10 h-10" />,
      title: 'Auto Updates',
      description: 'WordPress core, themes, and plugins updated automatically with testing'
    },
    {
      icon: <FaShieldAlt className="w-10 h-10" />,
      title: 'DDoS Protection',
      description: 'Enterprise-grade DDoS mitigation and traffic filtering included'
    },
    {
      icon: <FaRocket className="w-10 h-10" />,
      title: 'Staging Sites',
      description: 'Test changes on staging environment before deploying to production'
    }
  ];

  const plans = [
    {
      name: 'Starter',
      price: '$29',
      period: '/month',
      features: [
        '1 WordPress Site',
        '20GB SSD Storage',
        '50GB Bandwidth',
        'Free SSL Certificate',
        'Daily Backups',
        'Email Support'
      ]
    },
    {
      name: 'Business',
      price: '$79',
      period: '/month',
      popular: true,
      features: [
        '5 WordPress Sites',
        '100GB SSD Storage',
        '200GB Bandwidth',
        'Free SSL Certificates',
        'Daily Backups',
        'Priority Support',
        'Staging Environment',
        'CDN Included'
      ]
    },
    {
      name: 'Enterprise',
      price: '$199',
      period: '/month',
      features: [
        'Unlimited Sites',
        '500GB SSD Storage',
        'Unlimited Bandwidth',
        'Free SSL Certificates',
        'Hourly Backups',
        '24/7 Support',
        'Multiple Staging',
        'Premium CDN',
        'Dedicated Resources'
      ]
    }
  ];

  return (
    <div className="relative z-10">
      {/* Hero Section */}
      <section className="py-28 px-4 sm:px-8 md:px-16 lg:px-24 text-center bg-gradient-to-b from-blue-50 to-transparent dark:from-blue-950/50">
        <div className="max-w-5xl mx-auto">
          <FaWordpress className="w-20 h-20 mx-auto text-darkgoldenrod dark:text-yellow-400 mb-8" />
          <h1 className="text-5xl sm:text-6xl md:text-7xl font-extrabold tracking-tight text-blue-900 dark:text-blue-300 leading-tight">
            Managed <span className="text-darkgoldenrod dark:text-yellow-400">WordPress</span> Hosting
          </h1>
          <p className="mt-8 text-lg sm:text-xl text-gray-700 dark:text-gray-300 max-w-3xl mx-auto">
            Enterprise-grade WordPress hosting optimized for performance, security, and reliability across Africa and beyond
          </p>
        </div>
      </section>

      {/* Features Section */}
      <section className="py-24 px-4 sm:px-8 bg-white dark:bg-gray-950">
        <div className="max-w-6xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-blue-900 dark:text-blue-300 mb-12">
            Why Choose Our Managed WordPress
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

      {/* Technical Specs */}
      <section className="py-24 px-4 sm:px-8 bg-gray-50 dark:bg-gray-900">
        <div className="max-w-6xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-darkgoldenrod dark:text-yellow-400 mb-12">
            Technical Excellence
          </h2>
          <div className="grid md:grid-cols-2 gap-8">
            <div className="p-8 bg-white dark:bg-gray-800 rounded-2xl">
              <h3 className="text-2xl font-semibold text-blue-900 dark:text-blue-300 mb-6">
                Performance Optimization
              </h3>
              <ul className="space-y-3 text-gray-600 dark:text-gray-400">
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Redis object caching for database optimization
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  PHP 8.2+ with OPcache enabled
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  HTTP/2 and HTTP/3 support
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Global CDN with edge caching
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Image optimization and lazy loading
                </li>
              </ul>
            </div>
            <div className="p-8 bg-white dark:bg-gray-800 rounded-2xl">
              <h3 className="text-2xl font-semibold text-blue-900 dark:text-blue-300 mb-6">
                Security & Compliance
              </h3>
              <ul className="space-y-3 text-gray-600 dark:text-gray-400">
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Web Application Firewall (WAF)
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Real-time malware detection and removal
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Two-factor authentication (2FA)
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Automated vulnerability scanning
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  PCI DSS and GDPR compliance support
                </li>
              </ul>
            </div>
          </div>
        </div>
      </section>

      {/* Pricing Section */}
      <section className="py-24 px-4 sm:px-8 bg-white dark:bg-gray-950">
        <div className="max-w-6xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-blue-900 dark:text-blue-300 mb-12">
            Pricing Plans
          </h2>
          <div className="grid grid-cols-1 md:grid-cols-3 gap-8">
            {plans.map((plan, index) => (
              <div
                key={index}
                className={`p-8 rounded-2xl ${
                  plan.popular
                    ? 'bg-gradient-to-br from-blue-600 to-purple-700 dark:from-blue-900 dark:to-purple-900 text-white shadow-xl scale-105'
                    : 'bg-gray-50 dark:bg-gray-900'
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
                  href={`https://wa.me/263787211325?text=${encodeURIComponent(`Hi! I'm interested in the ${plan.name} WordPress hosting plan...`)}`}
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
            Ready to Supercharge Your WordPress?
          </h2>
          <p className="text-xl text-blue-100 dark:text-blue-200 mb-12">
            Join 50+ businesses across Africa trusting us with their WordPress hosting
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
              Email Sales Team
            </a>
          </div>
        </div>
      </section>
    </div>
  );
}
