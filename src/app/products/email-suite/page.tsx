import type { Metadata } from 'next';
import { FaEnvelope, FaCalendar, FaUsers, FaWhatsapp } from 'react-icons/fa';
import { MdEmail, MdStorage, MdSecurity, MdVideoCall } from 'react-icons/md';
import { generatePageMetadata } from '@/lib/seo-config';

export const metadata: Metadata = generatePageMetadata({
  title: 'Email & Collaboration Suite',
  description: 'Professional business email with calendar, contacts, and collaboration tools. Custom domain email hosting across Africa with 99.9% uptime and advanced security.',
  url: '/products/email-suite'
});

export default function EmailSuitePage() {
  const whatsappMessage = encodeURIComponent(
    "Hi Slyker Tech! I'm interested in the Email & Collaboration Suite..."
  );

  const features = [
    {
      icon: <MdEmail className="w-10 h-10" />,
      title: 'Professional Email',
      description: 'Custom domain email addresses (you@yourbusiness.com) with webmail and mobile access'
    },
    {
      icon: <FaCalendar className="w-10 h-10" />,
      title: 'Shared Calendars',
      description: 'Team calendars with scheduling, reminders, and meeting coordination'
    },
    {
      icon: <FaUsers className="w-10 h-10" />,
      title: 'Contact Management',
      description: 'Centralized contact database with sharing and synchronization'
    },
    {
      icon: <MdStorage className="w-10 h-10" />,
      title: 'Cloud Storage',
      description: 'Secure file storage and sharing with team collaboration features'
    },
    {
      icon: <MdVideoCall className="w-10 h-10" />,
      title: 'Video Conferencing',
      description: 'Built-in video meetings and screen sharing for remote teams'
    },
    {
      icon: <MdSecurity className="w-10 h-10" />,
      title: 'Advanced Security',
      description: 'Spam filtering, virus protection, and two-factor authentication'
    }
  ];

  const plans = [
    {
      name: 'Basic',
      price: '$5',
      period: '/user/month',
      features: [
        '10GB Email Storage',
        'Custom Domain Email',
        'Webmail & Mobile Access',
        'Calendar & Contacts',
        'Spam Protection',
        'Email Support'
      ]
    },
    {
      name: 'Professional',
      price: '$12',
      period: '/user/month',
      popular: true,
      features: [
        '50GB Email Storage',
        'Custom Domain Email',
        'Advanced Webmail',
        'Shared Calendars',
        '100GB Cloud Storage',
        'Video Conferencing',
        'Priority Support',
        'Mobile Apps'
      ]
    },
    {
      name: 'Enterprise',
      price: '$25',
      period: '/user/month',
      features: [
        'Unlimited Email Storage',
        'Custom Domain Email',
        'Full Collaboration Suite',
        'Advanced Security',
        'Unlimited Cloud Storage',
        'Unlimited Video Meetings',
        '24/7 Support',
        'Admin Controls',
        'Compliance Features'
      ]
    }
  ];

  return (
    <div className="relative z-10">
      {/* Hero Section */}
      <section className="py-28 px-4 sm:px-8 md:px-16 lg:px-24 text-center bg-gradient-to-b from-blue-50 to-transparent dark:from-blue-950/50">
        <div className="max-w-5xl mx-auto">
          <FaEnvelope className="w-20 h-20 mx-auto text-darkgoldenrod dark:text-yellow-400 mb-8" />
          <h1 className="text-5xl sm:text-6xl md:text-7xl font-extrabold tracking-tight text-blue-900 dark:text-blue-300 leading-tight">
            Email & <span className="text-darkgoldenrod dark:text-yellow-400">Collaboration</span> Suite
          </h1>
          <p className="mt-8 text-lg sm:text-xl text-gray-700 dark:text-gray-300 max-w-3xl mx-auto">
            Professional business email, calendars, and collaboration tools to power your team across Africa
          </p>
        </div>
      </section>

      {/* Features Section */}
      <section className="py-24 px-4 sm:px-8 bg-white dark:bg-gray-950">
        <div className="max-w-6xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-blue-900 dark:text-blue-300 mb-12">
            Everything Your Team Needs
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

      {/* Benefits */}
      <section className="py-24 px-4 sm:px-8 bg-gray-50 dark:bg-gray-900">
        <div className="max-w-6xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-darkgoldenrod dark:text-yellow-400 mb-12">
            Why Choose Our Email Suite
          </h2>
          <div className="grid md:grid-cols-2 gap-8">
            <div className="p-8 bg-white dark:bg-gray-800 rounded-2xl">
              <h3 className="text-2xl font-semibold text-blue-900 dark:text-blue-300 mb-6">
                Professional & Reliable
              </h3>
              <ul className="space-y-3 text-gray-600 dark:text-gray-400">
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  99.9% uptime guarantee with redundant infrastructure
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Custom domain email for professional branding
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Access from any device—desktop, mobile, or web
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Optimized for low-bandwidth African networks
                </li>
              </ul>
            </div>
            <div className="p-8 bg-white dark:bg-gray-800 rounded-2xl">
              <h3 className="text-2xl font-semibold text-blue-900 dark:text-blue-300 mb-6">
                Secure & Compliant
              </h3>
              <ul className="space-y-3 text-gray-600 dark:text-gray-400">
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Advanced spam and virus protection
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Two-factor authentication for enhanced security
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Encrypted email transmission (TLS/SSL)
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  GDPR and data protection compliance
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
            Simple Per-User Pricing
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
                  href={`https://wa.me/263787211325?text=${encodeURIComponent(`Hi! I'm interested in the ${plan.name} Email Suite plan...`)}`}
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
            Get Professional Email for Your Business
          </h2>
          <p className="text-xl text-blue-100 dark:text-blue-200 mb-12">
            Trusted by businesses across Africa for reliable, secure email and collaboration
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
