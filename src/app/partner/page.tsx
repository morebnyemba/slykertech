import type { Metadata } from 'next';
import Link from 'next/link';
import { FaHandshake, FaUsers, FaWhatsapp, FaEnvelope, FaArrowRight } from 'react-icons/fa';
import { MdDevices, MdIntegrationInstructions, MdTrendingUp } from 'react-icons/md';
import { generatePageMetadata } from '@/lib/seo-config';

export const metadata: Metadata = generatePageMetadata({
  title: 'Partnership Opportunities',
  description: 'Partner with Slyker Tech Web Services as a reseller, agency partner, or technology alliance. Expand your offerings with our enterprise cloud solutions across Africa.',
  url: '/partner'
});

export default function PartnerPage() {
  const whatsappMessage = encodeURIComponent(
    "Hi Slyker Tech Web Services! I'm interested in partnership opportunities..."
  );

  const partnershipTypes = [
    {
      title: 'Reseller Program',
      icon: <MdTrendingUp className="w-16 h-16" />,
      description: 'White-label our services under your brand with competitive margins and tiered benefits',
      benefits: [
        '4 tier levels (Bronze, Silver, Gold, Platinum)',
        'Up to 30% commission on recurring revenue',
        'Dedicated partner portal and API access',
        'Full white-label capabilities'
      ],
      href: '/partner/reseller',
      badge: 'Most Popular'
    },
    {
      title: 'Agency Partners',
      icon: <FaUsers className="w-16 h-16" />,
      description: 'Collaborate on client projects and expand your service offerings',
      benefits: [
        'Referral bonuses for qualified leads',
        'Joint project opportunities',
        'Technical resources and support',
        'Priority support for agency clients'
      ],
      href: '/partner/agency',
      badge: null
    },
    {
      title: 'Technology Alliances',
      icon: <MdIntegrationInstructions className="w-16 h-16" />,
      description: 'Integrate your solutions with our platform for mutual growth',
      benefits: [
        'API access and documentation',
        'Co-marketing opportunities',
        'Revenue sharing on integrations',
        'Technical collaboration'
      ],
      href: '/partner/technology',
      badge: null
    }
  ];

  return (
    <div className="relative z-10">
      {/* Hero Section */}
      <section className="py-28 px-4 sm:px-8 md:px-16 lg:px-24 text-center bg-gradient-to-b from-blue-50 to-transparent dark:from-blue-950/50">
        <div className="max-w-5xl mx-auto">
          <h1 className="text-5xl sm:text-6xl md:text-7xl font-extrabold tracking-tight text-blue-900 dark:text-blue-300 leading-tight">
            Partner for <span className="text-darkgoldenrod dark:text-yellow-400">Mutual Success</span>
          </h1>
          <p className="mt-8 text-lg sm:text-xl text-gray-700 dark:text-gray-300 max-w-3xl mx-auto">
            Join our growing network of partners driving digital transformation across Africa and beyond
          </p>
        </div>
      </section>

      {/* Partnership Types */}
      <section className="py-24 px-4 sm:px-8 bg-white dark:bg-gray-950">
        <div className="max-w-7xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-blue-900 dark:text-blue-300 mb-4">
            Choose Your Partnership Type
          </h2>
          <p className="text-center text-lg text-gray-600 dark:text-gray-400 mb-16 max-w-3xl mx-auto">
            Select the partnership model that best fits your business goals and expertise
          </p>
          
          <div className="grid grid-cols-1 md:grid-cols-3 gap-8">
            {partnershipTypes.map((type, index) => (
              <Link
                key={index}
                href={type.href}
                className="group relative p-8 bg-gray-50 dark:bg-gray-900 rounded-2xl hover:shadow-xl transition-all duration-300 border-2 border-transparent hover:border-darkgoldenrod dark:hover:border-yellow-400"
              >
                {type.badge && (
                  <div className="absolute -top-3 -right-3">
                    <span className="px-4 py-1 bg-darkgoldenrod dark:bg-yellow-400 text-white dark:text-gray-900 rounded-full text-sm font-semibold">
                      {type.badge}
                    </span>
                  </div>
                )}
                
                <div className="text-darkgoldenrod dark:text-yellow-400 mb-6 flex justify-center">
                  {type.icon}
                </div>
                
                <h3 className="text-2xl font-bold text-blue-900 dark:text-blue-300 mb-4 text-center">
                  {type.title}
                </h3>
                
                <p className="text-gray-600 dark:text-gray-400 mb-6 text-center">
                  {type.description}
                </p>
                
                <ul className="space-y-3 mb-8">
                  {type.benefits.map((benefit, i) => (
                    <li key={i} className="flex items-start gap-2">
                      <span className="text-darkgoldenrod dark:text-yellow-400 mt-1">✓</span>
                      <span className="text-gray-600 dark:text-gray-400 text-sm">{benefit}</span>
                    </li>
                  ))}
                </ul>
                
                <div className="flex items-center justify-center gap-2 text-blue-600 dark:text-blue-400 font-semibold group-hover:gap-4 transition-all">
                  <span>Learn More</span>
                  <FaArrowRight className="w-4 h-4" />
                </div>
              </Link>
            ))}
          </div>
        </div>
      </section>

      {/* Why Partner Section */}
      <section className="py-24 px-4 sm:px-8 bg-gray-50 dark:bg-gray-900">
        <div className="max-w-6xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-blue-900 dark:text-blue-300 mb-12">
            Why Partner with Slyker Tech Web Services
          </h2>
          <div className="grid grid-cols-1 md:grid-cols-3 gap-8">
            <div className="p-8 bg-white dark:bg-gray-800 rounded-2xl text-center">
              <div className="text-darkgoldenrod dark:text-yellow-400 mb-6 flex justify-center">
                <MdDevices className="w-12 h-12" />
              </div>
              <h3 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-4">
                Africa-Wide Reach
              </h3>
              <p className="text-gray-600 dark:text-gray-400">
                Established presence across 8+ African countries with proven expertise in local markets
              </p>
            </div>

            <div className="p-8 bg-white dark:bg-gray-800 rounded-2xl text-center">
              <div className="text-darkgoldenrod dark:text-yellow-400 mb-6 flex justify-center">
                <FaHandshake className="w-12 h-12" />
              </div>
              <h3 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-4">
                Comprehensive Solutions
              </h3>
              <p className="text-gray-600 dark:text-gray-400">
                Full suite of cloud hosting, fintech, and development services with enterprise-grade reliability
              </p>
            </div>

            <div className="p-8 bg-white dark:bg-gray-800 rounded-2xl text-center">
              <div className="text-darkgoldenrod dark:text-yellow-400 mb-6 flex justify-center">
                <FaUsers className="w-12 h-12" />
              </div>
              <h3 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-4">
                Proven Track Record
              </h3>
              <p className="text-gray-600 dark:text-gray-400">
                50+ active enterprise clients, 100+ successful deployments, and 99.9% uptime SLA
              </p>
            </div>
          </div>
        </div>
      </section>

      {/* Partner Stats */}
      <section className="py-24 px-4 sm:px-8 bg-white dark:bg-gray-950">
        <div className="max-w-6xl mx-auto">
          <div className="bg-gradient-to-br from-blue-50 to-purple-50 dark:from-gray-800 dark:to-blue-900/50 rounded-3xl p-12">
            <h2 className="text-4xl font-bold text-blue-900 dark:text-blue-300 mb-8 text-center">
              Partner Success Metrics
            </h2>
            <div className="grid md:grid-cols-3 gap-8">
              <div className="p-6 bg-white dark:bg-gray-900 rounded-xl text-center">
                <div className="text-4xl font-bold text-darkgoldenrod dark:text-yellow-400 mb-2">
                  15+
                </div>
                <p className="text-gray-600 dark:text-gray-400">Active Partners</p>
              </div>
              <div className="p-6 bg-white dark:bg-gray-900 rounded-xl text-center">
                <div className="text-4xl font-bold text-darkgoldenrod dark:text-yellow-400 mb-2">
                  $50K+
                </div>
                <p className="text-gray-600 dark:text-gray-400">Average Annual Revenue</p>
              </div>
              <div className="p-6 bg-white dark:bg-gray-900 rounded-xl text-center">
                <div className="text-4xl font-bold text-darkgoldenrod dark:text-yellow-400 mb-2">
                  95%
                </div>
                <p className="text-gray-600 dark:text-gray-400">Partner Satisfaction Rate</p>
              </div>
            </div>
          </div>
        </div>
      </section>

      {/* CTA Section */}
      <section className="py-24 px-4 sm:px-8 bg-gradient-to-br from-blue-600 via-blue-700 to-blue-800 dark:from-blue-800 dark:via-blue-900 dark:to-gray-900">
        <div className="max-w-4xl mx-auto text-center">
          <h2 className="text-4xl font-bold text-white mb-6">
            Ready to Get Started?
          </h2>
          <p className="text-xl text-blue-100 dark:text-blue-200 mb-12">
            Let&apos;s discuss how we can work together to achieve mutual success
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
              href="mailto:partners@slykertech.co.zw"
              className="inline-flex items-center justify-center gap-3 px-8 py-4 bg-darkgoldenrod hover:bg-yellow-600 dark:bg-yellow-400 dark:hover:bg-yellow-500 text-white dark:text-gray-900 rounded-lg font-semibold transition-colors shadow-lg"
            >
              <FaEnvelope className="w-5 h-5" />
              Email Partnership Team
            </a>
          </div>
        </div>
      </section>
    </div>
  );
}
