import type { Metadata } from 'next';
import { FaHandshake, FaUsers, FaGlobeAfrica, FaWhatsapp, FaEnvelope } from 'react-icons/fa';
import { MdDevices, MdIntegrationInstructions, MdTrendingUp } from 'react-icons/md';
import { generatePageMetadata } from '@/lib/seo-config';

export const metadata: Metadata = generatePageMetadata({
  title: 'Partnership Opportunities',
  description: 'Partner with Slyker Tech as a reseller, agency partner, or technology alliance. Expand your offerings with our enterprise cloud solutions across Africa.',
  url: '/partner'
});

export default function PartnerPage() {
  const whatsappMessage = encodeURIComponent(
    "Hi Slyker Tech! I'm interested in partnership opportunities..."
  );

  const partnershipTypes = [
    {
      title: 'Reseller Program',
      icon: <MdTrendingUp className="w-12 h-12" />,
      description: 'White-label our services under your brand with competitive margins',
      benefits: [
        'Up to 30% commission on recurring revenue',
        'Dedicated account manager and technical support',
        'Marketing materials and co-branding options',
        'Access to partner portal and automation tools'
      ]
    },
    {
      title: 'Agency Partners',
      icon: <FaUsers className="w-12 h-12" />,
      description: 'Collaborate on client projects and expand your service offerings',
      benefits: [
        'Referral bonuses for qualified leads',
        'Joint project opportunities',
        'Technical resources and development support',
        'Priority support for agency clients'
      ]
    },
    {
      title: 'Technology Alliances',
      icon: <MdIntegrationInstructions className="w-12 h-12" />,
      description: 'Integrate your solutions with our platform for mutual growth',
      benefits: [
        'API access and technical documentation',
        'Co-marketing and joint go-to-market strategies',
        'Revenue sharing on integrated solutions',
        'Technical collaboration and support'
      ]
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
            Join our growing network of resellers, agencies, and technology partners driving digital transformation across Africa and beyond
          </p>
        </div>
      </section>

      {/* Why Partner Section */}
      <section className="py-24 px-4 sm:px-8 bg-white dark:bg-gray-950">
        <div className="max-w-6xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-blue-900 dark:text-blue-300 mb-12">
            Why Partner with Slyker Tech
          </h2>
          <div className="grid grid-cols-1 md:grid-cols-3 gap-8">
            <div className="p-8 bg-gray-50 dark:bg-gray-900 rounded-2xl hover:shadow-lg transition-shadow">
              <div className="text-darkgoldenrod dark:text-yellow-400 mb-6">
                <FaGlobeAfrica className="w-12 h-12" />
              </div>
              <h3 className="text-2xl font-semibold text-blue-900 dark:text-blue-300 mb-4">
                Africa-Wide Reach
              </h3>
              <p className="text-gray-600 dark:text-gray-400">
                Established presence across 8+ African countries with proven expertise in local markets and regulatory requirements
              </p>
            </div>

            <div className="p-8 bg-gray-50 dark:bg-gray-900 rounded-2xl hover:shadow-lg transition-shadow">
              <div className="text-darkgoldenrod dark:text-yellow-400 mb-6">
                <MdDevices className="w-12 h-12" />
              </div>
              <h3 className="text-2xl font-semibold text-blue-900 dark:text-blue-300 mb-4">
                Comprehensive Solutions
              </h3>
              <p className="text-gray-600 dark:text-gray-400">
                Full suite of cloud hosting, fintech, and development services with enterprise-grade reliability and support
              </p>
            </div>

            <div className="p-8 bg-gray-50 dark:bg-gray-900 rounded-2xl hover:shadow-lg transition-shadow">
              <div className="text-darkgoldenrod dark:text-yellow-400 mb-6">
                <FaHandshake className="w-12 h-12" />
              </div>
              <h3 className="text-2xl font-semibold text-blue-900 dark:text-blue-300 mb-4">
                Proven Track Record
              </h3>
              <p className="text-gray-600 dark:text-gray-400">
                50+ active enterprise clients, 100+ successful deployments, and 99.9% uptime SLA across all services
              </p>
            </div>
          </div>
        </div>
      </section>

      {/* Partnership Types */}
      <section className="py-24 px-4 sm:px-8 bg-gray-50 dark:bg-gray-900">
        <div className="max-w-6xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-darkgoldenrod dark:text-yellow-400 mb-12">
            Partnership Programs
          </h2>
          <div className="space-y-8">
            {partnershipTypes.map((type, index) => (
              <div
                key={index}
                className="p-8 bg-white dark:bg-gray-800 rounded-2xl shadow-sm hover:shadow-md transition-shadow"
              >
                <div className="flex flex-col md:flex-row items-start gap-6">
                  <div className="text-darkgoldenrod dark:text-yellow-400">
                    {type.icon}
                  </div>
                  <div className="flex-1">
                    <h3 className="text-2xl font-semibold text-blue-900 dark:text-blue-300 mb-3">
                      {type.title}
                    </h3>
                    <p className="text-gray-600 dark:text-gray-400 mb-6">
                      {type.description}
                    </p>
                    <div className="space-y-3">
                      {type.benefits.map((benefit, i) => (
                        <div key={i} className="flex items-start gap-3">
                          <span className="text-darkgoldenrod dark:text-yellow-400 mt-1">✓</span>
                          <span className="text-gray-600 dark:text-gray-400">{benefit}</span>
                        </div>
                      ))}
                    </div>
                  </div>
                </div>
              </div>
            ))}
          </div>
        </div>
      </section>

      {/* Partner Success Stories */}
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

      {/* Partner Requirements */}
      <section className="py-24 px-4 sm:px-8 bg-gray-50 dark:bg-gray-900">
        <div className="max-w-4xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-blue-900 dark:text-blue-300 mb-12">
            Getting Started
          </h2>
          <div className="bg-white dark:bg-gray-800 rounded-2xl p-8 shadow-sm">
            <ol className="space-y-6">
              <li className="flex gap-4">
                <span className="flex-shrink-0 w-10 h-10 bg-darkgoldenrod dark:bg-yellow-400 text-white dark:text-gray-900 rounded-full flex items-center justify-center font-bold">
                  1
                </span>
                <div>
                  <h3 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-2">
                    Initial Contact
                  </h3>
                  <p className="text-gray-600 dark:text-gray-400">
                    Reach out via WhatsApp or email to discuss partnership opportunities and requirements
                  </p>
                </div>
              </li>
              <li className="flex gap-4">
                <span className="flex-shrink-0 w-10 h-10 bg-darkgoldenrod dark:bg-yellow-400 text-white dark:text-gray-900 rounded-full flex items-center justify-center font-bold">
                  2
                </span>
                <div>
                  <h3 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-2">
                    Partnership Agreement
                  </h3>
                  <p className="text-gray-600 dark:text-gray-400">
                    Review and sign our partnership agreement outlining terms, responsibilities, and revenue sharing
                  </p>
                </div>
              </li>
              <li className="flex gap-4">
                <span className="flex-shrink-0 w-10 h-10 bg-darkgoldenrod dark:bg-yellow-400 text-white dark:text-gray-900 rounded-full flex items-center justify-center font-bold">
                  3
                </span>
                <div>
                  <h3 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-2">
                    Onboarding & Training
                  </h3>
                  <p className="text-gray-600 dark:text-gray-400">
                    Complete our partner onboarding program and receive access to resources, tools, and support
                  </p>
                </div>
              </li>
              <li className="flex gap-4">
                <span className="flex-shrink-0 w-10 h-10 bg-darkgoldenrod dark:bg-yellow-400 text-white dark:text-gray-900 rounded-full flex items-center justify-center font-bold">
                  4
                </span>
                <div>
                  <h3 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-2">
                    Launch & Grow
                  </h3>
                  <p className="text-gray-600 dark:text-gray-400">
                    Start selling, referring, or integrating our solutions with ongoing support from our team
                  </p>
                </div>
              </li>
            </ol>
          </div>
        </div>
      </section>

      {/* CTA Section */}
      <section className="py-24 px-4 sm:px-8 bg-gradient-to-r from-blue-600 to-purple-700 dark:from-blue-900 dark:to-purple-900">
        <div className="max-w-4xl mx-auto text-center">
          <h2 className="text-4xl font-bold text-white mb-6">
            Let&apos;s Build Something Great Together
          </h2>
          <p className="text-xl text-blue-100 dark:text-blue-200 mb-12">
            Join our partner ecosystem and unlock new revenue opportunities across Africa
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
              Email Partnership Team
            </a>
          </div>
        </div>
      </section>
    </div>
  );
}
