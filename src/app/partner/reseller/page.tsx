import type { Metadata } from 'next';
import Link from 'next/link';
import { FaWhatsapp, FaEnvelope, FaArrowLeft } from 'react-icons/fa';
import { MdTrendingUp } from 'react-icons/md';
import { generatePageMetadata } from '@/lib/seo-config';

export const metadata: Metadata = generatePageMetadata({
  title: 'Reseller Program',
  description: 'Join our 4-tier reseller program and white-label Slyker Tech Web Services under your brand. Earn up to 30% commission with our Platinum tier.',
  url: '/partner/reseller'
});

export default function ResellerPage() {
  const whatsappMessage = encodeURIComponent(
    "Hi Slyker Tech Web Services! I'm interested in the Reseller Program..."
  );

  const tiers = [
    {
      name: 'Bronze',
      badge: 'Entry Level',
      color: 'from-amber-700 to-amber-800',
      discount: '10%',
      commission: '5%',
      clients: '50',
      apiRate: '1,000',
      features: [
        'Up to 10% discount on services',
        '5% commission on recurring revenue',
        'Support up to 50 clients',
        'API access (1,000 requests/day)',
        'Marketing materials and co-branding options',
        'Email support within 24 hours'
      ]
    },
    {
      name: 'Silver',
      badge: 'Popular',
      color: 'from-gray-400 to-gray-500',
      discount: '15%',
      commission: '7%',
      clients: '100',
      apiRate: '2,500',
      features: [
        'Up to 15% discount on services',
        '7% commission on recurring revenue',
        'Support up to 100 clients',
        'API access (2,500 requests/day)',
        'Priority technical support',
        'Custom branding and webhook integration',
        'Quarterly business reviews'
      ]
    },
    {
      name: 'Gold',
      badge: 'Professional',
      color: 'from-yellow-500 to-yellow-600',
      discount: '20%',
      commission: '10%',
      clients: '200',
      apiRate: '5,000',
      features: [
        'Up to 20% discount on services',
        '10% commission on recurring revenue',
        'Support up to 200 clients',
        'API access (5,000 requests/day)',
        'Dedicated account manager',
        'Advanced automation tools',
        'Co-marketing opportunities',
        'Monthly business reviews'
      ]
    },
    {
      name: 'Platinum',
      badge: 'Enterprise',
      color: 'from-blue-600 to-blue-700',
      discount: '30%',
      commission: '15%',
      clients: 'Unlimited',
      apiRate: '10,000+',
      features: [
        'Up to 30% discount on services',
        '15% commission on recurring revenue',
        'Unlimited clients',
        'API access (10,000+ requests/day)',
        'Priority support and SLA guarantees',
        'Full white-label solution',
        'Custom integrations and development',
        'Dedicated technical team',
        'Weekly strategic planning sessions'
      ]
    }
  ];

  return (
    <div className="relative z-10">
      {/* Back Button */}
      <div className="py-4 px-4 sm:px-8 bg-gray-50 dark:bg-gray-900">
        <div className="max-w-7xl mx-auto">
          <Link 
            href="/partner"
            className="inline-flex items-center gap-2 text-blue-600 dark:text-blue-400 hover:text-darkgoldenrod dark:hover:text-yellow-400 transition-colors"
          >
            <FaArrowLeft className="w-4 h-4" />
            <span>Back to Partnership Types</span>
          </Link>
        </div>
      </div>

      {/* Hero Section */}
      <section className="py-28 px-4 sm:px-8 md:px-16 lg:px-24 text-center bg-gradient-to-b from-blue-50 to-transparent dark:from-blue-950/50">
        <div className="max-w-5xl mx-auto">
          <h1 className="text-5xl sm:text-6xl md:text-7xl font-extrabold tracking-tight text-blue-900 dark:text-blue-300 leading-tight">
            Reseller <span className="text-darkgoldenrod dark:text-yellow-400">Program</span>
          </h1>
          <p className="mt-8 text-lg sm:text-xl text-gray-700 dark:text-gray-300 max-w-3xl mx-auto">
            White-label our enterprise cloud solutions under your brand. Choose from 4 tiers designed to grow with your business.
          </p>
        </div>
      </section>

      {/* Tier Comparison */}
      <section className="py-24 px-4 sm:px-8 bg-white dark:bg-gray-950">
        <div className="max-w-7xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-blue-900 dark:text-blue-300 mb-4">
            Choose Your Tier
          </h2>
          <p className="text-center text-lg text-gray-600 dark:text-gray-400 mb-16 max-w-3xl mx-auto">
            All tiers include access to our partner portal, training resources, and dedicated partnership team
          </p>

          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-8">
            {tiers.map((tier, index) => (
              <div
                key={index}
                className="relative bg-gray-50 dark:bg-gray-900 rounded-2xl p-8 hover:shadow-2xl transition-all duration-300 border-2 border-transparent hover:border-darkgoldenrod dark:hover:border-yellow-400"
              >
                {tier.badge && (
                  <div className="absolute -top-3 left-1/2 transform -translate-x-1/2">
                    <span className="px-4 py-1 bg-darkgoldenrod dark:bg-yellow-400 text-white dark:text-gray-900 rounded-full text-xs font-semibold">
                      {tier.badge}
                    </span>
                  </div>
                )}

                <div className="text-center mb-6">
                  <div className={`inline-flex items-center justify-center w-16 h-16 rounded-full bg-gradient-to-br ${tier.color} mb-4`}>
                    <MdTrendingUp className="w-8 h-8 text-white" />
                  </div>
                  <h3 className="text-2xl font-bold text-blue-900 dark:text-blue-300 mb-2">
                    {tier.name}
                  </h3>
                </div>

                <div className="mb-6 pb-6 border-b border-gray-200 dark:border-gray-700">
                  <div className="grid grid-cols-2 gap-4 text-center">
                    <div>
                      <div className="text-2xl font-bold text-darkgoldenrod dark:text-yellow-400">
                        {tier.discount}
                      </div>
                      <div className="text-xs text-gray-600 dark:text-gray-400">Discount</div>
                    </div>
                    <div>
                      <div className="text-2xl font-bold text-darkgoldenrod dark:text-yellow-400">
                        {tier.commission}
                      </div>
                      <div className="text-xs text-gray-600 dark:text-gray-400">Commission</div>
                    </div>
                    <div>
                      <div className="text-lg font-bold text-blue-600 dark:text-blue-400">
                        {tier.clients}
                      </div>
                      <div className="text-xs text-gray-600 dark:text-gray-400">Clients</div>
                    </div>
                    <div>
                      <div className="text-lg font-bold text-blue-600 dark:text-blue-400">
                        {tier.apiRate}
                      </div>
                      <div className="text-xs text-gray-600 dark:text-gray-400">API/day</div>
                    </div>
                  </div>
                </div>

                <ul className="space-y-3 mb-8">
                  {tier.features.map((feature, i) => (
                    <li key={i} className="flex items-start gap-2 text-sm">
                      <span className="text-darkgoldenrod dark:text-yellow-400 mt-0.5">✓</span>
                      <span className="text-gray-600 dark:text-gray-400">{feature}</span>
                    </li>
                  ))}
                </ul>

                <a
                  href={`https://wa.me/263787211325?text=${encodeURIComponent(`Hi! I'm interested in the ${tier.name} tier of the Reseller Program...`)}`}
                  target="_blank"
                  rel="noopener noreferrer"
                  className="block w-full text-center py-3 bg-blue-600 hover:bg-blue-700 dark:bg-blue-700 dark:hover:bg-blue-600 text-white rounded-lg font-semibold transition-colors"
                >
                  Get Started
                </a>
              </div>
            ))}
          </div>
        </div>
      </section>

      {/* How It Works */}
      <section className="py-24 px-4 sm:px-8 bg-gray-50 dark:bg-gray-900">
        <div className="max-w-4xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-darkgoldenrod dark:text-yellow-400 mb-12">
            How It Works
          </h2>
          <div className="space-y-6">
            <div className="flex gap-4">
              <div className="flex-shrink-0 w-10 h-10 bg-darkgoldenrod dark:bg-yellow-400 text-white dark:text-gray-900 rounded-full flex items-center justify-center font-bold">
                1
              </div>
              <div>
                <h3 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-2">
                  Choose Your Tier
                </h3>
                <p className="text-gray-600 dark:text-gray-400">
                  Select the tier that matches your business needs and growth ambitions
                </p>
              </div>
            </div>
            <div className="flex gap-4">
              <div className="flex-shrink-0 w-10 h-10 bg-darkgoldenrod dark:bg-yellow-400 text-white dark:text-gray-900 rounded-full flex items-center justify-center font-bold">
                2
              </div>
              <div>
                <h3 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-2">
                  Sign Agreement
                </h3>
                <p className="text-gray-600 dark:text-gray-400">
                  Review and sign the partnership agreement with clear terms and commission structure
                </p>
              </div>
            </div>
            <div className="flex gap-4">
              <div className="flex-shrink-0 w-10 h-10 bg-darkgoldenrod dark:bg-yellow-400 text-white dark:text-gray-900 rounded-full flex items-center justify-center font-bold">
                3
              </div>
              <div>
                <h3 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-2">
                  Get Onboarded
                </h3>
                <p className="text-gray-600 dark:text-gray-400">
                  Access partner portal, API credentials, marketing materials, and complete training
                </p>
              </div>
            </div>
            <div className="flex gap-4">
              <div className="flex-shrink-0 w-10 h-10 bg-darkgoldenrod dark:bg-yellow-400 text-white dark:text-gray-900 rounded-full flex items-center justify-center font-bold">
                4
              </div>
              <div>
                <h3 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-2">
                  Start Reselling
                </h3>
                <p className="text-gray-600 dark:text-gray-400">
                  Begin provisioning services for your clients and earning commissions on every sale
                </p>
              </div>
            </div>
          </div>
        </div>
      </section>

      {/* CTA Section */}
      <section className="py-24 px-4 sm:px-8 bg-gradient-to-br from-blue-600 via-blue-700 to-blue-800 dark:from-blue-800 dark:via-blue-900 dark:to-gray-900">
        <div className="max-w-4xl mx-auto text-center">
          <h2 className="text-4xl font-bold text-white mb-6">
            Ready to Become a Reseller?
          </h2>
          <p className="text-xl text-blue-100 dark:text-blue-200 mb-12">
            Join our network of successful resellers across Africa
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
              Email Reseller Team
            </a>
          </div>
        </div>
      </section>
    </div>
  );
}
