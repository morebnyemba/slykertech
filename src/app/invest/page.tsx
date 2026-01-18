import type { Metadata } from 'next';
import { FaChartLine, FaHandshake, FaGlobeAfrica, FaWhatsapp, FaEnvelope } from 'react-icons/fa';
import { MdTrendingUp, MdSecurity, MdCloud } from 'react-icons/md';
import { generatePageMetadata } from '@/lib/seo-config';

export const metadata: Metadata = generatePageMetadata({
  title: 'Investment Opportunities',
  description: 'Invest in Africa\'s digital transformation. Join Slyker Tech in building enterprise cloud solutions and driving technological innovation across the continent.',
  url: '/invest'
});

export default function InvestPage() {
  const whatsappMessage = encodeURIComponent(
    "Hi Slyker Tech! I'm interested in investment opportunities..."
  );

  const focusAreas = [
    {
      title: 'Cloud Infrastructure',
      icon: <MdCloud className="w-12 h-12" />,
      description: 'Expanding enterprise-grade hosting and cloud solutions across African markets',
      metrics: '50+ active clients, 99.9% uptime'
    },
    {
      title: 'Fintech Solutions',
      icon: <MdSecurity className="w-12 h-12" />,
      description: 'Building payment gateways, mobile money integrations, and digital wallet platforms',
      metrics: 'Multi-currency support, API-first architecture'
    },
    {
      title: 'Digital Services',
      icon: <MdTrendingUp className="w-12 h-12" />,
      description: 'Full-stack development, e-commerce platforms, and custom software solutions',
      metrics: '100+ projects deployed, 8 countries served'
    }
  ];

  return (
    <div className="relative z-10">
      {/* Hero Section */}
      <section className="py-28 px-4 sm:px-8 md:px-16 lg:px-24 text-center bg-gradient-to-b from-blue-50 to-transparent dark:from-blue-950/50">
        <div className="max-w-5xl mx-auto">
          <h1 className="text-5xl sm:text-6xl md:text-7xl font-extrabold tracking-tight text-blue-900 dark:text-blue-300 leading-tight">
            Invest in <span className="text-darkgoldenrod dark:text-yellow-400">Africa&apos;s Digital Future</span>
          </h1>
          <p className="mt-8 text-lg sm:text-xl text-gray-700 dark:text-gray-300 max-w-3xl mx-auto">
            Join us in powering digital transformation across Africa and beyond with enterprise cloud solutions, fintech innovation, and cutting-edge technology services
          </p>
        </div>
      </section>

      {/* Why Invest Section */}
      <section className="py-24 px-4 sm:px-8 bg-white dark:bg-gray-950">
        <div className="max-w-6xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-blue-900 dark:text-blue-300 mb-12">
            Why Invest in Slyker Tech
          </h2>
          <div className="grid grid-cols-1 md:grid-cols-3 gap-8">
            <div className="p-8 bg-gray-50 dark:bg-gray-900 rounded-2xl hover:shadow-lg transition-shadow">
              <div className="text-darkgoldenrod dark:text-yellow-400 mb-6">
                <FaChartLine className="w-12 h-12" />
              </div>
              <h3 className="text-2xl font-semibold text-blue-900 dark:text-blue-300 mb-4">
                Proven Growth
              </h3>
              <p className="text-gray-600 dark:text-gray-400">
                From local startup to pan-African tech provider, serving 8+ countries with 50+ active enterprise clients and 100+ successful deployments
              </p>
            </div>

            <div className="p-8 bg-gray-50 dark:bg-gray-900 rounded-2xl hover:shadow-lg transition-shadow">
              <div className="text-darkgoldenrod dark:text-yellow-400 mb-6">
                <FaHandshake className="w-12 h-12" />
              </div>
              <h3 className="text-2xl font-semibold text-blue-900 dark:text-blue-300 mb-4">
                Strategic Partnerships
              </h3>
              <p className="text-gray-600 dark:text-gray-400">
                Strong relationships with DigitalOcean, AWS, and African payment providers. Positioned for exponential scaling
              </p>
            </div>

            <div className="p-8 bg-gray-50 dark:bg-gray-900 rounded-2xl hover:shadow-lg transition-shadow">
              <div className="text-darkgoldenrod dark:text-yellow-400 mb-6">
                <FaGlobeAfrica className="w-12 h-12" />
              </div>
              <h3 className="text-2xl font-semibold text-blue-900 dark:text-blue-300 mb-4">
                African Market Focus
              </h3>
              <p className="text-gray-600 dark:text-gray-400">
                Addressing real challenges with localized solutions. Deep understanding of African tech infrastructure and payment ecosystems
              </p>
            </div>
          </div>
        </div>
      </section>

      {/* Focus Areas */}
      <section className="py-24 px-4 sm:px-8 bg-gray-50 dark:bg-gray-900">
        <div className="max-w-6xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-darkgoldenrod dark:text-yellow-400 mb-12">
            Investment Focus Areas
          </h2>
          <div className="space-y-8">
            {focusAreas.map((area, index) => (
              <div
                key={index}
                className="p-8 bg-white dark:bg-gray-800 rounded-2xl shadow-sm hover:shadow-md transition-shadow"
              >
                <div className="flex flex-col md:flex-row items-start gap-6">
                  <div className="text-darkgoldenrod dark:text-yellow-400">
                    {area.icon}
                  </div>
                  <div className="flex-1">
                    <h3 className="text-2xl font-semibold text-blue-900 dark:text-blue-300 mb-3">
                      {area.title}
                    </h3>
                    <p className="text-gray-600 dark:text-gray-400 mb-3">
                      {area.description}
                    </p>
                    <div className="inline-block px-4 py-2 bg-blue-50 dark:bg-gray-700 rounded-lg">
                      <p className="text-sm font-medium text-blue-900 dark:text-blue-300">
                        {area.metrics}
                      </p>
                    </div>
                  </div>
                </div>
              </div>
            ))}
          </div>
        </div>
      </section>

      {/* Market Opportunity */}
      <section className="py-24 px-4 sm:px-8 bg-white dark:bg-gray-950">
        <div className="max-w-6xl mx-auto">
          <div className="bg-gradient-to-br from-blue-50 to-purple-50 dark:from-gray-800 dark:to-blue-900/50 rounded-3xl p-12">
            <h2 className="text-4xl font-bold text-blue-900 dark:text-blue-300 mb-8 text-center">
              The African Tech Opportunity
            </h2>
            <div className="grid md:grid-cols-2 gap-8">
              <div className="p-6 bg-white dark:bg-gray-900 rounded-xl">
                <h3 className="text-xl font-semibold text-darkgoldenrod dark:text-yellow-400 mb-4">
                  Market Growth
                </h3>
                <ul className="space-y-3 text-gray-600 dark:text-gray-400">
                  <li className="flex items-start gap-2">
                    <span className="text-darkgoldenrod dark:text-yellow-400">▹</span>
                    Africa&apos;s digital economy projected to reach $180B by 2025
                  </li>
                  <li className="flex items-start gap-2">
                    <span className="text-darkgoldenrod dark:text-yellow-400">▹</span>
                    Growing demand for enterprise cloud solutions
                  </li>
                  <li className="flex items-start gap-2">
                    <span className="text-darkgoldenrod dark:text-yellow-400">▹</span>
                    Mobile money users exceeding 500M across the continent
                  </li>
                </ul>
              </div>
              <div className="p-6 bg-white dark:bg-gray-900 rounded-xl">
                <h3 className="text-xl font-semibold text-darkgoldenrod dark:text-yellow-400 mb-4">
                  Our Competitive Edge
                </h3>
                <ul className="space-y-3 text-gray-600 dark:text-gray-400">
                  <li className="flex items-start gap-2">
                    <span className="text-darkgoldenrod dark:text-yellow-400">▹</span>
                    Local expertise with global standards
                  </li>
                  <li className="flex items-start gap-2">
                    <span className="text-darkgoldenrod dark:text-yellow-400">▹</span>
                    Proven track record across multiple African markets
                  </li>
                  <li className="flex items-start gap-2">
                    <span className="text-darkgoldenrod dark:text-yellow-400">▹</span>
                    Scalable infrastructure and repeatable solutions
                  </li>
                </ul>
              </div>
            </div>
          </div>
        </div>
      </section>

      {/* CTA Section */}
      <section className="py-24 px-4 sm:px-8 bg-gradient-to-r from-blue-600 to-purple-700 dark:from-blue-900 dark:to-purple-900">
        <div className="max-w-4xl mx-auto text-center">
          <h2 className="text-4xl font-bold text-white mb-6">
            Ready to Partner with Us?
          </h2>
          <p className="text-xl text-blue-100 dark:text-blue-200 mb-12">
            Let&apos;s discuss how we can drive digital transformation across Africa together
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
              Email Investment Team
            </a>
          </div>
        </div>
      </section>
    </div>
  );
}
