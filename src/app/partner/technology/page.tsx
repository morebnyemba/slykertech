import type { Metadata } from 'next';
import Link from 'next/link';
import { FaWhatsapp, FaEnvelope, FaArrowLeft, FaCode, FaPlug } from 'react-icons/fa';
import { MdIntegrationInstructions } from 'react-icons/md';
import { generatePageMetadata } from '@/lib/seo-config';

export const metadata: Metadata = generatePageMetadata({
  title: 'Technology Alliance Program',
  description: 'Integrate your technology with Slyker Tech Web Services platform. API access and revenue sharing opportunities.',
  url: '/partner/technology'
});

export default function TechnologyPage() {
  const whatsappMessage = encodeURIComponent(
    "Hi Slyker Tech Web Services! I'm interested in the Technology Alliance Program..."
  );

  const benefits = [
    {
      title: 'API Access',
      description: 'Full API documentation and developer support for seamless integration',
      icon: <FaCode className="w-12 h-12" />
    },
    {
      title: 'Co-Marketing',
      description: 'Joint marketing campaigns and go-to-market strategies',
      icon: <MdIntegrationInstructions className="w-12 h-12" />
    },
    {
      title: 'Revenue Sharing',
      description: '15% revenue share on integrated solutions and joint offerings',
      icon: <FaPlug className="w-12 h-12" />
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
      <section className="py-28 px-4 sm:px-8 text-center bg-gradient-to-b from-blue-50 to-transparent dark:from-blue-950/50">
        <div className="max-w-5xl mx-auto">
          <h1 className="text-5xl sm:text-6xl md:text-7xl font-extrabold tracking-tight text-blue-900 dark:text-blue-300 leading-tight">
            Technology <span className="text-darkgoldenrod dark:text-yellow-400">Alliance</span>
          </h1>
          <p className="mt-8 text-lg sm:text-xl text-gray-700 dark:text-gray-300 max-w-3xl mx-auto">
            Integrate your solutions with our platform for mutual growth and success
          </p>
        </div>
      </section>

      {/* Benefits */}
      <section className="py-24 px-4 sm:px-8 bg-white dark:bg-gray-950">
        <div className="max-w-6xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-blue-900 dark:text-blue-300 mb-16">
            Technology Alliance Benefits
          </h2>
          <div className="grid md:grid-cols-3 gap-8">
            {benefits.map((benefit, index) => (
              <div key={index} className="p-8 bg-gray-50 dark:bg-gray-900 rounded-2xl text-center">
                <div className="text-darkgoldenrod dark:text-yellow-400 mb-6 flex justify-center">
                  {benefit.icon}
                </div>
                <h3 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-4">
                  {benefit.title}
                </h3>
                <p className="text-gray-600 dark:text-gray-400">
                  {benefit.description}
                </p>
              </div>
            ))}
          </div>
        </div>
      </section>

      {/* CTA */}
      <section className="py-24 px-4 sm:px-8 bg-gradient-to-br from-blue-600 via-blue-700 to-blue-800 dark:from-blue-800 dark:via-blue-900 dark:to-gray-900">
        <div className="max-w-4xl mx-auto text-center">
          <h2 className="text-4xl font-bold text-white mb-6">
            Build Technology Partnerships
          </h2>
          <p className="text-xl text-blue-100 dark:text-blue-200 mb-12">
            Let&apos;s explore integration opportunities
          </p>
          <div className="flex flex-col sm:flex-row gap-6 justify-center">
            <a
              href={`https://wa.me/263787211325?text=${whatsappMessage}`}
              target="_blank"
              rel="noopener noreferrer"
              className="inline-flex items-center justify-center gap-3 px-8 py-4 bg-green-600 hover:bg-green-700 text-white rounded-lg font-semibold transition-colors shadow-lg"
            >
              <FaWhatsapp className="w-5 h-5" />
              WhatsApp Us
            </a>
            <a
              href="mailto:partners@slykertech.co.zw"
              className="inline-flex items-center justify-center gap-3 px-8 py-4 bg-darkgoldenrod hover:bg-yellow-600 dark:bg-yellow-400 dark:hover:bg-yellow-500 text-white dark:text-gray-900 rounded-lg font-semibold transition-colors shadow-lg"
            >
              <FaEnvelope className="w-5 h-5" />
              Email Us
            </a>
          </div>
        </div>
      </section>
    </div>
  );
}
