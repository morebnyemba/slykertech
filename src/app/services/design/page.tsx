import type { Metadata } from 'next';
import { FaPaintBrush, FaMobile, FaShoppingCart, FaWhatsapp, FaEnvelope } from 'react-icons/fa';
import { MdBrush, MdWeb, MdDevices, MdColorLens } from 'react-icons/md';
import { generatePageMetadata } from '@/lib/seo-config';

export const metadata: Metadata = generatePageMetadata({
  title: 'Design Services',
  description: 'Professional design services for African businesses. UI/UX design, branding, web design, mobile app design, and graphic design by expert designers.',
  url: '/services/design'
});

export default function DesignServicesPage() {
  const whatsappMessage = encodeURIComponent(
    "Hi Slyker Tech Web Services! I'm interested in design services..."
  );

  const services = [
    {
      icon: <MdWeb className="w-12 h-12" />,
      title: 'Web Design',
      description: 'Modern, responsive website designs optimized for conversions and user experience',
      deliverables: [
        'Custom homepage and internal page designs',
        'Mobile-responsive layouts',
        'Interactive prototypes',
        'Design system and style guide'
      ]
    },
    {
      icon: <MdDevices className="w-12 h-12" />,
      title: 'Mobile App Design',
      description: 'Intuitive mobile app interfaces for iOS and Android platforms',
      deliverables: [
        'User flow diagrams',
        'Wireframes and prototypes',
        'High-fidelity UI designs',
        'Icon and asset creation'
      ]
    },
    {
      icon: <MdColorLens className="w-12 h-12" />,
      title: 'Brand Identity',
      description: 'Complete brand identity packages including logos, colors, and brand guidelines',
      deliverables: [
        'Logo design with variations',
        'Color palette and typography',
        'Brand guidelines document',
        'Business card and stationery'
      ]
    },
    {
      icon: <FaShoppingCart className="w-12 h-12" />,
      title: 'E-Commerce Design',
      description: 'Conversion-optimized designs for online stores and marketplaces',
      deliverables: [
        'Product page designs',
        'Shopping cart and checkout flows',
        'Category and filter layouts',
        'Mobile shopping experience'
      ]
    },
    {
      icon: <MdBrush className="w-12 h-12" />,
      title: 'Graphic Design',
      description: 'Marketing materials, social media graphics, and promotional designs',
      deliverables: [
        'Social media templates',
        'Banner ads and promotional graphics',
        'Presentation decks',
        'Print materials'
      ]
    },
    {
      icon: <FaPaintBrush className="w-12 h-12" />,
      title: 'UI/UX Design',
      description: 'User research and interface design focused on African users',
      deliverables: [
        'User research and personas',
        'Information architecture',
        'Usability testing',
        'Design iteration and refinement'
      ]
    }
  ];

  const process = [
    {
      step: '1',
      title: 'Discovery',
      description: 'Understanding your business, target audience, and design requirements'
    },
    {
      step: '2',
      title: 'Research',
      description: 'Competitive analysis and user research to inform design decisions'
    },
    {
      step: '3',
      title: 'Concept',
      description: 'Initial design concepts and mood boards for your review'
    },
    {
      step: '4',
      title: 'Design',
      description: 'High-fidelity designs with multiple revision rounds'
    },
    {
      step: '5',
      title: 'Prototype',
      description: 'Interactive prototypes for testing and stakeholder approval'
    },
    {
      step: '6',
      title: 'Delivery',
      description: 'Final designs with all assets, guidelines, and documentation'
    }
  ];

  return (
    <div className="relative z-10">
      {/* Hero Section */}
      <section className="py-28 px-4 sm:px-8 md:px-16 lg:px-24 text-center bg-gradient-to-b from-blue-50 to-transparent dark:from-blue-950/50">
        <div className="max-w-5xl mx-auto">
          <FaPaintBrush className="w-20 h-20 mx-auto text-darkgoldenrod dark:text-yellow-400 mb-8" />
          <h1 className="text-5xl sm:text-6xl md:text-7xl font-extrabold tracking-tight text-blue-900 dark:text-blue-300 leading-tight">
            Professional <span className="text-darkgoldenrod dark:text-yellow-400">Design</span> Services
          </h1>
          <p className="mt-8 text-lg sm:text-xl text-gray-700 dark:text-gray-300 max-w-3xl mx-auto">
            Transform your brand with stunning designs crafted by expert designers who understand African markets
          </p>
        </div>
      </section>

      {/* Services Section */}
      <section className="py-24 px-4 sm:px-8 bg-white dark:bg-gray-950">
        <div className="max-w-6xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-blue-900 dark:text-blue-300 mb-12">
            Our Design Services
          </h2>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-8">
            {services.map((service, index) => (
              <div
                key={index}
                className="p-8 bg-gray-50 dark:bg-gray-900 rounded-2xl hover:shadow-lg transition-shadow"
              >
                <div className="text-darkgoldenrod dark:text-yellow-400 mb-4">
                  {service.icon}
                </div>
                <h3 className="text-2xl font-semibold text-blue-900 dark:text-blue-300 mb-3">
                  {service.title}
                </h3>
                <p className="text-gray-600 dark:text-gray-400 mb-4">
                  {service.description}
                </p>
                <div className="space-y-2">
                  <p className="text-sm font-semibold text-blue-900 dark:text-blue-300">Deliverables:</p>
                  {service.deliverables.map((item, i) => (
                    <div key={i} className="flex items-start gap-2 text-sm text-gray-600 dark:text-gray-400">
                      <span className="text-darkgoldenrod dark:text-yellow-400">•</span>
                      {item}
                    </div>
                  ))}
                </div>
              </div>
            ))}
          </div>
        </div>
      </section>

      {/* Design Process */}
      <section className="py-24 px-4 sm:px-8 bg-gray-50 dark:bg-gray-900">
        <div className="max-w-6xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-darkgoldenrod dark:text-yellow-400 mb-12">
            Our Design Process
          </h2>
          <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
            {process.map((item, index) => (
              <div
                key={index}
                className="p-8 bg-white dark:bg-gray-800 rounded-2xl shadow-sm hover:shadow-md transition-shadow"
              >
                <div className="w-12 h-12 bg-darkgoldenrod dark:bg-yellow-400 text-white dark:text-gray-900 rounded-full flex items-center justify-center text-2xl font-bold mb-4">
                  {item.step}
                </div>
                <h3 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-3">
                  {item.title}
                </h3>
                <p className="text-gray-600 dark:text-gray-400">
                  {item.description}
                </p>
              </div>
            ))}
          </div>
        </div>
      </section>

      {/* Why Choose Us */}
      <section className="py-24 px-4 sm:px-8 bg-white dark:bg-gray-950">
        <div className="max-w-6xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-blue-900 dark:text-blue-300 mb-12">
            Why Choose Our Design Team
          </h2>
          <div className="grid md:grid-cols-2 gap-8">
            <div className="p-8 bg-gray-50 dark:bg-gray-900 rounded-2xl">
              <h3 className="text-2xl font-semibold text-blue-900 dark:text-blue-300 mb-6">
                Africa-Focused Design
              </h3>
              <ul className="space-y-3 text-gray-600 dark:text-gray-400">
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Deep understanding of African user preferences and behaviors
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Optimized for local connectivity and device constraints
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Cultural sensitivity and localization expertise
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Experience with African payment systems and mobile money
                </li>
              </ul>
            </div>
            <div className="p-8 bg-gray-50 dark:bg-gray-900 rounded-2xl">
              <h3 className="text-2xl font-semibold text-blue-900 dark:text-blue-300 mb-6">
                Professional Excellence
              </h3>
              <ul className="space-y-3 text-gray-600 dark:text-gray-400">
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Expert designers with international portfolio experience
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Modern design tools: Figma, Adobe Creative Suite, Sketch
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Fast turnaround with multiple revision rounds included
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Collaborative process with regular client feedback
                </li>
              </ul>
            </div>
          </div>
        </div>
      </section>

      {/* Pricing */}
      <section className="py-24 px-4 sm:px-8 bg-gray-50 dark:bg-gray-900">
        <div className="max-w-4xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-darkgoldenrod dark:text-yellow-400 mb-12">
            Flexible Pricing
          </h2>
          <div className="grid md:grid-cols-2 gap-8">
            <div className="p-8 bg-white dark:bg-gray-800 rounded-2xl shadow-sm">
              <h3 className="text-2xl font-bold text-blue-900 dark:text-blue-300 mb-4">
                Project-Based
              </h3>
              <p className="text-gray-600 dark:text-gray-400 mb-6">
                Fixed-price projects with clearly defined scope and deliverables
              </p>
              <ul className="space-y-3 text-gray-600 dark:text-gray-400 mb-6">
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Logo design: Starting at $500
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Website design: Starting at $2,000
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Mobile app design: Starting at $3,500
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Brand identity package: Starting at $2,500
                </li>
              </ul>
            </div>

            <div className="p-8 bg-gradient-to-br from-blue-600 to-purple-700 dark:from-blue-900 dark:to-purple-900 rounded-2xl shadow-xl text-white">
              <div className="text-center mb-4">
                <span className="px-4 py-1 bg-yellow-400 text-gray-900 rounded-full text-sm font-semibold">
                  Popular
                </span>
              </div>
              <h3 className="text-2xl font-bold mb-4">
                Monthly Retainer
              </h3>
              <p className="text-blue-100 mb-6">
                Ongoing design support for businesses with regular design needs
              </p>
              <ul className="space-y-3 text-blue-100 mb-6">
                <li className="flex items-start gap-2">
                  <span className="text-yellow-400">✓</span>
                  Dedicated designer for your brand
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-yellow-400">✓</span>
                  Priority support and fast turnaround
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-yellow-400">✓</span>
                  Unlimited revision requests
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-yellow-400">✓</span>
                  Starting at $1,500/month
                </li>
              </ul>
            </div>
          </div>
          <p className="text-center text-gray-600 dark:text-gray-400 mt-8">
            Custom enterprise packages available. Contact us for a detailed quote.
          </p>
        </div>
      </section>

      {/* Portfolio CTA */}
      <section className="py-24 px-4 sm:px-8 bg-white dark:bg-gray-950">
        <div className="max-w-4xl mx-auto text-center">
          <FaMobile className="w-16 h-16 mx-auto text-darkgoldenrod dark:text-yellow-400 mb-6" />
          <h2 className="text-4xl font-bold text-blue-900 dark:text-blue-300 mb-6">
            View Our Portfolio
          </h2>
          <p className="text-xl text-gray-600 dark:text-gray-400 mb-8">
            See examples of our design work for clients across Africa
          </p>
          <a
            href="/portfolio"
            className="inline-block px-8 py-4 bg-blue-600 hover:bg-blue-700 dark:bg-blue-700 dark:hover:bg-blue-600 text-white rounded-lg font-semibold transition-colors"
          >
            Browse Portfolio
          </a>
        </div>
      </section>

      {/* CTA Section */}
      <section className="py-24 px-4 sm:px-8 bg-gradient-to-r from-blue-600 to-purple-700 dark:from-blue-900 dark:to-purple-900">
        <div className="max-w-4xl mx-auto text-center">
          <h2 className="text-4xl font-bold text-white mb-6">
            Let&apos;s Create Something Beautiful
          </h2>
          <p className="text-xl text-blue-100 dark:text-blue-200 mb-12">
            Start your design project with our expert team today
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
              Email Design Team
            </a>
          </div>
        </div>
      </section>
    </div>
  );
}
