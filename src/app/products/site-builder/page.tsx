import type { Metadata } from 'next';
import { FaPaintBrush, FaMobile, FaCode, FaWhatsapp, FaEnvelope } from 'react-icons/fa';
import { MdDragIndicator, MdSpeed, MdDevices, MdShoppingCart } from 'react-icons/md';
import { generatePageMetadata } from '@/lib/seo-config';

export const metadata: Metadata = generatePageMetadata({
  title: 'Site Builder',
  description: 'Build professional websites with our intuitive drag-and-drop site builder. No coding required. Mobile-responsive templates, e-commerce ready, and SEO optimized.',
  url: '/products/site-builder'
});

export default function SiteBuilderPage() {
  const whatsappMessage = encodeURIComponent(
    "Hi Slyker Tech! I'm interested in the Site Builder..."
  );

  const features = [
    {
      icon: <MdDragIndicator className="w-10 h-10" />,
      title: 'Drag & Drop Editor',
      description: 'Build stunning websites with our intuitive visual editor—no coding required'
    },
    {
      icon: <MdDevices className="w-10 h-10" />,
      title: 'Mobile Responsive',
      description: 'All templates automatically adapt to mobile, tablet, and desktop screens'
    },
    {
      icon: <FaPaintBrush className="w-10 h-10" />,
      title: '200+ Templates',
      description: 'Professional templates for businesses, portfolios, e-commerce, and more'
    },
    {
      icon: <MdShoppingCart className="w-10 h-10" />,
      title: 'E-Commerce Ready',
      description: 'Built-in shopping cart with African payment gateway integration'
    },
    {
      icon: <MdSpeed className="w-10 h-10" />,
      title: 'SEO Optimized',
      description: 'Built-in SEO tools to help your site rank higher on Google'
    },
    {
      icon: <FaCode className="w-10 h-10" />,
      title: 'Custom Code',
      description: 'Add custom HTML, CSS, and JavaScript for advanced customization'
    }
  ];

  const useCases = [
    {
      title: 'Business Websites',
      description: 'Showcase your products and services with professional business templates',
      templates: 'Corporate, Professional Services, Consulting'
    },
    {
      title: 'Online Stores',
      description: 'Launch your e-commerce store with integrated payment gateways',
      templates: 'Fashion, Electronics, Food Delivery, Marketplace'
    },
    {
      title: 'Portfolios',
      description: 'Display your work with stunning portfolio and gallery templates',
      templates: 'Photography, Design, Creative Agency'
    },
    {
      title: 'Blogs & Media',
      description: 'Create engaging content websites with blog and magazine templates',
      templates: 'News, Magazine, Personal Blog, Podcast'
    }
  ];

  return (
    <div className="relative z-10">
      {/* Hero Section */}
      <section className="py-28 px-4 sm:px-8 md:px-16 lg:px-24 text-center bg-gradient-to-b from-blue-50 to-transparent dark:from-blue-950/50">
        <div className="max-w-5xl mx-auto">
          <FaPaintBrush className="w-20 h-20 mx-auto text-darkgoldenrod dark:text-yellow-400 mb-8" />
          <h1 className="text-5xl sm:text-6xl md:text-7xl font-extrabold tracking-tight text-blue-900 dark:text-blue-300 leading-tight">
            Build Your <span className="text-darkgoldenrod dark:text-yellow-400">Dream Website</span>
          </h1>
          <p className="mt-8 text-lg sm:text-xl text-gray-700 dark:text-gray-300 max-w-3xl mx-auto">
            Create professional websites in minutes with our powerful drag-and-drop site builder. No coding skills required.
          </p>
        </div>
      </section>

      {/* Features Section */}
      <section className="py-24 px-4 sm:px-8 bg-white dark:bg-gray-950">
        <div className="max-w-6xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-blue-900 dark:text-blue-300 mb-12">
            Everything You Need to Build Amazing Sites
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

      {/* Use Cases */}
      <section className="py-24 px-4 sm:px-8 bg-gray-50 dark:bg-gray-900">
        <div className="max-w-6xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-darkgoldenrod dark:text-yellow-400 mb-12">
            Perfect For Every Business
          </h2>
          <div className="grid md:grid-cols-2 gap-8">
            {useCases.map((useCase, index) => (
              <div
                key={index}
                className="p-8 bg-white dark:bg-gray-800 rounded-2xl shadow-sm hover:shadow-md transition-shadow"
              >
                <h3 className="text-2xl font-semibold text-blue-900 dark:text-blue-300 mb-3">
                  {useCase.title}
                </h3>
                <p className="text-gray-600 dark:text-gray-400 mb-4">
                  {useCase.description}
                </p>
                <div className="inline-block px-4 py-2 bg-blue-50 dark:bg-gray-700 rounded-lg">
                  <p className="text-sm text-gray-600 dark:text-gray-400">
                    <span className="font-semibold text-blue-900 dark:text-blue-300">Templates: </span>
                    {useCase.templates}
                  </p>
                </div>
              </div>
            ))}
          </div>
        </div>
      </section>

      {/* Key Benefits */}
      <section className="py-24 px-4 sm:px-8 bg-white dark:bg-gray-950">
        <div className="max-w-6xl mx-auto">
          <div className="bg-gradient-to-br from-blue-50 to-purple-50 dark:from-gray-800 dark:to-blue-900/50 rounded-3xl p-12">
            <h2 className="text-4xl font-bold text-blue-900 dark:text-blue-300 mb-8 text-center">
              Why Choose Our Site Builder
            </h2>
            <div className="grid md:grid-cols-3 gap-8">
              <div className="text-center">
                <div className="text-4xl font-bold text-darkgoldenrod dark:text-yellow-400 mb-2">
                  <FaMobile className="w-12 h-12 mx-auto" />
                </div>
                <h3 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-2">
                  Mobile First
                </h3>
                <p className="text-gray-600 dark:text-gray-400">
                  All sites automatically optimized for mobile users across Africa
                </p>
              </div>
              <div className="text-center">
                <div className="text-4xl font-bold text-darkgoldenrod dark:text-yellow-400 mb-2">
                  ⚡
                </div>
                <h3 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-2">
                  Lightning Fast
                </h3>
                <p className="text-gray-600 dark:text-gray-400">
                  Optimized hosting with CDN for fast loading across all devices
                </p>
              </div>
              <div className="text-center">
                <div className="text-4xl font-bold text-darkgoldenrod dark:text-yellow-400 mb-2">
                  🔒
                </div>
                <h3 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-2">
                  Secure & Reliable
                </h3>
                <p className="text-gray-600 dark:text-gray-400">
                  Free SSL, automatic backups, and 99.9% uptime guarantee
                </p>
              </div>
            </div>
          </div>
        </div>
      </section>

      {/* Pricing Preview */}
      <section className="py-24 px-4 sm:px-8 bg-gray-50 dark:bg-gray-900">
        <div className="max-w-4xl mx-auto text-center">
          <h2 className="text-4xl font-bold text-blue-900 dark:text-blue-300 mb-6">
            Simple, Transparent Pricing
          </h2>
          <p className="text-xl text-gray-600 dark:text-gray-400 mb-12">
            All plans include hosting, SSL certificates, and 24/7 support
          </p>
          <div className="grid md:grid-cols-2 gap-8">
            <div className="p-8 bg-white dark:bg-gray-800 rounded-2xl shadow-sm">
              <h3 className="text-2xl font-bold text-blue-900 dark:text-blue-300 mb-4">
                Personal
              </h3>
              <div className="mb-6">
                <span className="text-4xl font-bold text-darkgoldenrod dark:text-yellow-400">$19</span>
                <span className="text-gray-600 dark:text-gray-400">/month</span>
              </div>
              <ul className="space-y-3 text-left mb-8">
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  <span className="text-gray-600 dark:text-gray-400">1 Website</span>
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  <span className="text-gray-600 dark:text-gray-400">200+ Templates</span>
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  <span className="text-gray-600 dark:text-gray-400">Free SSL & Hosting</span>
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  <span className="text-gray-600 dark:text-gray-400">Email Support</span>
                </li>
              </ul>
              <a
                href={`https://wa.me/263787211325?text=${encodeURIComponent("Hi! I'm interested in the Personal Site Builder plan...")}`}
                target="_blank"
                rel="noopener noreferrer"
                className="block px-6 py-3 bg-blue-600 hover:bg-blue-700 dark:bg-blue-700 dark:hover:bg-blue-600 text-white rounded-lg font-semibold transition-colors"
              >
                Get Started
              </a>
            </div>

            <div className="p-8 bg-gradient-to-br from-blue-600 to-purple-700 dark:from-blue-900 dark:to-purple-900 rounded-2xl shadow-xl text-white">
              <div className="text-center mb-4">
                <span className="px-4 py-1 bg-yellow-400 text-gray-900 rounded-full text-sm font-semibold">
                  Most Popular
                </span>
              </div>
              <h3 className="text-2xl font-bold mb-4">
                Business
              </h3>
              <div className="mb-6">
                <span className="text-4xl font-bold">$49</span>
                <span className="text-blue-100">/month</span>
              </div>
              <ul className="space-y-3 text-left mb-8">
                <li className="flex items-start gap-2">
                  <span className="text-yellow-400">✓</span>
                  <span className="text-blue-100">5 Websites</span>
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-yellow-400">✓</span>
                  <span className="text-blue-100">All Templates</span>
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-yellow-400">✓</span>
                  <span className="text-blue-100">E-Commerce Features</span>
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-yellow-400">✓</span>
                  <span className="text-blue-100">Priority Support</span>
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-yellow-400">✓</span>
                  <span className="text-blue-100">Custom Code</span>
                </li>
              </ul>
              <a
                href={`https://wa.me/263787211325?text=${encodeURIComponent("Hi! I'm interested in the Business Site Builder plan...")}`}
                target="_blank"
                rel="noopener noreferrer"
                className="block px-6 py-3 bg-white text-blue-900 hover:bg-gray-100 rounded-lg font-semibold transition-colors text-center"
              >
                Get Started
              </a>
            </div>
          </div>
        </div>
      </section>

      {/* CTA Section */}
      <section className="py-24 px-4 sm:px-8 bg-gradient-to-r from-blue-600 to-purple-700 dark:from-blue-900 dark:to-purple-900">
        <div className="max-w-4xl mx-auto text-center">
          <h2 className="text-4xl font-bold text-white mb-6">
            Start Building Your Website Today
          </h2>
          <p className="text-xl text-blue-100 dark:text-blue-200 mb-12">
            Join thousands of African businesses using our site builder to grow online
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
