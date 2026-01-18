import type { Metadata } from 'next';
import { FaRocket, FaGlobeAfrica, FaChartLine, FaWhatsapp, FaEnvelope } from 'react-icons/fa';
import { MdSpeed, MdStorage, MdSecurity, MdTrendingUp } from 'react-icons/md';
import { generatePageMetadata } from '@/lib/seo-config';

export const metadata: Metadata = generatePageMetadata({
  title: 'CDN & Edge Caching',
  description: 'Accelerate your website with global CDN and edge caching. Deliver content faster to users across Africa and worldwide with 99.9% uptime.',
  url: '/products/cdn'
});

export default function CDNPage() {
  const whatsappMessage = encodeURIComponent(
    "Hi Slyker Tech! I'm interested in CDN & Edge Caching..."
  );

  const features = [
    {
      icon: <MdSpeed className="w-10 h-10" />,
      title: 'Lightning Fast',
      description: 'Serve content from edge locations closest to your users for blazing-fast load times'
    },
    {
      icon: <FaGlobeAfrica className="w-10 h-10" />,
      title: 'Global Network',
      description: '200+ edge locations across Africa and worldwide for optimal performance'
    },
    {
      icon: <MdSecurity className="w-10 h-10" />,
      title: 'DDoS Protection',
      description: 'Built-in DDoS mitigation and security at the edge to protect your origin'
    },
    {
      icon: <MdStorage className="w-10 h-10" />,
      title: 'Smart Caching',
      description: 'Intelligent caching rules to optimize performance and reduce bandwidth costs'
    },
    {
      icon: <FaChartLine className="w-10 h-10" />,
      title: 'Real-time Analytics',
      description: 'Detailed insights into traffic patterns, cache performance, and user locations'
    },
    {
      icon: <MdTrendingUp className="w-10 h-10" />,
      title: 'Auto Scaling',
      description: 'Handle traffic spikes automatically without performance degradation'
    }
  ];

  const benefits = [
    {
      title: 'Faster Page Loads',
      stat: '70%',
      description: 'Average reduction in page load times across Africa'
    },
    {
      title: 'Lower Bandwidth',
      stat: '60%',
      description: 'Reduction in origin server bandwidth and costs'
    },
    {
      title: 'Higher SEO Rankings',
      stat: '+25%',
      description: 'Improvement in Google rankings due to faster speeds'
    },
    {
      title: 'Better Uptime',
      stat: '99.9%',
      description: 'Uptime guarantee with redundant infrastructure'
    }
  ];

  return (
    <div className="relative z-10">
      {/* Hero Section */}
      <section className="py-28 px-4 sm:px-8 md:px-16 lg:px-24 text-center bg-gradient-to-b from-blue-50 to-transparent dark:from-blue-950/50">
        <div className="max-w-5xl mx-auto">
          <FaRocket className="w-20 h-20 mx-auto text-darkgoldenrod dark:text-yellow-400 mb-8" />
          <h1 className="text-5xl sm:text-6xl md:text-7xl font-extrabold tracking-tight text-blue-900 dark:text-blue-300 leading-tight">
            CDN & <span className="text-darkgoldenrod dark:text-yellow-400">Edge Caching</span>
          </h1>
          <p className="mt-8 text-lg sm:text-xl text-gray-700 dark:text-gray-300 max-w-3xl mx-auto">
            Deliver content at lightning speed to users across Africa and globally with our enterprise CDN network
          </p>
        </div>
      </section>

      {/* Features Section */}
      <section className="py-24 px-4 sm:px-8 bg-white dark:bg-gray-950">
        <div className="max-w-6xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-blue-900 dark:text-blue-300 mb-12">
            Accelerate Your Website
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

      {/* Benefits Stats */}
      <section className="py-24 px-4 sm:px-8 bg-gray-50 dark:bg-gray-900">
        <div className="max-w-6xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-darkgoldenrod dark:text-yellow-400 mb-12">
            Real Performance Impact
          </h2>
          <div className="grid grid-cols-1 md:grid-cols-4 gap-8">
            {benefits.map((benefit, index) => (
              <div
                key={index}
                className="p-8 bg-white dark:bg-gray-800 rounded-2xl text-center shadow-sm hover:shadow-md transition-shadow"
              >
                <div className="text-5xl font-bold text-darkgoldenrod dark:text-yellow-400 mb-3">
                  {benefit.stat}
                </div>
                <h3 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-2">
                  {benefit.title}
                </h3>
                <p className="text-gray-600 dark:text-gray-400 text-sm">
                  {benefit.description}
                </p>
              </div>
            ))}
          </div>
        </div>
      </section>

      {/* How It Works */}
      <section className="py-24 px-4 sm:px-8 bg-white dark:bg-gray-950">
        <div className="max-w-6xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-blue-900 dark:text-blue-300 mb-12">
            How CDN Works
          </h2>
          <div className="grid md:grid-cols-3 gap-8">
            <div className="p-8 bg-gray-50 dark:bg-gray-900 rounded-2xl">
              <div className="w-12 h-12 bg-darkgoldenrod dark:bg-yellow-400 text-white dark:text-gray-900 rounded-full flex items-center justify-center text-2xl font-bold mb-4">
                1
              </div>
              <h3 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-3">
                Content Distribution
              </h3>
              <p className="text-gray-600 dark:text-gray-400">
                Your website content is distributed to edge servers across 200+ global locations
              </p>
            </div>
            <div className="p-8 bg-gray-50 dark:bg-gray-900 rounded-2xl">
              <div className="w-12 h-12 bg-darkgoldenrod dark:bg-yellow-400 text-white dark:text-gray-900 rounded-full flex items-center justify-center text-2xl font-bold mb-4">
                2
              </div>
              <h3 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-3">
                Smart Routing
              </h3>
              <p className="text-gray-600 dark:text-gray-400">
                Users are automatically directed to the nearest edge server for optimal performance
              </p>
            </div>
            <div className="p-8 bg-gray-50 dark:bg-gray-900 rounded-2xl">
              <div className="w-12 h-12 bg-darkgoldenrod dark:bg-yellow-400 text-white dark:text-gray-900 rounded-full flex items-center justify-center text-2xl font-bold mb-4">
                3
              </div>
              <h3 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-3">
                Fast Delivery
              </h3>
              <p className="text-gray-600 dark:text-gray-400">
                Content is served from cache, reducing latency and improving user experience
              </p>
            </div>
          </div>
        </div>
      </section>

      {/* Use Cases */}
      <section className="py-24 px-4 sm:px-8 bg-gray-50 dark:bg-gray-900">
        <div className="max-w-6xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-darkgoldenrod dark:text-yellow-400 mb-12">
            Perfect For
          </h2>
          <div className="grid md:grid-cols-2 gap-8">
            <div className="p-8 bg-white dark:bg-gray-800 rounded-2xl">
              <h3 className="text-2xl font-semibold text-blue-900 dark:text-blue-300 mb-6">
                E-Commerce Sites
              </h3>
              <ul className="space-y-3 text-gray-600 dark:text-gray-400">
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Faster product page loads increase conversions
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Handle traffic spikes during sales and promotions
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Optimize image delivery across devices
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Reduce cart abandonment due to slow checkout
                </li>
              </ul>
            </div>
            <div className="p-8 bg-white dark:bg-gray-800 rounded-2xl">
              <h3 className="text-2xl font-semibold text-blue-900 dark:text-blue-300 mb-6">
                Media & Content Sites
              </h3>
              <ul className="space-y-3 text-gray-600 dark:text-gray-400">
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Stream video and audio with minimal buffering
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Deliver high-resolution images efficiently
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Support large file downloads
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Handle viral content traffic spikes
                </li>
              </ul>
            </div>
            <div className="p-8 bg-white dark:bg-gray-800 rounded-2xl">
              <h3 className="text-2xl font-semibold text-blue-900 dark:text-blue-300 mb-6">
                SaaS Applications
              </h3>
              <ul className="space-y-3 text-gray-600 dark:text-gray-400">
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Improve application responsiveness globally
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Cache API responses at the edge
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Reduce origin server load and costs
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Ensure consistent user experience
                </li>
              </ul>
            </div>
            <div className="p-8 bg-white dark:bg-gray-800 rounded-2xl">
              <h3 className="text-2xl font-semibold text-blue-900 dark:text-blue-300 mb-6">
                Corporate Websites
              </h3>
              <ul className="space-y-3 text-gray-600 dark:text-gray-400">
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Professional fast-loading pages
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Protect against DDoS attacks
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Improve SEO rankings with speed
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Reliable global accessibility
                </li>
              </ul>
            </div>
          </div>
        </div>
      </section>

      {/* Pricing */}
      <section className="py-24 px-4 sm:px-8 bg-white dark:bg-gray-950">
        <div className="max-w-4xl mx-auto text-center">
          <h2 className="text-4xl font-bold text-blue-900 dark:text-blue-300 mb-6">
            Simple Pay-As-You-Go Pricing
          </h2>
          <div className="p-12 bg-gradient-to-br from-blue-50 to-purple-50 dark:from-gray-800 dark:to-blue-900/50 rounded-3xl">
            <div className="mb-8">
              <span className="text-5xl font-bold text-darkgoldenrod dark:text-yellow-400">$0.08</span>
              <span className="text-xl text-gray-600 dark:text-gray-400">/GB</span>
            </div>
            <p className="text-lg text-gray-600 dark:text-gray-400 mb-8">
              Only pay for bandwidth you use. No minimum commitments or hidden fees.
            </p>
            <ul className="space-y-3 text-left max-w-md mx-auto mb-8">
              <li className="flex items-start gap-2 text-gray-600 dark:text-gray-400">
                <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                First 1TB free every month
              </li>
              <li className="flex items-start gap-2 text-gray-600 dark:text-gray-400">
                <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                Volume discounts for larger usage
              </li>
              <li className="flex items-start gap-2 text-gray-600 dark:text-gray-400">
                <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                Enterprise plans available
              </li>
              <li className="flex items-start gap-2 text-gray-600 dark:text-gray-400">
                <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                24/7 support included
              </li>
            </ul>
            <a
              href={`https://wa.me/263787211325?text=${encodeURIComponent("Hi! I'm interested in CDN services...")}`}
              target="_blank"
              rel="noopener noreferrer"
              className="inline-block px-8 py-4 bg-blue-600 hover:bg-blue-700 dark:bg-blue-700 dark:hover:bg-blue-600 text-white rounded-lg font-semibold transition-colors"
            >
              Get Started
            </a>
          </div>
        </div>
      </section>

      {/* CTA Section */}
      <section className="py-24 px-4 sm:px-8 bg-gradient-to-r from-blue-600 to-purple-700 dark:from-blue-900 dark:to-purple-900">
        <div className="max-w-4xl mx-auto text-center">
          <h2 className="text-4xl font-bold text-white mb-6">
            Speed Up Your Website Today
          </h2>
          <p className="text-xl text-blue-100 dark:text-blue-200 mb-12">
            Join businesses across Africa delivering content faster with our global CDN
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
