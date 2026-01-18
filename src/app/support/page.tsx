import type { Metadata } from 'next';
import { FaWhatsapp, FaEnvelope, FaBook, FaQuestionCircle, FaTicketAlt } from 'react-icons/fa';
import { MdAccessTime, MdSupport, MdLiveHelp } from 'react-icons/md';
import { generatePageMetadata } from '@/lib/seo-config';

export const metadata: Metadata = generatePageMetadata({
  title: 'Support & Help Center',
  description: 'Get help with Slyker Tech Web Services. Access FAQs, documentation, submit support tickets, and contact our technical support team across Africa.',
  url: '/support'
});

export default function SupportPage() {
  const whatsappMessage = encodeURIComponent(
    "Hi Slyker Tech Web Services! I need support with..."
  );

  const faqs = [
    {
      question: 'What are your support hours?',
      answer: 'Monday-Friday: 7:30AM-5:30PM CAT, Saturday: 8:00AM-12:30PM CAT. Emergency support available for enterprise clients 24/7.'
    },
    {
      question: 'How quickly do you respond to support requests?',
      answer: 'WhatsApp inquiries: Within 30 minutes during business hours. Email tickets: Within 4 hours. Critical issues: Immediate response for enterprise clients.'
    },
    {
      question: 'Do you provide support across Africa?',
      answer: 'Yes! We support clients across 8+ African countries with local expertise and regional payment integration assistance.'
    },
    {
      question: 'What types of issues do you support?',
      answer: 'Technical issues, billing inquiries, hosting problems, domain management, API integration, payment gateway setup, and custom development questions.'
    },
    {
      question: 'Is there a cost for support?',
      answer: 'Basic support is included with all services. Priority support with dedicated account managers available for enterprise plans.'
    },
    {
      question: 'Can I schedule a consultation call?',
      answer: 'Yes! Contact us via WhatsApp or email to schedule a technical consultation or strategy session with our team.'
    }
  ];

  const supportChannels = [
    {
      title: 'WhatsApp Support',
      icon: <FaWhatsapp className="w-12 h-12" />,
      description: 'Instant support via WhatsApp',
      details: 'Get real-time help from our technical team. Fastest response times during business hours.',
      action: 'Start Chat',
      link: `https://wa.me/263787211325?text=${whatsappMessage}`,
      external: true
    },
    {
      title: 'Email Support',
      icon: <FaEnvelope className="w-12 h-12" />,
      description: 'Submit detailed support requests',
      details: 'For complex issues requiring documentation or screenshots. Response within 4 hours.',
      action: 'Email Us',
      link: 'mailto:support@slykertech.co.zw',
      external: false
    },
    {
      title: 'Client Portal',
      icon: <FaTicketAlt className="w-12 h-12" />,
      description: 'Manage tickets and services',
      details: 'Access your dashboard to view service status, submit tickets, and track support requests.',
      action: 'Open Portal',
      link: '/dashboard',
      external: false
    }
  ];

  return (
    <div className="relative z-10">
      {/* Hero Section */}
      <section className="py-28 px-4 sm:px-8 md:px-16 lg:px-24 text-center bg-gradient-to-b from-blue-50 to-transparent dark:from-blue-950/50">
        <div className="max-w-5xl mx-auto">
          <h1 className="text-5xl sm:text-6xl md:text-7xl font-extrabold tracking-tight text-blue-900 dark:text-blue-300 leading-tight">
            We&apos;re Here to <span className="text-darkgoldenrod dark:text-yellow-400">Help</span>
          </h1>
          <p className="mt-8 text-lg sm:text-xl text-gray-700 dark:text-gray-300 max-w-3xl mx-auto">
            Get expert support for all your hosting, development, and digital service needs across Africa
          </p>
        </div>
      </section>

      {/* Support Channels */}
      <section className="py-24 px-4 sm:px-8 bg-white dark:bg-gray-950">
        <div className="max-w-6xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-blue-900 dark:text-blue-300 mb-12">
            How Can We Help You?
          </h2>
          <div className="grid grid-cols-1 md:grid-cols-3 gap-8">
            {supportChannels.map((channel, index) => (
              <div
                key={index}
                className="p-8 bg-gray-50 dark:bg-gray-900 rounded-2xl hover:shadow-lg transition-shadow"
              >
                <div className="text-darkgoldenrod dark:text-yellow-400 mb-6">
                  {channel.icon}
                </div>
                <h3 className="text-2xl font-semibold text-blue-900 dark:text-blue-300 mb-3">
                  {channel.title}
                </h3>
                <p className="text-gray-600 dark:text-gray-400 mb-4">
                  {channel.description}
                </p>
                <p className="text-sm text-gray-500 dark:text-gray-500 mb-6">
                  {channel.details}
                </p>
                <a
                  href={channel.link}
                  target={channel.external ? "_blank" : undefined}
                  rel={channel.external ? "noopener noreferrer" : undefined}
                  className="inline-block px-6 py-3 bg-blue-600 hover:bg-blue-700 dark:bg-blue-700 dark:hover:bg-blue-600 text-white rounded-lg font-semibold transition-colors"
                >
                  {channel.action}
                </a>
              </div>
            ))}
          </div>
        </div>
      </section>

      {/* Support Hours */}
      <section className="py-16 px-4 sm:px-8 bg-gray-50 dark:bg-gray-900">
        <div className="max-w-6xl mx-auto">
          <div className="bg-white dark:bg-gray-800 rounded-2xl p-8 shadow-sm">
            <div className="flex flex-col md:flex-row items-center gap-8">
              <div className="text-darkgoldenrod dark:text-yellow-400">
                <MdAccessTime className="w-16 h-16" />
              </div>
              <div className="flex-1 text-center md:text-left">
                <h3 className="text-2xl font-semibold text-blue-900 dark:text-blue-300 mb-4">
                  Support Hours (CAT - Central Africa Time)
                </h3>
                <div className="grid md:grid-cols-2 gap-4 text-gray-600 dark:text-gray-400">
                  <div>
                    <p className="font-semibold">Weekdays</p>
                    <p>Monday - Friday: 7:30AM - 5:30PM</p>
                  </div>
                  <div>
                    <p className="font-semibold">Weekend</p>
                    <p>Saturday: 8:00AM - 12:30PM</p>
                    <p>Sunday: Closed</p>
                  </div>
                </div>
                <p className="mt-4 text-sm text-gray-500 dark:text-gray-500">
                  Enterprise clients receive 24/7 priority support for critical issues
                </p>
              </div>
            </div>
          </div>
        </div>
      </section>

      {/* FAQs */}
      <section className="py-24 px-4 sm:px-8 bg-white dark:bg-gray-950">
        <div className="max-w-4xl mx-auto">
          <div className="text-center mb-12">
            <FaQuestionCircle className="w-12 h-12 mx-auto text-darkgoldenrod dark:text-yellow-400 mb-4" />
            <h2 className="text-4xl font-bold text-blue-900 dark:text-blue-300 mb-4">
              Frequently Asked Questions
            </h2>
          </div>
          <div className="space-y-6">
            {faqs.map((faq, index) => (
              <div
                key={index}
                className="p-6 bg-gray-50 dark:bg-gray-900 rounded-xl"
              >
                <h3 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-3">
                  {faq.question}
                </h3>
                <p className="text-gray-600 dark:text-gray-400">
                  {faq.answer}
                </p>
              </div>
            ))}
          </div>
        </div>
      </section>

      {/* Resources */}
      <section className="py-24 px-4 sm:px-8 bg-gray-50 dark:bg-gray-900">
        <div className="max-w-6xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-darkgoldenrod dark:text-yellow-400 mb-12">
            Self-Service Resources
          </h2>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-8">
            <div className="p-8 bg-white dark:bg-gray-800 rounded-2xl shadow-sm">
              <FaBook className="w-10 h-10 text-darkgoldenrod dark:text-yellow-400 mb-4" />
              <h3 className="text-2xl font-semibold text-blue-900 dark:text-blue-300 mb-4">
                Documentation
              </h3>
              <p className="text-gray-600 dark:text-gray-400 mb-6">
                Access comprehensive guides for hosting setup, domain management, API integration, and development services
              </p>
              <ul className="space-y-2 text-gray-600 dark:text-gray-400">
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">▹</span>
                  Getting Started Guides
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">▹</span>
                  API Documentation
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">▹</span>
                  Video Tutorials
                </li>
              </ul>
            </div>

            <div className="p-8 bg-white dark:bg-gray-800 rounded-2xl shadow-sm">
              <MdLiveHelp className="w-10 h-10 text-darkgoldenrod dark:text-yellow-400 mb-4" />
              <h3 className="text-2xl font-semibold text-blue-900 dark:text-blue-300 mb-4">
                Knowledge Base
              </h3>
              <p className="text-gray-600 dark:text-gray-400 mb-6">
                Search our knowledge base for answers to common questions and troubleshooting steps
              </p>
              <ul className="space-y-2 text-gray-600 dark:text-gray-400">
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">▹</span>
                  Troubleshooting Articles
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">▹</span>
                  Best Practices
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">▹</span>
                  Integration Examples
                </li>
              </ul>
            </div>
          </div>
        </div>
      </section>

      {/* CTA Section */}
      <section className="py-24 px-4 sm:px-8 bg-gradient-to-r from-blue-600 to-purple-700 dark:from-blue-900 dark:to-purple-900">
        <div className="max-w-4xl mx-auto text-center">
          <MdSupport className="w-16 h-16 mx-auto text-white mb-6" />
          <h2 className="text-4xl font-bold text-white mb-6">
            Still Need Help?
          </h2>
          <p className="text-xl text-blue-100 dark:text-blue-200 mb-12">
            Our technical support team is ready to assist you across Africa
          </p>
          <div className="flex flex-col sm:flex-row gap-6 justify-center">
            <a
              href={`https://wa.me/263787211325?text=${whatsappMessage}`}
              target="_blank"
              rel="noopener noreferrer"
              className="inline-flex items-center justify-center gap-3 px-8 py-4 bg-green-600 hover:bg-green-700 text-white rounded-lg font-semibold transition-colors shadow-lg"
            >
              <FaWhatsapp className="w-5 h-5" />
              WhatsApp Support: +263 78 721 1325
            </a>
            <a
              href="mailto:support@slykertech.co.zw"
              className="inline-flex items-center justify-center gap-3 px-8 py-4 bg-white hover:bg-gray-100 text-blue-900 rounded-lg font-semibold transition-colors shadow-lg"
            >
              <FaEnvelope className="w-5 h-5" />
              Email Support Team
            </a>
          </div>
        </div>
      </section>
    </div>
  );
}
