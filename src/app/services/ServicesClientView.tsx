// app/services/ServicesClientView.tsx
'use client';

import { useState } from 'react';
import { FaWhatsapp, FaEnvelope } from 'react-icons/fa';
import { MdSupport } from 'react-icons/md'; // Added this import
import servicesData from './servicesData';
import { SITE_NAME, BASE_URL } from '@/lib/seo-config';

export default function ServicesClientView() {
  const [selectedService, setSelectedService] = useState<string | null>(null);

  const handleContact = (serviceTitle: string) => {
    const whatsappMessage = encodeURIComponent(
      `Hi ${SITE_NAME}! I'm interested in your ${serviceTitle} service. Could you provide more details?`
    );
    const emailSubject = encodeURIComponent(`Service Inquiry: ${serviceTitle}`);
    const emailBody = encodeURIComponent(
      `Dear ${SITE_NAME} Team,\n\nI would like more information about:\nService: ${serviceTitle}\n\nMy Requirements:\n[Briefly describe your needs]\n\nContact Details:\nPhone: [Your Number]\nCompany: [Your Business Name]\n\nBest regards,\n[Your Name]`
    );

    return (
      <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center p-4 z-50">
        <div className="bg-white dark:bg-gray-900 rounded-2xl p-8 max-w-md w-full">
          <h3 className="text-xl font-bold text-blue-900 dark:text-blue-300 mb-4">
            Contact About {serviceTitle}
          </h3>
          <div className="space-y-4">
            <a
              href={`https://wa.me/263787211325?text=${whatsappMessage}`}
              target="_blank"
              rel="noopener noreferrer"
              className="flex items-center gap-4 p-4 bg-green-100 dark:bg-green-900 rounded-lg hover:bg-green-200 dark:hover:bg-green-800 transition-colors"
              aria-label={`Contact via WhatsApp about ${serviceTitle}`}
            >
              <FaWhatsapp className="text-green-600 dark:text-green-400 w-6 h-6" />
              <span className="font-medium">WhatsApp Chat</span>
            </a>

            <div className="text-center text-gray-500 dark:text-gray-400">OR</div>

            <a
              href={`mailto:contact@${BASE_URL.replace('https://', '')}?subject=${emailSubject}&body=${emailBody}`}
              className="flex items-center gap-4 p-4 bg-blue-100 dark:bg-blue-900 rounded-lg hover:bg-blue-200 dark:hover:bg-blue-800 transition-colors"
              aria-label={`Email about ${serviceTitle}`}
            >
              <FaEnvelope className="text-blue-600 dark:text-blue-400 w-6 h-6" />
              <span className="font-medium">Send Email</span>
            </a>
          </div>
          <button
            onClick={() => setSelectedService(null)}
            className="mt-6 w-full py-2 text-gray-600 dark:text-gray-300 hover:text-gray-800 dark:hover:text-gray-100"
            type="button"
            aria-label="Close contact dialog"
          >
            Cancel
          </button>
        </div>
      </div>
    );
  };

  return (
    <div className="relative z-10">
      {/* Hero Section */}
      <section aria-labelledby="services-heading" className="py-28 px-4 sm:px-8 md:px-16 lg:px-24 text-center bg-gradient-to-b from-blue-50 to-transparent dark:from-blue-950/50">
        <div className="max-w-5xl mx-auto">
          <h1 id="services-heading" className="text-5xl sm:text-6xl md:text-7xl font-extrabold tracking-tight text-blue-900 dark:text-blue-300 leading-tight">
            Our <span className="text-darkgoldenrod dark:text-yellow-400">Digital Services</span>
          </h1>
          <p className="mt-8 text-lg sm:text-xl text-gray-700 dark:text-gray-300 max-w-3xl mx-auto">
            Comprehensive solutions designed to power African businesses in the digital economy
          </p>
        </div>
      </section>

      {/* Services Grid */}
      <section aria-labelledby="services-grid-heading" className="py-24 px-4 sm:px-8 bg-white dark:bg-gray-950">
        <h2 id="services-grid-heading" className="sr-only">Our Service Catalog</h2>
        <div className="max-w-7xl mx-auto grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-8">
          {servicesData.map((service, index) => (
            <article 
              key={`service-${index}`}
              className="p-8 border border-gray-200 dark:border-gray-700 rounded-2xl hover:shadow-lg transition-all cursor-pointer group"
              onClick={() => setSelectedService(service.title)}
              aria-labelledby={`service-${index}-title`}
            >
              <div className="text-darkgoldenrod dark:text-yellow-400 mb-6 transition-transform group-hover:scale-110" aria-hidden="true">
                {service.icon}
              </div>
              <h3 id={`service-${index}-title`} className="text-2xl font-semibold text-blue-900 dark:text-blue-300 mb-4 group-hover:text-blue-700 dark:group-hover:text-blue-200 transition-colors">
                {service.title}
              </h3>
              <p className="text-gray-600 dark:text-gray-400 mb-4">
                {service.description}
              </p>
              <ul className="mb-4 space-y-2">
                {service.features.map((feature, i) => (
                  <li key={`feature-${i}`} className="flex items-start">
                    <span className="text-darkgoldenrod dark:text-yellow-400 mr-2">•</span>
                    <span className="text-gray-700 dark:text-gray-300">{feature}</span>
                  </li>
                ))}
              </ul>
              <div className="flex flex-wrap gap-2" aria-label="Service tags">
                {service.keywords.split(', ').map((keyword, i) => (
                  <span 
                    key={`keyword-${i}`}
                    className="px-3 py-1 bg-blue-50 dark:bg-gray-800 text-blue-900 dark:text-blue-300 rounded-full text-sm"
                  >
                    {keyword}
                  </span>
                ))}
              </div>
            </article>
          ))}
        </div>
      </section>

      {/* CTA Section */}
      <section aria-label="Service inquiry" className="py-16 bg-gray-50 dark:bg-gray-900">
        <div className="max-w-7xl mx-auto px-4 sm:px-8 text-center">
          <div className="p-8 bg-white dark:bg-gray-800 rounded-2xl shadow-sm">
            <MdSupport className="text-4xl text-darkgoldenrod dark:text-yellow-400 mx-auto mb-4" aria-hidden="true" />
            <h3 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-2">
              Need a Custom Solution?
            </h3>
            <p className="text-gray-600 dark:text-gray-400 mb-6">
              We specialize in creating tailored digital solutions for unique business challenges
            </p>
            <button 
              onClick={() => setSelectedService('Custom Digital Solution')}
              className="px-8 py-3 bg-gradient-to-r from-blue-600 to-blue-700 text-white font-medium rounded-lg hover:shadow-lg transition-all"
              type="button"
              aria-label="Get custom consultation"
            >
              Request Custom Consultation
            </button>
          </div>
        </div>
      </section>

      {/* Contact Modal */}
      {selectedService && handleContact(selectedService)}
    </div>
  );
}