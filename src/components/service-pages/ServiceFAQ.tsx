'use client';

import { useState } from 'react';
import { FaChevronDown } from 'react-icons/fa';

interface FAQItem {
  question: string;
  answer: string;
}

interface ServiceFAQProps {
  faqs: FAQItem[];
  serviceName: string;
}

export default function ServiceFAQ({ faqs, serviceName }: ServiceFAQProps) {
  const [openIndex, setOpenIndex] = useState<number | null>(0);

  const toggleFAQ = (index: number) => {
    setOpenIndex(openIndex === index ? null : index);
  };

  return (
    <section className="py-16 bg-white">
      <div className="max-w-4xl mx-auto px-4 sm:px-6 lg:px-8">
        <div className="text-center mb-12">
          <h2 className="text-3xl font-bold text-gray-900 mb-4">
            Frequently Asked Questions
          </h2>
          <p className="text-lg text-gray-600">
            Common questions about our {serviceName}
          </p>
        </div>

        <div className="space-y-4">
          {faqs.map((faq, index) => {
            const isOpen = openIndex === index;
            return (
              <div
                key={index}
                className={`border rounded-lg overflow-hidden transition-all duration-300 ${isOpen
                    ? 'border-blue-300 shadow-md'
                    : 'border-gray-200 hover:border-blue-200'
                  }`}
              >
                <button
                  onClick={() => toggleFAQ(index)}
                  className={`w-full px-6 py-4 text-left flex items-center justify-between transition-colors duration-300 ${isOpen
                      ? 'bg-blue-50 dark:bg-blue-900/20'
                      : 'bg-gray-50 hover:bg-gray-100'
                    }`}
                >
                  <span className={`font-semibold pr-4 transition-colors duration-300 ${isOpen ? 'text-blue-900' : 'text-gray-900'
                    }`}>
                    {faq.question}
                  </span>
                  <FaChevronDown
                    className={`accordion-chevron flex-shrink-0 transition-colors duration-300 ${isOpen ? 'rotated text-blue-600' : 'text-gray-400'
                      }`}
                  />
                </button>

                <div className={`accordion-content ${isOpen ? 'expanded' : ''}`}>
                  <div className="accordion-inner">
                    <div className="px-6 py-4 bg-white">
                      <p className="text-gray-600 leading-relaxed">{faq.answer}</p>
                    </div>
                  </div>
                </div>
              </div>
            );
          })}
        </div>
      </div>
    </section>
  );
}
