'use client';

import { FaCheck, FaRocket, FaShieldAlt, FaHeadset, FaClock } from 'react-icons/fa';
import { ReactNode } from 'react';
import { useStaggerReveal } from '@/lib/useScrollReveal';

interface Benefit {
  icon?: ReactNode;
  title: string;
  description: string;
}

interface WhyChooseUsProps {
  serviceName: string;
  benefits: Benefit[];
  className?: string;
}

const defaultIcons = [FaRocket, FaShieldAlt, FaHeadset, FaClock];

export default function WhyChooseUs({ serviceName, benefits, className = '' }: WhyChooseUsProps) {
  const gridRef = useStaggerReveal();

  return (
    <section className={`py-16 bg-gradient-to-br from-blue-50 to-indigo-50 ${className}`}>
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
        <div className="text-center mb-12">
          <h2 className="text-3xl font-bold text-gray-900 mb-4">
            Why Choose Our {serviceName}?
          </h2>
          <p className="text-lg text-gray-600 max-w-2xl mx-auto">
            We provide reliable, secure, and high-performance hosting solutions backed by expert support
          </p>
        </div>

        <div ref={gridRef} className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-8">
          {benefits.map((benefit, index) => {
            const IconComponent = defaultIcons[index % defaultIcons.length];

            return (
              <div
                key={index}
                className="scroll-reveal-child bg-white rounded-lg p-6 shadow-md hover:shadow-xl transition-all duration-300 hover:-translate-y-1 hover:border-blue-200 border border-transparent"
              >
                <div className="flex items-center justify-center w-12 h-12 bg-blue-100 rounded-lg mb-4">
                  {benefit.icon || <IconComponent className="text-blue-600 text-2xl" />}
                </div>
                <h3 className="text-xl font-semibold text-gray-900 mb-2">
                  {benefit.title}
                </h3>
                <p className="text-gray-600 leading-relaxed">
                  {benefit.description}
                </p>
              </div>
            );
          })}
        </div>

        <div className="mt-12 bg-white rounded-lg p-8 shadow-md">
          <div className="flex flex-col md:flex-row items-center justify-between">
            <div className="mb-4 md:mb-0">
              <h3 className="text-2xl font-bold text-gray-900 mb-2">
                99.9% Uptime Guarantee
              </h3>
              <p className="text-gray-600">
                Your website stays online when you need it most
              </p>
            </div>
            <div className="flex items-center space-x-4">
              <FaCheck className="text-green-500 text-3xl" />
              <div>
                <p className="text-sm text-gray-500">Monitored 24/7</p>
                <p className="text-sm text-gray-500">Enterprise-grade infrastructure</p>
              </div>
            </div>
          </div>
        </div>
      </div>
    </section>
  );
}
