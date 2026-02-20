'use client';

import Link from 'next/link';
import { FaGift, FaArrowRight, FaTags, FaPercent } from 'react-icons/fa';
import { useStaggerReveal } from '@/lib/useScrollReveal';

interface PromotionsCTAProps {
  serviceName?: string;
  className?: string;
}

export default function PromotionsCTA({ serviceName, className = '' }: PromotionsCTAProps) {
  const cardsRef = useStaggerReveal();

  return (
    <section className={`py-16 bg-gradient-to-r from-blue-600 to-blue-800 dark:from-blue-700 dark:to-blue-900 ${className}`}>
      <div className="max-w-6xl mx-auto px-4 sm:px-6 lg:px-8">
        <div className="text-center text-white mb-8">
          <div className="inline-flex items-center justify-center w-16 h-16 bg-white/20 rounded-full mb-4">
            <FaGift className="text-3xl text-white" />
          </div>
          <h2 className="text-3xl md:text-4xl font-bold mb-4">
            Special Offers & Promotions
          </h2>
          <p className="text-xl text-blue-100 max-w-2xl mx-auto">
            {serviceName
              ? `Don't miss out on exclusive deals for ${serviceName}`
              : 'Save more with our current promotional offers'}
          </p>
        </div>

        <div ref={cardsRef} className="grid grid-cols-1 md:grid-cols-3 gap-6 mb-8">
          <div className="scroll-reveal-child bg-white/10 backdrop-blur-sm rounded-lg p-6 border border-white/20 transition-all duration-300 hover:bg-white/20 hover:-translate-y-1">
            <FaPercent className="text-3xl text-white mb-3" />
            <h3 className="text-xl font-semibold text-white mb-2">
              Limited Time Discounts
            </h3>
            <p className="text-blue-100">
              Up to 50% off on annual plans
            </p>
          </div>

          <div className="scroll-reveal-child bg-white/10 backdrop-blur-sm rounded-lg p-6 border border-white/20 transition-all duration-300 hover:bg-white/20 hover:-translate-y-1">
            <FaTags className="text-3xl text-white mb-3" />
            <h3 className="text-xl font-semibold text-white mb-2">
              Bundle & Save
            </h3>
            <p className="text-blue-100">
              Combine services for extra savings
            </p>
          </div>

          <div className="scroll-reveal-child bg-white/10 backdrop-blur-sm rounded-lg p-6 border border-white/20 transition-all duration-300 hover:bg-white/20 hover:-translate-y-1">
            <FaGift className="text-3xl text-white mb-3" />
            <h3 className="text-xl font-semibold text-white mb-2">
              Free Extras
            </h3>
            <p className="text-blue-100">
              Free domain, SSL, and more included
            </p>
          </div>
        </div>

        <div className="text-center">
          <Link
            href="/promotions"
            className="inline-flex items-center px-8 py-4 bg-white text-blue-600 font-semibold rounded-lg hover:bg-gray-100 transition-all duration-200 shadow-lg hover:shadow-xl transform hover:-translate-y-1"
          >
            View All Promotions
            <FaArrowRight className="ml-2" />
          </Link>
          <p className="mt-4 text-sm text-blue-100">
            New offers added regularly • Terms and conditions apply
          </p>
        </div>
      </div>
    </section>
  );
}
