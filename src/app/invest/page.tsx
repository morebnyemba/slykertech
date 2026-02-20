'use client';

import { useState, useEffect } from 'react';
import { FaChartLine, FaHandshake, FaGlobeAfrica, FaWhatsapp, FaEnvelope, FaCalculator } from 'react-icons/fa';
import { MdTrendingUp, MdSecurity, MdCloud } from 'react-icons/md';
import InvestmentApplicationForm from '@/components/investments/InvestmentApplicationForm';
import { useScrollReveal, useStaggerReveal } from '@/lib/useScrollReveal';
import SectionBackground from '@/components/SectionBackground';

interface InvestmentPackage {
  id: number;
  name: string;
  description: string;
  minimum_amount: string;
  expected_return: string;
  duration_months: number;
  is_active: boolean;
  investment_count: number;
}

export default function InvestPage() {
  const [packages, setPackages] = useState<InvestmentPackage[]>([]);
  const [loading, setLoading] = useState(true);
  const [selectedPackage, setSelectedPackage] = useState<InvestmentPackage | null>(null);
  const [calculatorAmount, setCalculatorAmount] = useState('10000');
  const [showApplicationForm, setShowApplicationForm] = useState(false);

  const heroRef = useScrollReveal<HTMLElement>();
  const whyRef = useStaggerReveal<HTMLElement>();
  const focusRef = useScrollReveal<HTMLElement>();
  const packagesRef = useStaggerReveal<HTMLElement>();
  const marketRef = useScrollReveal<HTMLElement>();
  const ctaRef = useScrollReveal<HTMLElement>();

  useEffect(() => {
    fetchPackages();
  }, []);

  const fetchPackages = async () => {
    try {
      const API_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:8000/api';
      const response = await fetch(`${API_URL}/investments/packages/`);
      const data = await response.json();
      setPackages(data.results || data);
    } catch {
      // Error fetching investment packages
    } finally {
      setLoading(false);
    }
  };

  const calculateReturn = (amount: number, returnRate: number, months: number) => {
    const principal = amount;
    const rate = returnRate / 100;
    const returns = principal * rate;
    return {
      principal,
      returns,
      total: principal + returns,
      monthlyReturn: returns / months
    };
  };

  const whatsappMessage = encodeURIComponent(
    "Hi Slyker Tech Web Services! I'm interested in investment opportunities..."
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
      <section
        ref={heroRef}
        className="scroll-reveal relative py-28 px-4 sm:px-8 md:px-16 lg:px-24 text-center bg-gradient-to-b from-blue-50 to-transparent dark:from-blue-950/50"
      >
        <SectionBackground variant="gradient-accent" />
        <div className="relative max-w-5xl mx-auto">
          <h1 className="text-5xl sm:text-6xl md:text-7xl font-extrabold tracking-tight text-blue-900 dark:text-blue-300 leading-tight">
            Invest in <span className="text-darkgoldenrod dark:text-yellow-400">Africa&apos;s Digital Future</span>
          </h1>
          <p className="mt-8 text-lg sm:text-xl text-gray-700 dark:text-gray-300 max-w-3xl mx-auto">
            Join us in powering digital transformation across Africa and beyond with enterprise cloud solutions, fintech innovation, and cutting-edge technology services
          </p>
        </div>
      </section>

      {/* Why Invest Section */}
      <section ref={whyRef} className="scroll-reveal py-24 px-4 sm:px-8 bg-white dark:bg-gray-950">
        <div className="max-w-6xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-blue-900 dark:text-blue-300 mb-12">
            Why Invest in Slyker Tech Web Services
          </h2>
          <div className="grid grid-cols-1 md:grid-cols-3 gap-8">
            <div className="scroll-reveal-child p-8 bg-gray-50 dark:bg-gray-900 rounded-2xl hover:shadow-lg hover:-translate-y-1 transition-all duration-300">
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

            <div className="scroll-reveal-child p-8 bg-gray-50 dark:bg-gray-900 rounded-2xl hover:shadow-lg hover:-translate-y-1 transition-all duration-300">
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

            <div className="scroll-reveal-child p-8 bg-gray-50 dark:bg-gray-900 rounded-2xl hover:shadow-lg hover:-translate-y-1 transition-all duration-300">
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
      <section ref={focusRef} className="scroll-reveal relative py-24 px-4 sm:px-8 bg-gray-50 dark:bg-gray-900">
        <SectionBackground variant="waves" />
        <div className="relative max-w-6xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-darkgoldenrod dark:text-yellow-400 mb-12">
            Investment Focus Areas
          </h2>
          <div className="space-y-8">
            {focusAreas.map((area, index) => (
              <div
                key={index}
                className="p-8 bg-white dark:bg-gray-800 rounded-2xl shadow-sm hover:shadow-lg transition-all duration-300 hover:-translate-y-0.5"
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

      {/* Investment Packages */}
      <section ref={packagesRef} className="scroll-reveal py-24 px-4 sm:px-8 bg-white dark:bg-gray-950">
        <div className="max-w-6xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-blue-900 dark:text-blue-300 mb-4">
            Investment Packages
          </h2>
          <p className="text-center text-gray-600 dark:text-gray-400 mb-12 max-w-2xl mx-auto">
            Choose from our range of investment packages designed to match different investment goals and risk profiles
          </p>

          {loading ? (
            <div className="text-center py-12">
              <p className="text-gray-600 dark:text-gray-400">Loading packages...</p>
            </div>
          ) : packages.length === 0 ? (
            <div className="text-center py-12">
              <p className="text-gray-600 dark:text-gray-400">No active investment packages at the moment.</p>
            </div>
          ) : (
            <div className="grid grid-cols-1 md:grid-cols-3 gap-8 mb-12">
              {packages.map((pkg) => (
                <div
                  key={pkg.id}
                  className="scroll-reveal-child p-8 bg-gradient-to-br from-blue-50 to-purple-50 dark:from-gray-800 dark:to-blue-900/50 rounded-2xl shadow-lg hover:shadow-xl transition-all duration-300 hover:-translate-y-1 border-2 border-transparent hover:border-darkgoldenrod dark:hover:border-yellow-400"
                >
                  <h3 className="text-2xl font-bold text-blue-900 dark:text-blue-300 mb-4">
                    {pkg.name}
                  </h3>
                  <div className="text-4xl font-extrabold text-darkgoldenrod dark:text-yellow-400 mb-4">
                    {pkg.expected_return}%
                  </div>
                  <p className="text-gray-600 dark:text-gray-400 mb-6">
                    {pkg.description}
                  </p>

                  <div className="space-y-3 mb-6">
                    <div className="flex justify-between">
                      <span className="text-gray-600 dark:text-gray-400">Minimum:</span>
                      <span className="font-semibold text-blue-900 dark:text-blue-300">
                        ${parseFloat(pkg.minimum_amount).toLocaleString()}
                      </span>
                    </div>
                    <div className="flex justify-between">
                      <span className="text-gray-600 dark:text-gray-400">Duration:</span>
                      <span className="font-semibold text-blue-900 dark:text-blue-300">
                        {pkg.duration_months} months
                      </span>
                    </div>
                    <div className="flex justify-between">
                      <span className="text-gray-600 dark:text-gray-400">Active Investments:</span>
                      <span className="font-semibold text-blue-900 dark:text-blue-300">
                        {pkg.investment_count}
                      </span>
                    </div>
                  </div>

                  <button
                    onClick={() => {
                      setSelectedPackage(pkg);
                      setCalculatorAmount(pkg.minimum_amount);
                    }}
                    className="w-full px-6 py-3 bg-blue-600 hover:bg-blue-700 dark:bg-blue-700 dark:hover:bg-blue-600 text-white rounded-lg font-semibold transition-colors mb-2"
                  >
                    Calculate Returns
                  </button>
                  <button
                    onClick={() => {
                      setSelectedPackage(pkg);
                      setShowApplicationForm(true);
                    }}
                    className="w-full px-6 py-3 bg-darkgoldenrod hover:bg-yellow-600 dark:bg-yellow-600 dark:hover:bg-yellow-500 text-white rounded-lg font-semibold transition-colors"
                  >
                    Invest Now
                  </button>
                </div>
              ))}
            </div>
          )}

          {/* Calculator */}
          {selectedPackage && !showApplicationForm && (
            <div className="bg-gray-50 dark:bg-gray-900 rounded-2xl p-8 shadow-lg">
              <div className="flex items-center gap-3 mb-6">
                <FaCalculator className="text-darkgoldenrod dark:text-yellow-400 text-2xl" />
                <h3 className="text-2xl font-bold text-blue-900 dark:text-blue-300">
                  Investment Calculator - {selectedPackage.name}
                </h3>
              </div>

              <div className="mb-6">
                <label className="block text-gray-700 dark:text-gray-300 mb-2 font-medium">
                  Investment Amount ($)
                </label>
                <input
                  type="number"
                  value={calculatorAmount}
                  onChange={(e) => setCalculatorAmount(e.target.value)}
                  min={selectedPackage.minimum_amount}
                  step="1000"
                  className="w-full px-4 py-3 rounded-lg border border-gray-300 dark:border-gray-700 bg-white dark:bg-gray-800 text-gray-900 dark:text-gray-100 text-lg font-semibold"
                />
              </div>

              {(() => {
                const calc = calculateReturn(
                  parseFloat(calculatorAmount) || 0,
                  parseFloat(selectedPackage.expected_return),
                  selectedPackage.duration_months
                );
                return (
                  <div className="grid grid-cols-1 md:grid-cols-4 gap-4">
                    <div className="p-4 bg-white dark:bg-gray-800 rounded-lg">
                      <p className="text-sm text-gray-600 dark:text-gray-400 mb-1">Principal</p>
                      <p className="text-2xl font-bold text-blue-900 dark:text-blue-300">
                        ${calc.principal.toLocaleString()}
                      </p>
                    </div>
                    <div className="p-4 bg-white dark:bg-gray-800 rounded-lg">
                      <p className="text-sm text-gray-600 dark:text-gray-400 mb-1">Returns</p>
                      <p className="text-2xl font-bold text-green-600 dark:text-green-400">
                        +${calc.returns.toLocaleString()}
                      </p>
                    </div>
                    <div className="p-4 bg-white dark:bg-gray-800 rounded-lg">
                      <p className="text-sm text-gray-600 dark:text-gray-400 mb-1">Total Value</p>
                      <p className="text-2xl font-bold text-darkgoldenrod dark:text-yellow-400">
                        ${calc.total.toLocaleString()}
                      </p>
                    </div>
                    <div className="p-4 bg-white dark:bg-gray-800 rounded-lg">
                      <p className="text-sm text-gray-600 dark:text-gray-400 mb-1">Monthly Avg</p>
                      <p className="text-2xl font-bold text-purple-600 dark:text-purple-400">
                        ${calc.monthlyReturn.toLocaleString()}
                      </p>
                    </div>
                  </div>
                );
              })()}
            </div>
          )}
        </div>
      </section>

      {/* Market Opportunity */}
      <section ref={marketRef} className="scroll-reveal relative py-24 px-4 sm:px-8 bg-white dark:bg-gray-950">
        <SectionBackground variant="grid" />
        <div className="relative max-w-6xl mx-auto">
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
      <section ref={ctaRef} className="scroll-reveal relative py-24 px-4 sm:px-8 bg-gradient-to-r from-blue-600 to-purple-700 dark:from-blue-900 dark:to-purple-900">
        <SectionBackground variant="radial-glow" />
        <div className="relative max-w-4xl mx-auto text-center">
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

      {/* Investment Application Form Modal */}
      {showApplicationForm && selectedPackage && (
        <InvestmentApplicationForm
          package={selectedPackage}
          onClose={() => {
            setShowApplicationForm(false);
            setSelectedPackage(null);
          }}
        />
      )}
    </div>
  );
}
