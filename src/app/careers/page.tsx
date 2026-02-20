'use client';

import { useState, useEffect } from 'react';
import { FaUsers, FaHeart, FaRocket, FaWhatsapp, FaEnvelope, FaFilter } from 'react-icons/fa';
import { MdWork, MdTrendingUp, MdHealthAndSafety } from 'react-icons/md';
import { useScrollReveal, useStaggerReveal } from '@/lib/useScrollReveal';
import SectionBackground from '@/components/SectionBackground';

interface JobPosting {
  id: number;
  title: string;
  description: string;
  employment_type: string;
  location: string;
  salary_range?: string;
  requirements: string;
  responsibilities: string;
  posted_date: string;
  deadline: string;
  is_active: boolean;
  application_count: number;
}

export default function CareersPage() {
  const [jobs, setJobs] = useState<JobPosting[]>([]);
  const [loading, setLoading] = useState(true);
  const [selectedType, setSelectedType] = useState<string>('all');
  const [selectedLocation, setSelectedLocation] = useState<string>('all');

  const heroRef = useScrollReveal<HTMLElement>();
  const cultureRef = useStaggerReveal<HTMLElement>();
  const benefitsRef = useStaggerReveal<HTMLElement>();
  const positionsRef = useScrollReveal<HTMLElement>();
  const processRef = useScrollReveal<HTMLElement>();
  const ctaRef = useScrollReveal<HTMLElement>();

  useEffect(() => {
    fetchJobs();
  }, []);

  const fetchJobs = async () => {
    try {
      const API_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:8000/api';
      const response = await fetch(`${API_URL}/jobs/postings/?is_active=true`);
      const data = await response.json();
      setJobs(data.results || data);
    } catch {
      // Error fetching jobs
    } finally {
      setLoading(false);
    }
  };

  const filteredJobs = jobs.filter(job => {
    const typeMatch = selectedType === 'all' || job.employment_type === selectedType;
    const locationMatch = selectedLocation === 'all' || job.location.toLowerCase().includes(selectedLocation.toLowerCase());
    return typeMatch && locationMatch;
  });

  const employmentTypes = Array.from(new Set(jobs.map(j => j.employment_type)));
  const locations = Array.from(new Set(jobs.map(j => j.location)));

  const whatsappMessage = encodeURIComponent(
    "Hi Slyker Tech Web Services! I'm interested in career opportunities..."
  );

  const benefits = [
    {
      title: 'Competitive Compensation',
      icon: <MdTrendingUp className="w-12 h-12" />,
      description: 'Market-competitive salaries, performance bonuses, and equity opportunities for key roles'
    },
    {
      title: 'Remote-First Culture',
      icon: <FaRocket className="w-12 h-12" />,
      description: 'Work from anywhere in Africa with flexible hours and collaborative remote workflows'
    },
    {
      title: 'Growth & Learning',
      icon: <MdWork className="w-12 h-12" />,
      description: 'Continuous learning budget, conference attendance, and mentorship programs'
    },
    {
      title: 'Health & Wellness',
      icon: <MdHealthAndSafety className="w-12 h-12" />,
      description: 'Health insurance, wellness programs, and work-life balance initiatives'
    }
  ];

  return (
    <div className="relative z-10">
      {/* Hero Section */}
      <section
        ref={heroRef}
        className="scroll-reveal relative py-28 px-4 sm:px-8 md:px-16 lg:px-24 text-center bg-gradient-to-b from-blue-50 to-transparent dark:from-blue-950/50"
      >
        <SectionBackground variant="waves" />
        <div className="relative max-w-5xl mx-auto">
          <h1 className="text-5xl sm:text-6xl md:text-7xl font-extrabold tracking-tight text-blue-900 dark:text-blue-300 leading-tight">
            Build the <span className="text-darkgoldenrod dark:text-yellow-400">Future of Africa</span>
          </h1>
          <p className="mt-8 text-lg sm:text-xl text-gray-700 dark:text-gray-300 max-w-3xl mx-auto">
            Join our team of innovators driving digital transformation across Africa with enterprise cloud solutions, fintech, and cutting-edge technology
          </p>
        </div>
      </section>

      {/* Company Culture */}
      <section ref={cultureRef} className="scroll-reveal py-24 px-4 sm:px-8 bg-white dark:bg-gray-950">
        <div className="max-w-6xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-blue-900 dark:text-blue-300 mb-12">
            Why Work at Slyker Tech Web Services
          </h2>
          <div className="grid grid-cols-1 md:grid-cols-3 gap-8 mb-16">
            <div className="scroll-reveal-child p-8 bg-gray-50 dark:bg-gray-900 rounded-2xl hover:shadow-lg hover:-translate-y-1 transition-all duration-300 text-center">
              <div className="text-darkgoldenrod dark:text-yellow-400 mb-6 flex justify-center">
                <FaUsers className="w-12 h-12" />
              </div>
              <h3 className="text-2xl font-semibold text-blue-900 dark:text-blue-300 mb-4">
                Diverse Team
              </h3>
              <p className="text-gray-600 dark:text-gray-400">
                Work with talented professionals from across Africa, bringing unique perspectives and expertise
              </p>
            </div>

            <div className="scroll-reveal-child p-8 bg-gray-50 dark:bg-gray-900 rounded-2xl hover:shadow-lg hover:-translate-y-1 transition-all duration-300 text-center">
              <div className="text-darkgoldenrod dark:text-yellow-400 mb-6 flex justify-center">
                <FaRocket className="w-12 h-12" />
              </div>
              <h3 className="text-2xl font-semibold text-blue-900 dark:text-blue-300 mb-4">
                Impact-Driven
              </h3>
              <p className="text-gray-600 dark:text-gray-400">
                Build solutions that solve real African challenges and improve lives across the continent
              </p>
            </div>

            <div className="scroll-reveal-child p-8 bg-gray-50 dark:bg-gray-900 rounded-2xl hover:shadow-lg hover:-translate-y-1 transition-all duration-300 text-center">
              <div className="text-darkgoldenrod dark:text-yellow-400 mb-6 flex justify-center">
                <FaHeart className="w-12 h-12" />
              </div>
              <h3 className="text-2xl font-semibold text-blue-900 dark:text-blue-300 mb-4">
                Innovation Culture
              </h3>
              <p className="text-gray-600 dark:text-gray-400">
                Experiment with cutting-edge technologies and contribute to open-source projects
              </p>
            </div>
          </div>

          <div className="bg-gradient-to-br from-blue-50 to-purple-50 dark:from-gray-800 dark:to-blue-900/50 rounded-3xl p-12">
            <blockquote className="text-center">
              <p className="text-2xl text-gray-700 dark:text-gray-300 italic mb-6">
                &quot;At Slyker Tech Web Services, we&apos;re not just building software—we&apos;re empowering Africa&apos;s digital future and creating opportunities for communities across the continent.&quot;
              </p>
              <footer className="text-darkgoldenrod dark:text-yellow-400 font-semibold">
                — Moreblessing Nyemba, Founder
              </footer>
            </blockquote>
          </div>
        </div>
      </section>

      {/* Benefits */}
      <section ref={benefitsRef} className="scroll-reveal relative py-24 px-4 sm:px-8 bg-gray-50 dark:bg-gray-900">
        <SectionBackground variant="dots" />
        <div className="relative max-w-6xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-darkgoldenrod dark:text-yellow-400 mb-12">
            Benefits & Perks
          </h2>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-8">
            {benefits.map((benefit, index) => (
              <div
                key={index}
                className="scroll-reveal-child p-8 bg-white dark:bg-gray-800 rounded-2xl shadow-sm hover:shadow-lg hover:-translate-y-1 transition-all duration-300"
              >
                <div className="flex items-start gap-6">
                  <div className="text-darkgoldenrod dark:text-yellow-400">
                    {benefit.icon}
                  </div>
                  <div>
                    <h3 className="text-2xl font-semibold text-blue-900 dark:text-blue-300 mb-3">
                      {benefit.title}
                    </h3>
                    <p className="text-gray-600 dark:text-gray-400">
                      {benefit.description}
                    </p>
                  </div>
                </div>
              </div>
            ))}
          </div>
        </div>
      </section>

      {/* Open Positions */}
      <section ref={positionsRef} className="scroll-reveal py-24 px-4 sm:px-8 bg-white dark:bg-gray-950">
        <div className="max-w-6xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-blue-900 dark:text-blue-300 mb-8">
            Open Positions
          </h2>

          {/* Filters */}
          <div className="mb-8 flex flex-wrap gap-4 justify-center">
            <div className="flex items-center gap-2">
              <FaFilter className="text-darkgoldenrod dark:text-yellow-400" />
              <select
                value={selectedType}
                onChange={(e) => setSelectedType(e.target.value)}
                className="px-4 py-2 rounded-lg border border-gray-300 dark:border-gray-700 bg-white dark:bg-gray-800 text-gray-900 dark:text-gray-100"
              >
                <option value="all">All Types</option>
                {employmentTypes.map(type => (
                  <option key={type} value={type}>{type}</option>
                ))}
              </select>
            </div>
            <select
              value={selectedLocation}
              onChange={(e) => setSelectedLocation(e.target.value)}
              className="px-4 py-2 rounded-lg border border-gray-300 dark:border-gray-700 bg-white dark:bg-gray-800 text-gray-900 dark:text-gray-100"
            >
              <option value="all">All Locations</option>
              {locations.map(loc => (
                <option key={loc} value={loc}>{loc}</option>
              ))}
            </select>
          </div>

          {loading ? (
            <div className="text-center py-12">
              <p className="text-gray-600 dark:text-gray-400">Loading positions...</p>
            </div>
          ) : filteredJobs.length === 0 ? (
            <div className="text-center py-12">
              <p className="text-gray-600 dark:text-gray-400">No open positions at the moment. Check back soon!</p>
            </div>
          ) : (
            <div className="space-y-6">
              {filteredJobs.map((position) => (
                <div
                  key={position.id}
                  className="p-8 bg-gray-50 dark:bg-gray-900 rounded-2xl shadow-sm hover:shadow-lg transition-all duration-300 hover:-translate-y-0.5"
                >
                  <div className="flex flex-col lg:flex-row lg:items-start lg:justify-between gap-6">
                    <div className="flex-1">
                      <div className="flex flex-wrap items-center gap-4 mb-3">
                        <h3 className="text-2xl font-semibold text-blue-900 dark:text-blue-300">
                          {position.title}
                        </h3>
                        <span className="px-3 py-1 bg-blue-100 dark:bg-blue-900 text-blue-900 dark:text-blue-300 rounded-full text-sm font-medium">
                          {position.employment_type}
                        </span>
                      </div>
                      <p className="text-gray-500 dark:text-gray-500 mb-4">
                        📍 {position.location}
                        {position.salary_range && ` | 💰 ${position.salary_range}`}
                      </p>
                      <p className="text-gray-600 dark:text-gray-400 mb-4">
                        {position.description}
                      </p>
                      <div className="mb-4">
                        <h4 className="font-semibold text-blue-900 dark:text-blue-300 mb-2">Requirements:</h4>
                        <p className="text-gray-600 dark:text-gray-400 text-sm whitespace-pre-line">
                          {position.requirements}
                        </p>
                      </div>
                    </div>
                    <div className="lg:w-auto">
                      <a
                        href={`/careers/${position.id}`}
                        className="inline-block px-6 py-3 bg-blue-600 hover:bg-blue-700 dark:bg-blue-700 dark:hover:bg-blue-600 text-white rounded-lg font-semibold transition-colors whitespace-nowrap mb-2"
                      >
                        View Details & Apply
                      </a>
                      <p className="text-sm text-gray-500 dark:text-gray-500 text-center">
                        {position.application_count} applicants
                      </p>
                    </div>
                  </div>
                </div>
              ))}
            </div>
          )}
        </div>
      </section>

      {/* Application Process */}
      <section ref={processRef} className="scroll-reveal relative py-24 px-4 sm:px-8 bg-gray-50 dark:bg-gray-900">
        <SectionBackground variant="grid" />
        <div className="relative max-w-4xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-darkgoldenrod dark:text-yellow-400 mb-12">
            Application Process
          </h2>
          <div className="bg-white dark:bg-gray-800 rounded-2xl p-8 shadow-sm">
            <ol className="space-y-6">
              <li className="flex gap-4">
                <span className="flex-shrink-0 w-10 h-10 bg-darkgoldenrod dark:bg-yellow-400 text-white dark:text-gray-900 rounded-full flex items-center justify-center font-bold">
                  1
                </span>
                <div>
                  <h3 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-2">
                    Submit Application
                  </h3>
                  <p className="text-gray-600 dark:text-gray-400">
                    Contact us via WhatsApp or email with your CV, portfolio, and position of interest
                  </p>
                </div>
              </li>
              <li className="flex gap-4">
                <span className="flex-shrink-0 w-10 h-10 bg-darkgoldenrod dark:bg-yellow-400 text-white dark:text-gray-900 rounded-full flex items-center justify-center font-bold">
                  2
                </span>
                <div>
                  <h3 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-2">
                    Initial Screening
                  </h3>
                  <p className="text-gray-600 dark:text-gray-400">
                    Brief phone or video call to discuss your experience and fit for the role
                  </p>
                </div>
              </li>
              <li className="flex gap-4">
                <span className="flex-shrink-0 w-10 h-10 bg-darkgoldenrod dark:bg-yellow-400 text-white dark:text-gray-900 rounded-full flex items-center justify-center font-bold">
                  3
                </span>
                <div>
                  <h3 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-2">
                    Technical Assessment
                  </h3>
                  <p className="text-gray-600 dark:text-gray-400">
                    Complete a practical assignment or technical interview relevant to the role
                  </p>
                </div>
              </li>
              <li className="flex gap-4">
                <span className="flex-shrink-0 w-10 h-10 bg-darkgoldenrod dark:bg-yellow-400 text-white dark:text-gray-900 rounded-full flex items-center justify-center font-bold">
                  4
                </span>
                <div>
                  <h3 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-2">
                    Final Interview & Offer
                  </h3>
                  <p className="text-gray-600 dark:text-gray-400">
                    Meet the team, discuss terms, and receive your offer to join Slyker Tech Web Services
                  </p>
                </div>
              </li>
            </ol>
          </div>
        </div>
      </section>

      {/* CTA Section */}
      <section ref={ctaRef} className="scroll-reveal relative py-24 px-4 sm:px-8 bg-gradient-to-r from-blue-600 to-purple-700 dark:from-blue-900 dark:to-purple-900">
        <SectionBackground variant="radial-glow" />
        <div className="relative max-w-4xl mx-auto text-center">
          <h2 className="text-4xl font-bold text-white mb-6">
            Ready to Join Our Team?
          </h2>
          <p className="text-xl text-blue-100 dark:text-blue-200 mb-12">
            Start your career journey with Slyker Tech Web Services and make an impact across Africa
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
              Email Your Application
            </a>
          </div>
        </div>
      </section>
    </div>
  );
}
