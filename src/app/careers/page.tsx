import type { Metadata } from 'next';
import { FaUsers, FaHeart, FaRocket, FaWhatsapp, FaEnvelope } from 'react-icons/fa';
import { MdWork, MdTrendingUp, MdHealthAndSafety } from 'react-icons/md';
import { generatePageMetadata } from '@/lib/seo-config';

export const metadata: Metadata = generatePageMetadata({
  title: 'Careers',
  description: 'Join the Slyker Tech Web Services team and help drive digital transformation across Africa. Explore career opportunities in software development, cloud infrastructure, and fintech.',
  url: '/careers'
});

export default function CareersPage() {
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

  const openPositions = [
    {
      title: 'Senior Full-Stack Developer',
      location: 'Remote (Africa)',
      type: 'Full-time',
      description: 'Build enterprise cloud solutions using React, Node.js, and modern infrastructure. 5+ years experience required.',
      skills: ['React', 'Node.js', 'TypeScript', 'Docker', 'AWS/DigitalOcean']
    },
    {
      title: 'DevOps Engineer',
      location: 'Remote (Africa)',
      type: 'Full-time',
      description: 'Manage cloud infrastructure, CI/CD pipelines, and ensure 99.9% uptime for client services.',
      skills: ['Kubernetes', 'Docker', 'CI/CD', 'Linux', 'Monitoring']
    },
    {
      title: 'Fintech Integration Specialist',
      location: 'Harare / Remote',
      type: 'Full-time',
      description: 'Integrate payment gateways, mobile money solutions, and financial APIs across African markets.',
      skills: ['API Integration', 'Payment Systems', 'Mobile Money', 'Security']
    },
    {
      title: 'UI/UX Designer',
      location: 'Remote (Africa)',
      type: 'Contract',
      description: 'Design intuitive interfaces for web and mobile applications serving African users.',
      skills: ['Figma', 'User Research', 'Prototyping', 'Responsive Design']
    },
    {
      title: 'Technical Support Engineer',
      location: 'Harare',
      type: 'Full-time',
      description: 'Provide technical support to clients across Africa via WhatsApp, email, and client portal.',
      skills: ['Customer Support', 'Technical Troubleshooting', 'Communication']
    }
  ];

  return (
    <div className="relative z-10">
      {/* Hero Section */}
      <section className="py-28 px-4 sm:px-8 md:px-16 lg:px-24 text-center bg-gradient-to-b from-blue-50 to-transparent dark:from-blue-950/50">
        <div className="max-w-5xl mx-auto">
          <h1 className="text-5xl sm:text-6xl md:text-7xl font-extrabold tracking-tight text-blue-900 dark:text-blue-300 leading-tight">
            Build the <span className="text-darkgoldenrod dark:text-yellow-400">Future of Africa</span>
          </h1>
          <p className="mt-8 text-lg sm:text-xl text-gray-700 dark:text-gray-300 max-w-3xl mx-auto">
            Join our team of innovators driving digital transformation across Africa with enterprise cloud solutions, fintech, and cutting-edge technology
          </p>
        </div>
      </section>

      {/* Company Culture */}
      <section className="py-24 px-4 sm:px-8 bg-white dark:bg-gray-950">
        <div className="max-w-6xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-blue-900 dark:text-blue-300 mb-12">
            Why Work at Slyker Tech Web Services
          </h2>
          <div className="grid grid-cols-1 md:grid-cols-3 gap-8 mb-16">
            <div className="p-8 bg-gray-50 dark:bg-gray-900 rounded-2xl hover:shadow-lg transition-shadow text-center">
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

            <div className="p-8 bg-gray-50 dark:bg-gray-900 rounded-2xl hover:shadow-lg transition-shadow text-center">
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

            <div className="p-8 bg-gray-50 dark:bg-gray-900 rounded-2xl hover:shadow-lg transition-shadow text-center">
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
      <section className="py-24 px-4 sm:px-8 bg-gray-50 dark:bg-gray-900">
        <div className="max-w-6xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-darkgoldenrod dark:text-yellow-400 mb-12">
            Benefits & Perks
          </h2>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-8">
            {benefits.map((benefit, index) => (
              <div
                key={index}
                className="p-8 bg-white dark:bg-gray-800 rounded-2xl shadow-sm hover:shadow-md transition-shadow"
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
      <section className="py-24 px-4 sm:px-8 bg-white dark:bg-gray-950">
        <div className="max-w-6xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-blue-900 dark:text-blue-300 mb-12">
            Open Positions
          </h2>
          <div className="space-y-6">
            {openPositions.map((position, index) => (
              <div
                key={index}
                className="p-8 bg-gray-50 dark:bg-gray-900 rounded-2xl shadow-sm hover:shadow-md transition-shadow"
              >
                <div className="flex flex-col lg:flex-row lg:items-start lg:justify-between gap-6">
                  <div className="flex-1">
                    <div className="flex flex-wrap items-center gap-4 mb-3">
                      <h3 className="text-2xl font-semibold text-blue-900 dark:text-blue-300">
                        {position.title}
                      </h3>
                      <span className="px-3 py-1 bg-blue-100 dark:bg-blue-900 text-blue-900 dark:text-blue-300 rounded-full text-sm font-medium">
                        {position.type}
                      </span>
                    </div>
                    <p className="text-gray-500 dark:text-gray-500 mb-4">
                      📍 {position.location}
                    </p>
                    <p className="text-gray-600 dark:text-gray-400 mb-4">
                      {position.description}
                    </p>
                    <div className="flex flex-wrap gap-2">
                      {position.skills.map((skill, i) => (
                        <span
                          key={i}
                          className="px-3 py-1 bg-darkgoldenrod/10 dark:bg-yellow-400/10 text-darkgoldenrod dark:text-yellow-400 rounded-lg text-sm"
                        >
                          {skill}
                        </span>
                      ))}
                    </div>
                  </div>
                  <div className="lg:w-auto">
                    <a
                      href={`https://wa.me/263787211325?text=${encodeURIComponent(`Hi! I'm interested in the ${position.title} position...`)}`}
                      target="_blank"
                      rel="noopener noreferrer"
                      className="inline-block px-6 py-3 bg-blue-600 hover:bg-blue-700 dark:bg-blue-700 dark:hover:bg-blue-600 text-white rounded-lg font-semibold transition-colors whitespace-nowrap"
                    >
                      Apply Now
                    </a>
                  </div>
                </div>
              </div>
            ))}
          </div>
        </div>
      </section>

      {/* Application Process */}
      <section className="py-24 px-4 sm:px-8 bg-gray-50 dark:bg-gray-900">
        <div className="max-w-4xl mx-auto">
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
      <section className="py-24 px-4 sm:px-8 bg-gradient-to-r from-blue-600 to-purple-700 dark:from-blue-900 dark:to-purple-900">
        <div className="max-w-4xl mx-auto text-center">
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
