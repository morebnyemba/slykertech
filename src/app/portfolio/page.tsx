// app/portfolio/page.tsx
'use client';

import { Metadata } from 'next';
import Link from 'next/link';
import { FiMail, FiMessageSquare, FiGithub, FiCode, FiLayers } from 'react-icons/fi';

export const metadata: Metadata = {
  title: 'Portfolio | Slyker Tech Web Services',
  description: 'View our portfolio of successful web development projects, design work, and digital solutions delivered to clients worldwide.',
  keywords: ['portfolio', 'projects', 'case studies', 'web development', 'design'],
  openGraph: {
    title: 'Portfolio | Slyker Tech Web Services',
    description: 'Explore our professional projects and digital solutions portfolio',
    url: 'https://slykertech.co.zw/portfolio',
    type: 'website',
    images: [{
      url: '/images/og-preview.png',
      width: 1200,
      height: 630,
      alt: 'Slyker Tech Portfolio'
    }]
  },
  twitter: {
    card: 'summary_large_image',
    title: 'Portfolio | Slyker Tech Web Services',
    description: 'View our successful projects',
    images: ['/images/og-preview-twitter.png']
  }
};

export default function Portfolio() {
  return (
    <>
      <main className="min-h-screen">
        {/* Hero Section */}
        <section className="relative py-20 bg-gradient-to-r from-blue-600 to-blue-800 dark:from-blue-900 dark:to-blue-950 text-white">
          <div className="container mx-auto px-6 text-center">
            <h1 className="text-4xl md:text-5xl font-bold mb-6">Our Portfolio</h1>
            <p className="text-xl md:text-2xl max-w-3xl mx-auto">
              We&apos;re curating our best work to showcase here
            </p>
          </div>
        </section>

        {/* Under Development Section */}
        <section className="py-16 container mx-auto px-6">
          <div className="max-w-3xl mx-auto text-center bg-white dark:bg-gray-800 rounded-xl p-8 md:p-12 shadow-lg">
            <div className="text-6xl mb-6">🚧</div>
            <h2 className="text-2xl md:text-3xl font-bold text-gray-900 dark:text-white mb-4">
              Portfolio Under Construction
            </h2>
            <p className="text-gray-600 dark:text-gray-300 mb-8 text-lg">
              We&lsquo;re carefully selecting our best projects to showcase here. In the meantime, we&apos;d love to share relevant work samples based on your specific needs.
            </p>
            
            <div className="grid grid-cols-1 md:grid-cols-2 gap-6 mb-10">
              <div className="bg-gray-50 dark:bg-gray-700 p-6 rounded-lg">
                <FiLayers className="w-10 h-10 text-blue-600 dark:text-blue-400 mb-4 mx-auto" />
                <h3 className="font-bold text-lg mb-2">Request Samples</h3>
                <p className="text-gray-600 dark:text-gray-300">
                  Tell us about your project and we&apos;ll share relevant case studies
                </p>
              </div>
              <div className="bg-gray-50 dark:bg-gray-700 p-6 rounded-lg">
                <FiCode className="w-10 h-10 text-blue-600 dark:text-blue-400 mb-4 mx-auto" />
                <h3 className="font-bold text-lg mb-2">GitHub</h3>
                <p className="text-gray-600 dark:text-gray-300">
                  Check out our open-source contributions
                </p>
              </div>
            </div>

            <div className="flex flex-col sm:flex-row justify-center gap-4">
              <a
                href="mailto:contact@slykertech.co.zw"
                className="flex items-center justify-center gap-2 px-6 py-3 bg-blue-600 hover:bg-blue-700 text-white font-medium rounded-lg transition-colors"
              >
                <FiMail className="w-5 h-5" />
                Email Us
              </a>
              <a
                href="https://wa.me/263787211325"
                target="_blank"
                rel="noopener noreferrer"
                className="flex items-center justify-center gap-2 px-6 py-3 border-2 border-blue-600 text-blue-600 dark:border-blue-400 dark:text-blue-400 hover:bg-blue-50 dark:hover:bg-blue-900/20 rounded-lg font-medium transition-colors"
              >
                <FiMessageSquare className="w-5 h-5" />
                WhatsApp
              </a>
              <a
                href="https://github.com/morebnyemba/zimdevelopers.com/tree/main"
                target="_blank"
                rel="noopener noreferrer"
                className="flex items-center justify-center gap-2 px-6 py-3 border-2 border-gray-800 text-gray-800 dark:border-gray-300 dark:text-gray-300 hover:bg-gray-100 dark:hover:bg-gray-700 rounded-lg font-medium transition-colors"
              >
                <FiGithub className="w-5 h-5" />
                GitHub
              </a>
            </div>
          </div>
        </section>

        {/* CTA Section */}
        <section className="py-16 bg-gray-100 dark:bg-gray-800">
          <div className="container mx-auto px-6 text-center">
            <h2 className="text-2xl md:text-3xl font-bold text-gray-900 dark:text-white mb-6">
              Ready to discuss your project?
            </h2>
            <p className="text-gray-600 dark:text-gray-300 max-w-2xl mx-auto mb-8">
              We&apos;re happy to provide custom samples and references based on your requirements.
            </p>
            <Link
              href="/contact"
              className="inline-block px-8 py-3 bg-blue-600 hover:bg-blue-700 text-white font-medium rounded-lg transition-colors"
            >
              Contact Us
            </Link>
          </div>
        </section>
      </main>
    </>
  );
}