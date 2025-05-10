import { generatePageMetadata } from '@/lib/seo-config';
import Link from 'next/link';

export async function generateMetadata() {
  return generatePageMetadata({
    title: 'Customer-Friendly Terms',
    description: 'Fair terms that protect both you and our ability to improve services',
    url: '/terms-and-conditions',
    keywords: ['terms of service', 'user agreement', 'service terms'],
  });
}

export default function TermsAndConditions() {
  return (
    <>
      {/* Hero Section */}
      <section className="relative py-20 bg-gradient-to-r from-blue-600 to-blue-800 dark:from-blue-900 dark:to-blue-950 text-white">
        <div className="container mx-auto px-6 text-center">
          <h1 className="text-4xl md:text-5xl font-bold mb-6">Fair Terms of Service</h1>
          <p className="text-xl md:text-2xl max-w-3xl mx-auto">
            Transparent agreements that benefit everyone
          </p>
        </div>
      </section>

      {/* Main Content */}
      <section className="py-16 container mx-auto px-6">
        <div className="max-w-4xl mx-auto bg-white dark:bg-gray-800 rounded-xl p-8 md:p-12 shadow-lg">
          <div className="prose dark:prose-invert max-w-none">
            <div className="text-center mb-12">
              <p className="text-lg mb-6">
                We believe in simple, fair terms that respect your rights while allowing us to deliver great services.
              </p>
              <div className="inline-flex items-center bg-green-100 dark:bg-green-900/30 text-green-800 dark:text-green-200 px-4 py-2 rounded-full text-sm font-medium">
                <svg className="w-4 h-4 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M5 13l4 4L19 7" />
                </svg>
                Last updated: {new Date().toLocaleDateString()}
              </div>
            </div>

            {/* Terms Sections */}
            <div className="space-y-10">
              <section>
                <h2 className="text-2xl font-bold mb-4 text-blue-600 dark:text-blue-400">1. Your Rights</h2>
                <ul className="list-disc pl-6 space-y-3">
                  <li><strong>Transparent Pricing:</strong> No hidden fees - all costs explained upfront</li>
                  <li><strong>Service Guarantee:</strong> We&apos;ll fix any service issues within agreed timelines</li>
                  <li><strong>Data Control:</strong> You decide what personal information to share (see our <Link href="/privacy" className="text-blue-600 dark:text-blue-400 hover:underline">Privacy Policy</Link>)</li>
                  <li><strong>Cancel Anytime:</strong> Most services can be stopped with 30 days notice</li>
                </ul>
              </section>

              <section>
                <h2 className="text-2xl font-bold mb-4 text-blue-600 dark:text-blue-400">2. Our Commitments</h2>
                <ul className="list-disc pl-6 space-y-3">
                  <li><strong>Quality Service:</strong> We meet industry standards for all deliverables</li>
                  <li><strong>Security:</strong> Enterprise-grade protection for your data</li>
                  <li><strong>Support:</strong> Responsive help when you need it</li>
                  <li><strong>Continuous Improvement:</strong> We regularly enhance our services</li>
                </ul>
              </section>

              <section className="bg-blue-50 dark:bg-blue-900/20 rounded-xl p-6 -mx-2">
                <h2 className="text-2xl font-bold mb-4 text-blue-600 dark:text-blue-400">3. Service Enhancements</h2>
                <p className="mb-4">
                  To maintain and improve our services, we retain the right to:
                </p>
                <ul className="list-disc pl-6 space-y-3">
                  <li><strong>Embed analytics tools</strong> in our products to monitor performance, identify issues, and understand usage patterns</li>
                  <li><strong>Include marketing mechanisms</strong> such as (but not limited to) service announcements, upgrade offers, or promotional content within our interfaces</li>
                  <li><strong>Implement tracking technologies</strong> like cookies, pixels, or similar mechanisms to optimize service delivery</li>
                  <li><strong>Collect anonymized usage data</strong> to guide product development decisions</li>
                </ul>
                <p className="mt-4 text-sm bg-white dark:bg-gray-800 p-3 rounded-lg border border-blue-100 dark:border-blue-900">
                  <span className="font-semibold">Note:</span> These mechanisms are implemented with respect for your privacy. You can control many tracking features through our <Link href="/privacy#cookies" className="text-blue-600 dark:text-blue-400 hover:underline">Cookie Settings</Link>.
                </p>
              </section>

              <section>
                <h2 className="text-2xl font-bold mb-4 text-blue-600 dark:text-blue-400">4. Fair Usage</h2>
                <ul className="list-disc pl-6 space-y-3">
                  <li><strong>Our IP:</strong> We own the underlying technology, but you own your content</li>
                  <li><strong>Your Content:</strong> You retain rights to materials you provide</li>
                  <li><strong>Third-Party Services:</strong> Some features may rely on external providers</li>
                </ul>
              </section>

              <section>
                <h2 className="text-2xl font-bold mb-4 text-blue-600 dark:text-blue-400">5. Balanced Responsibilities</h2>
                <ul className="list-disc pl-6 space-y-3">
                  <li><strong>Accurate Information:</strong> You agree to provide truthful details</li>
                  <li><strong>Lawful Use:</strong> Services won&apos;t be used for illegal activities</li>
                  <li><strong>Security:</strong> You&apos;ll keep login credentials secure</li>
                </ul>
              </section>

              <section className="bg-gray-50 dark:bg-gray-700 rounded-xl p-6 -mx-2">
                <h2 className="text-2xl font-bold mb-4 text-blue-600 dark:text-blue-400">6. Changes to Terms</h2>
                <p className="mb-3">We may update these terms to reflect:</p>
                <ul className="list-disc pl-6 space-y-2 mb-4">
                  <li>New service features</li>
                  <li>Legal or regulatory requirements</li>
                  <li>Industry best practices</li>
                </ul>
                <p>
                  Significant changes will be communicated via email or in-app notice at least <strong>30 days</strong> in advance.
                </p>
              </section>

              <section className="text-center border-t pt-8 mt-6">
                <h3 className="text-xl font-semibold mb-4">Have Questions?</h3>
                <p className="mb-6">
                  We&apos;re happy to clarify any part of these terms.
                </p>
                <div className="flex flex-wrap justify-center gap-4">
                  <Link href="/contact" className="px-6 py-3 bg-blue-600 hover:bg-blue-700 text-white rounded-lg font-medium">
                    Contact Us
                  </Link>
                  <Link href="/privacy" className="px-6 py-3 border border-gray-300 dark:border-gray-600 rounded-lg font-medium">
                    View Privacy Policy
                  </Link>
                </div>
              </section>
            </div>
          </div>
        </div>
      </section>
    </>
  );
}
