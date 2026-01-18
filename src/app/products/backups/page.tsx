import type { Metadata } from 'next';
import { FaDatabase, FaCloud, FaHistory, FaWhatsapp, FaEnvelope } from 'react-icons/fa';
import { MdBackup, MdRestore, MdSecurity, MdSpeed } from 'react-icons/md';
import { generatePageMetadata } from '@/lib/seo-config';

export const metadata: Metadata = generatePageMetadata({
  title: 'Backups & Disaster Recovery',
  description: 'Automated backups and disaster recovery for your business. Protect your data with daily backups, one-click restore, and 99.9% recovery guarantee across Africa.',
  url: '/products/backups'
});

export default function BackupsPage() {
  const whatsappMessage = encodeURIComponent(
    "Hi Slyker Tech! I'm interested in Backups & Disaster Recovery..."
  );

  const features = [
    {
      icon: <MdBackup className="w-10 h-10" />,
      title: 'Automated Backups',
      description: 'Schedule automatic backups daily, hourly, or in real-time without manual intervention'
    },
    {
      icon: <MdRestore className="w-10 h-10" />,
      title: 'One-Click Restore',
      description: 'Restore your entire website or specific files with a single click in minutes'
    },
    {
      icon: <FaCloud className="w-10 h-10" />,
      title: 'Offsite Storage',
      description: 'Backups stored in multiple geographic locations for maximum redundancy'
    },
    {
      icon: <MdSecurity className="w-10 h-10" />,
      title: 'Encrypted Backups',
      description: 'All backups encrypted in transit and at rest with AES-256 encryption'
    },
    {
      icon: <FaHistory className="w-10 h-10" />,
      title: 'Version History',
      description: 'Keep 30+ backup versions with point-in-time recovery options'
    },
    {
      icon: <MdSpeed className="w-10 h-10" />,
      title: 'Fast Recovery',
      description: 'RTO under 1 hour for critical business data and applications'
    }
  ];

  const plans = [
    {
      name: 'Basic',
      price: '$15',
      period: '/month',
      features: [
        'Daily Automated Backups',
        '30-Day Retention',
        '50GB Backup Storage',
        'One-Click Restore',
        'Email Notifications',
        'Email Support'
      ]
    },
    {
      name: 'Professional',
      price: '$49',
      period: '/month',
      popular: true,
      features: [
        'Hourly Automated Backups',
        '90-Day Retention',
        '500GB Backup Storage',
        'One-Click Restore',
        'Offsite Replication',
        'Encrypted Backups',
        'Priority Support',
        'Backup Verification'
      ]
    },
    {
      name: 'Enterprise',
      price: '$149',
      period: '/month',
      features: [
        'Real-time Backups',
        'Unlimited Retention',
        'Unlimited Storage',
        'Instant Recovery',
        'Multi-Region Replication',
        'Full Encryption',
        '24/7 Support',
        'Disaster Recovery Plan',
        'Compliance Reporting'
      ]
    }
  ];

  return (
    <div className="relative z-10">
      {/* Hero Section */}
      <section className="py-28 px-4 sm:px-8 md:px-16 lg:px-24 text-center bg-gradient-to-b from-blue-50 to-transparent dark:from-blue-950/50">
        <div className="max-w-5xl mx-auto">
          <FaDatabase className="w-20 h-20 mx-auto text-darkgoldenrod dark:text-yellow-400 mb-8" />
          <h1 className="text-5xl sm:text-6xl md:text-7xl font-extrabold tracking-tight text-blue-900 dark:text-blue-300 leading-tight">
            Backups & <span className="text-darkgoldenrod dark:text-yellow-400">Disaster Recovery</span>
          </h1>
          <p className="mt-8 text-lg sm:text-xl text-gray-700 dark:text-gray-300 max-w-3xl mx-auto">
            Protect your business with automated backups and enterprise-grade disaster recovery across Africa
          </p>
        </div>
      </section>

      {/* Features Section */}
      <section className="py-24 px-4 sm:px-8 bg-white dark:bg-gray-950">
        <div className="max-w-6xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-blue-900 dark:text-blue-300 mb-12">
            Complete Data Protection
          </h2>
          <div className="grid grid-cols-1 md:grid-cols-3 gap-8">
            {features.map((feature, index) => (
              <div
                key={index}
                className="p-8 bg-gray-50 dark:bg-gray-900 rounded-2xl hover:shadow-lg transition-shadow"
              >
                <div className="text-darkgoldenrod dark:text-yellow-400 mb-4">
                  {feature.icon}
                </div>
                <h3 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-3">
                  {feature.title}
                </h3>
                <p className="text-gray-600 dark:text-gray-400">
                  {feature.description}
                </p>
              </div>
            ))}
          </div>
        </div>
      </section>

      {/* Why Backups Matter */}
      <section className="py-24 px-4 sm:px-8 bg-gray-50 dark:bg-gray-900">
        <div className="max-w-6xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-darkgoldenrod dark:text-yellow-400 mb-12">
            Why Backups Are Critical
          </h2>
          <div className="grid md:grid-cols-2 gap-8">
            <div className="p-8 bg-white dark:bg-gray-800 rounded-2xl">
              <h3 className="text-2xl font-semibold text-blue-900 dark:text-blue-300 mb-6">
                Common Data Loss Scenarios
              </h3>
              <ul className="space-y-3 text-gray-600 dark:text-gray-400">
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">⚠</span>
                  Hardware failures and server crashes
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">⚠</span>
                  Ransomware and cyber attacks
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">⚠</span>
                  Accidental deletions by staff
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">⚠</span>
                  Software bugs and updates
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">⚠</span>
                  Natural disasters and power outages
                </li>
              </ul>
            </div>
            <div className="p-8 bg-white dark:bg-gray-800 rounded-2xl">
              <h3 className="text-2xl font-semibold text-blue-900 dark:text-blue-300 mb-6">
                Business Impact
              </h3>
              <ul className="space-y-3 text-gray-600 dark:text-gray-400">
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  60% of businesses close within 6 months of major data loss
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Average downtime costs $5,600 per minute for businesses
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  93% of companies without backups fail within one year of disaster
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-darkgoldenrod dark:text-yellow-400">✓</span>
                  Customer trust and reputation damage can be irreversible
                </li>
              </ul>
            </div>
          </div>
        </div>
      </section>

      {/* Disaster Recovery Process */}
      <section className="py-24 px-4 sm:px-8 bg-white dark:bg-gray-950">
        <div className="max-w-6xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-blue-900 dark:text-blue-300 mb-12">
            Our Recovery Process
          </h2>
          <div className="grid md:grid-cols-4 gap-6">
            <div className="p-6 bg-gray-50 dark:bg-gray-900 rounded-2xl">
              <div className="w-12 h-12 bg-darkgoldenrod dark:bg-yellow-400 text-white dark:text-gray-900 rounded-full flex items-center justify-center text-2xl font-bold mb-4">
                1
              </div>
              <h3 className="text-lg font-semibold text-blue-900 dark:text-blue-300 mb-2">
                Detect
              </h3>
              <p className="text-sm text-gray-600 dark:text-gray-400">
                Automatic monitoring detects data loss or corruption
              </p>
            </div>
            <div className="p-6 bg-gray-50 dark:bg-gray-900 rounded-2xl">
              <div className="w-12 h-12 bg-darkgoldenrod dark:bg-yellow-400 text-white dark:text-gray-900 rounded-full flex items-center justify-center text-2xl font-bold mb-4">
                2
              </div>
              <h3 className="text-lg font-semibold text-blue-900 dark:text-blue-300 mb-2">
                Alert
              </h3>
              <p className="text-sm text-gray-600 dark:text-gray-400">
                Instant notification sent to your team
              </p>
            </div>
            <div className="p-6 bg-gray-50 dark:bg-gray-900 rounded-2xl">
              <div className="w-12 h-12 bg-darkgoldenrod dark:bg-yellow-400 text-white dark:text-gray-900 rounded-full flex items-center justify-center text-2xl font-bold mb-4">
                3
              </div>
              <h3 className="text-lg font-semibold text-blue-900 dark:text-blue-300 mb-2">
                Restore
              </h3>
              <p className="text-sm text-gray-600 dark:text-gray-400">
                One-click restoration from backup
              </p>
            </div>
            <div className="p-6 bg-gray-50 dark:bg-gray-900 rounded-2xl">
              <div className="w-12 h-12 bg-darkgoldenrod dark:bg-yellow-400 text-white dark:text-gray-900 rounded-full flex items-center justify-center text-2xl font-bold mb-4">
                4
              </div>
              <h3 className="text-lg font-semibold text-blue-900 dark:text-blue-300 mb-2">
                Verify
              </h3>
              <p className="text-sm text-gray-600 dark:text-gray-400">
                Confirm data integrity and business continuity
              </p>
            </div>
          </div>
        </div>
      </section>

      {/* Pricing Section */}
      <section className="py-24 px-4 sm:px-8 bg-gray-50 dark:bg-gray-900">
        <div className="max-w-6xl mx-auto">
          <h2 className="text-4xl font-bold text-center text-darkgoldenrod dark:text-yellow-400 mb-12">
            Backup Plans
          </h2>
          <div className="grid grid-cols-1 md:grid-cols-3 gap-8">
            {plans.map((plan, index) => (
              <div
                key={index}
                className={`p-8 rounded-2xl ${
                  plan.popular
                    ? 'bg-gradient-to-br from-blue-600 to-purple-700 dark:from-blue-900 dark:to-purple-900 text-white shadow-xl scale-105'
                    : 'bg-white dark:bg-gray-800'
                }`}
              >
                {plan.popular && (
                  <div className="text-center mb-4">
                    <span className="px-4 py-1 bg-yellow-400 text-gray-900 rounded-full text-sm font-semibold">
                      Most Popular
                    </span>
                  </div>
                )}
                <h3
                  className={`text-2xl font-bold mb-4 ${
                    plan.popular ? 'text-white' : 'text-blue-900 dark:text-blue-300'
                  }`}
                >
                  {plan.name}
                </h3>
                <div className="mb-6">
                  <span className={`text-4xl font-bold ${plan.popular ? 'text-white' : 'text-darkgoldenrod dark:text-yellow-400'}`}>
                    {plan.price}
                  </span>
                  <span className={plan.popular ? 'text-blue-100' : 'text-gray-600 dark:text-gray-400'}>
                    {plan.period}
                  </span>
                </div>
                <ul className="space-y-3 mb-8">
                  {plan.features.map((feature, i) => (
                    <li key={i} className="flex items-start gap-2">
                      <span className={plan.popular ? 'text-yellow-400' : 'text-darkgoldenrod dark:text-yellow-400'}>
                        ✓
                      </span>
                      <span className={plan.popular ? 'text-blue-100' : 'text-gray-600 dark:text-gray-400'}>
                        {feature}
                      </span>
                    </li>
                  ))}
                </ul>
                <a
                  href={`https://wa.me/263787211325?text=${encodeURIComponent(`Hi! I'm interested in the ${plan.name} Backup plan...`)}`}
                  target="_blank"
                  rel="noopener noreferrer"
                  className={`block text-center px-6 py-3 rounded-lg font-semibold transition-colors ${
                    plan.popular
                      ? 'bg-white text-blue-900 hover:bg-gray-100'
                      : 'bg-blue-600 hover:bg-blue-700 dark:bg-blue-700 dark:hover:bg-blue-600 text-white'
                  }`}
                >
                  Get Started
                </a>
              </div>
            ))}
          </div>
        </div>
      </section>

      {/* CTA Section */}
      <section className="py-24 px-4 sm:px-8 bg-gradient-to-r from-blue-600 to-purple-700 dark:from-blue-900 dark:to-purple-900">
        <div className="max-w-4xl mx-auto text-center">
          <h2 className="text-4xl font-bold text-white mb-6">
            Don&apos;t Risk Your Data
          </h2>
          <p className="text-xl text-blue-100 dark:text-blue-200 mb-12">
            Protect your business with enterprise-grade backups and disaster recovery today
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
              Email Sales Team
            </a>
          </div>
        </div>
      </section>
    </div>
  );
}
