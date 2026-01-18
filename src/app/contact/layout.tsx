import type { Metadata } from 'next';

export const metadata: Metadata = {
  title: 'Contact Us | Slyker Tech Web Services',
  description:
    'Get in touch with Slyker Tech Web Services for all your web development, hosting, and digital service needs. WhatsApp, email, or phone support available.',
  keywords: ['contact', 'support', 'web services', 'reach out', 'customer service'],
  openGraph: {
    title: 'Contact Us | Slyker Tech Web Services',
    description: 'Reach out to our expert team for professional web services',
    url: 'https://slykertech.co.zw/contact',
    type: 'website',
    images: [
      {
        url: 'https://slykertech.co.zw/images/stws.png',
        width: 490,
        height: 112,
        alt: 'Slyker Tech Web Services Logo',
      },
    ],
  },
  twitter: {
    card: 'summary',
    title: 'Contact Us | Slyker Tech Web Services',
    description: 'Get professional support from our team',
    images: ['https://slykertech.co.zw/images/stws.png'],
  },
  alternates: {
    canonical: 'https://slykertech.co.zw/contact'
  }
};

export default function ContactLayout({ children }: { children: React.ReactNode }) {
  return <>{children}</>;
}