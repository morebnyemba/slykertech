// app/about/metadata.ts
import { Metadata } from 'next';

const metadata: Metadata = {
  title: 'About Us | Slyker Tech Web Services',
  description: 'Learn more about Slyker Tech Web Services and our commitment to innovative web solutions in Zimbabwe.',
  keywords: ['Slyker Tech', 'web development', 'Zimbabwe', 'software services', 'digital solutions'],
  openGraph: {
    title: 'About Us | Slyker Tech Web Services',
    description: 'Discover our mission, vision, and the services we provide across Zimbabwe and beyond.',
    url: 'https://www.slykertech.co.zw/about',
    siteName: 'Slyker Tech Web Services',
    images: [
      {
        url: 'https://www.slykertech.co.zw/images/og-about.jpg',
        width: 1200,
        height: 630,
        alt: 'Slyker Tech About Page',
      },
    ],
    locale: 'en_ZW',
    type: 'website',
  },
  twitter: {
    card: 'summary_large_image',
    title: 'About Us | Slyker Tech Web Services',
    description: 'Meet the team behind Zimbabwe’s premier tech solutions provider.',
    images: ['https://www.slykertech.co.zw/images/og-about.jpg'],
  },
};

export default metadata;
