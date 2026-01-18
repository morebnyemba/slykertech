// app/about/metadata.ts
import { Metadata } from 'next';

const metadata: Metadata = {
  title: 'About Us | Slyker Tech Web Services',
  description:
    "Discover Slyker Tech Web Services' mission, vision, and expertise in providing professional web development, hosting, and digital solutions across Africa and globally.",
  keywords: ['about us', 'Slyker Tech Web Services', 'digital solutions', 'web services', 'professional team'],
  openGraph: {
    title: 'About Us | Slyker Tech Web Services',
    description: 'Learn about our team, mission, and commitment to digital excellence',
    url: 'https://slykertech.co.zw/about',
    type: 'website',
    siteName: 'Slyker Tech Web Services',
    images: [
      {
        url: 'https://slykertech.co.zw/images/stws.png',
        width: 490,
        height: 112,
        alt: 'Slyker Tech Web Services Logo',
        type: 'image/png',
      },
    ],
  },
  twitter: {
    card: 'summary',
    title: 'About Us | Slyker Tech Web Services',
    description: 'Learn about our team and digital excellence commitment',
    images: ['https://slykertech.co.zw/images/stws.png'],
  },
  alternates: {
    canonical: 'https://slykertech.co.zw/about'
  }
};

export default metadata;
