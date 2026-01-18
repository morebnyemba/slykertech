// app/about/metadata.ts
import { Metadata } from 'next';

const metadata: Metadata = {
  title: 'About Us | Slyker Tech Web Services',
  description: 'Discover Slyker Tech\'s mission, vision, and expertise in providing professional web development, hosting, and digital solutions across Africa and globally.',
  keywords: ['about us', 'Slyker Tech', 'digital solutions', 'web services', 'professional team'],
  openGraph: {
    title: 'About Us | Slyker Tech Web Services',
    description: 'Learn about our team, mission, and commitment to digital excellence',
    url: 'https://www.slykertech.co.zw/about',
    type: 'website',
    siteName: 'Slyker Tech Web Services',
    images: [
      {
        url: 'https://www.slykertech.co.zw/images/og-preview.png',
        width: 1200,
        height: 630,
        alt: 'Slyker Tech About Page',
        type: 'image/png'
      },
    ],
  },
  twitter: {
    card: 'summary_large_image',
    title: 'About Us | Slyker Tech Web Services',
    description: 'Learn about our team and digital excellence commitment',
    images: ['https://www.slykertech.co.zw/images/og-preview-twitter.png'],
  },
};
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
