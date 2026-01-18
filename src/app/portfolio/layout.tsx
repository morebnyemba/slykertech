import type { Metadata } from 'next';

export const metadata: Metadata = {
  title: 'Portfolio | Slyker Tech Web Services',
  description:
    'View our portfolio of successful web development projects, design work, and digital solutions delivered to clients worldwide.',
  keywords: ['portfolio', 'projects', 'case studies', 'web development', 'design'],
  openGraph: {
    title: 'Portfolio | Slyker Tech Web Services',
    description: 'Explore our professional projects and digital solutions portfolio',
    url: 'https://slykertech.co.zw/portfolio',
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
    title: 'Portfolio | Slyker Tech Web Services',
    description: 'View our successful projects',
    images: ['https://slykertech.co.zw/images/stws.png'],
  },
  alternates: {
    canonical: 'https://slykertech.co.zw/portfolio'
  }
};

export default function PortfolioLayout({ children }: { children: React.ReactNode }) {
  return <>{children}</>;
}