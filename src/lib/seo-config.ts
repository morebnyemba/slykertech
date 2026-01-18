import type { Metadata } from 'next';

export const SITE_NAME = "Slyker Tech Web Services";
export const BASE_URL = "https://slykertech.co.zw";
// Align with solutionmerchants: use logo for OG/Twitter previews
export const DEFAULT_OG_IMAGE = "/images/stws.png";
export const TWITTER_IMAGE = "/images/stws.png";
export const LOGO_IMAGE = "/images/stws.png";
export const FAVICON = "/images/stws.ico";

export const SOCIAL_HANDLES = {
  twitter: "@SlykerTech",
  facebook: "SlykerTech",
  linkedin: "company/slykertech",
  instagram: "slykertech",
  whatsapp: "+263787211325"
};

// Helper function to format titles
const formatTitle = (pageTitle?: string) => 
  pageTitle ? `${pageTitle} | ${SITE_NAME}` : SITE_NAME;

export const defaultMetadata: Metadata = {
  title: SITE_NAME,
  description: "Professional web hosting, domain services, web development, and design solutions. Trusted global provider of digital services.",
  keywords: ["web hosting", "domain registration", "web development", "web design", "digital services", "cloud hosting", "website development", "e-commerce solutions"],
  metadataBase: new URL(BASE_URL),
  authors: [{ name: "Slyker Tech", url: BASE_URL }],
  creator: "Slyker Tech",
  publisher: "Slyker Tech",
  formatDetection: {
    email: false,
    address: false,
    telephone: false,
  },
  viewport: {
    width: 'device-width',
    initialScale: 1,
    maximumScale: 5,
  },
  robots: {
    index: true,
    follow: true,
    googleBot: {
      index: true,
      follow: true,
      'max-image-preview': 'large',
      'max-snippet': -1,
      'max-video-preview': -1,
    },
  },
  icons: {
    icon: FAVICON,
    apple: LOGO_IMAGE,
  },
  alternates: {
    canonical: BASE_URL
  },
  openGraph: {
    type: 'website',
    locale: 'en_US',
    siteName: SITE_NAME,
    title: SITE_NAME,
    description: "Your complete digital partner for web services, hosting, domains, and development",
    url: BASE_URL,
    emails: ['support@slykertech.co.zw'],
    phoneNumbers: ['+263787211325'],
    images: [
      {
        url: LOGO_IMAGE,
        width: 490,
        height: 112,
        alt: `${SITE_NAME} Logo`,
        type: 'image/png'
      }
    ]
  },
  twitter: {
    card: 'summary_large_image',
    site: SOCIAL_HANDLES.twitter,
    creator: SOCIAL_HANDLES.twitter,
    title: SITE_NAME,
    description: "Professional web services and digital solutions",
    images: [TWITTER_IMAGE]
  }
};

export const generatePageMetadata = (meta: {
  title: string;
  description?: string;
  url?: string;
  images?: string[];
}): Metadata => ({
  ...defaultMetadata,
  title: formatTitle(meta.title),
  description: meta.description || defaultMetadata.description,
  openGraph: {
    ...defaultMetadata.openGraph,
    type: 'website',
    title: formatTitle(meta.title),
    description: meta.description || defaultMetadata.description,
    url: meta.url ? `${BASE_URL}${meta.url}` : BASE_URL,
    images: meta.images?.map(img => ({
      url: img,
      width: 490,
      height: 112,
      alt: formatTitle(meta.title),
      type: 'image/png'
    })) || (defaultMetadata.openGraph?.images as any[])
  },
  twitter: {
    ...defaultMetadata.twitter,
    // Solutionmerchants uses summary for most pages; mirror that
    card: 'summary',
    title: formatTitle(meta.title),
    description: meta.description || defaultMetadata.description,
    images: meta.images || [TWITTER_IMAGE]
  },
  alternates: {
    canonical: meta.url ? `${BASE_URL}${meta.url}` : BASE_URL
  }
});