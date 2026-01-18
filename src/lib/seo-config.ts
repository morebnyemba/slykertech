import type { Metadata } from 'next';

export const SITE_NAME = "Slyker Tech Web Services";
export const BASE_URL = "https://slykertech.co.zw";
export const DEFAULT_OG_IMAGE = "/images/stws.png";
export const FAVICON = "/images/stws.ico";

export const SOCIAL_HANDLES = {
  twitter: "@SlykerTech",
  facebook: "SlykerTech",
  linkedin: "company/slykertech",
  instagram: "slykertech"
};

// Helper function to format titles
const formatTitle = (pageTitle?: string) => 
  pageTitle ? `${pageTitle} | ${SITE_NAME}` : SITE_NAME;

export const defaultMetadata: Metadata = {
  title: SITE_NAME,
  description: "Professional web hosting, domain services, web development, and design solutions. Trusted global provider of digital services.",
  keywords: ["web hosting", "domain registration", "web development", "web design", "digital services", "cloud hosting"],
  metadataBase: new URL(BASE_URL),
  authors: [{ name: "Slyker Tech", url: BASE_URL }],
  creator: "Slyker Tech",
  publisher: "Slyker Tech",
  formatDetection: {
    email: false,
    address: false,
    telephone: false,
  },
  icons: {
    icon: FAVICON,
    apple: "/images/stws.png",
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
    images: [{
      url: DEFAULT_OG_IMAGE,
      width: 1200,
      height: 630,
      alt: SITE_NAME,
      type: 'image/png'
    }]
  },
  twitter: {
    card: 'summary_large_image',
    site: SOCIAL_HANDLES.twitter,
    creator: SOCIAL_HANDLES.twitter,
    title: SITE_NAME,
    description: "Professional web services and digital solutions",
    images: [DEFAULT_OG_IMAGE]
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
    title: formatTitle(meta.title),
    description: meta.description || defaultMetadata.description,
    url: meta.url || BASE_URL,
    images: meta.images?.map(img => ({
      url: img,
      width: 1200,
      height: 630,
      alt: formatTitle(meta.title)
    })) || defaultMetadata.openGraph?.images
  },
  twitter: {
    ...defaultMetadata.twitter,
    title: formatTitle(meta.title),
    description: meta.description || defaultMetadata.description,
    images: meta.images || defaultMetadata.twitter?.images
  }
});