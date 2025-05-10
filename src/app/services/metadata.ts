import type { Metadata } from 'next';

export const SITE_NAME = "Slyker Tech Web Services";
export const BASE_URL = "https://slykertech.co.zw";
export const DEFAULT_OG_IMAGE = `${BASE_URL}/images/logo-og.jpg`; // Made absolute

export const SOCIAL_HANDLES = {
  twitter: "@SlykerTech",
  facebook: "SlykerTech",
  linkedin: "company/slykertech",
  instagram: "slykertech"
};

const formatTitle = (pageTitle?: string) => 
  pageTitle ? `${pageTitle} | ${SITE_NAME}` : SITE_NAME;

export const defaultMetadata: Metadata = {
  title: SITE_NAME,
  description: "Complete digital solutions provider offering professional web services worldwide",
  metadataBase: new URL(BASE_URL),
  alternates: {
    canonical: BASE_URL
  },
  openGraph: {
    type: 'website',
    siteName: SITE_NAME,
    title: SITE_NAME,
    description: "Your complete digital partner for all web services",
    images: [{
      url: DEFAULT_OG_IMAGE,
      width: 1200,
      height: 630,
      alt: SITE_NAME
    }]
  },
  twitter: {
    card: 'summary_large_image',
    site: SOCIAL_HANDLES.twitter,
    creator: SOCIAL_HANDLES.twitter,
    title: SITE_NAME,
    description: "One-stop solution for all digital needs",
    images: [{  // Made consistent with OpenGraph format
      url: DEFAULT_OG_IMAGE,
      width: 1200,
      height: 630,
      alt: SITE_NAME
    }]
  }
};

export const generatePageMetadata = (meta: {
  title: string;
  description?: string;
  url?: string;
  images?: string[];
}): Metadata => {
  const images = meta.images?.map(img => ({
    url: img.startsWith('http') ? img : `${BASE_URL}${img}`,
    width: 1200,
    height: 630,
    alt: formatTitle(meta.title)
  })) || defaultMetadata.openGraph?.images;

  return {
    ...defaultMetadata,
    title: formatTitle(meta.title),
    description: meta.description || defaultMetadata.description,
    ...(meta.url && {
      alternates: {
        canonical: meta.url
      }
    }),
    openGraph: {
      ...defaultMetadata.openGraph,
      title: formatTitle(meta.title),
      description: meta.description || defaultMetadata.description,
      url: meta.url || BASE_URL,
      images
    },
    twitter: {
      ...defaultMetadata.twitter,
      title: formatTitle(meta.title),
      description: meta.description || defaultMetadata.description,
      images
    }
  };
};