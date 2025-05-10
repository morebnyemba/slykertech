// app/services/page.tsx
import { Metadata } from 'next';
import ServicesClientView from './ServicesClientView';
import { generatePageMetadata } from '@/lib/seo-config';
import { SITE_NAME, BASE_URL } from '@/lib/seo-config';

export async function generateMetadata(): Promise<Metadata> {
  return generatePageMetadata({
    title: 'Our Services',
    description: 'Explore our comprehensive digital services for your business needs',
    url: '/services'
  });
}

export default function ServicesPage() {
  return (
    <>
      <ServicesClientView />
      <StructuredData />
    </>
  );
}

function StructuredData() {
  const jsonLd = {
    "@context": "https://schema.org",
    "@type": "WebPage",
    "name": `${SITE_NAME} Services`,
    "description": "Professional digital services for businesses",
    "url": `${BASE_URL}/services`
  };

  return (
    <script
      type="application/ld+json"
      dangerouslySetInnerHTML={{ __html: JSON.stringify(jsonLd) }}
    />
  );
}