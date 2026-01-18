import { Metadata } from 'next';
import HomePageClientView from './HomePageClientView';
import { generatePageMetadata, LOGO_IMAGE } from '@/lib/seo-config';
import { SITE_NAME, BASE_URL } from '@/lib/seo-config';

export const metadata: Metadata = generatePageMetadata({
    title: 'Digital Transformation Across Africa & Beyond',
    description: 'Slyker Tech Web Services delivers enterprise-grade cloud solutions, AI integration, and custom software development across Africa and globally. From Cape Town to Cairo, we power business growth with cutting-edge technology.',
    url: '/',
    images: [LOGO_IMAGE]
});

export default function HomePage() {
    return (
        <>
            <HomePageClientView />
            <StructuredData />
        </>
    );
}

function StructuredData() {
    const jsonLd = {
        "@context": "https://schema.org",
        "@type": "WebPage",
        "name": `${SITE_NAME} Home`,
        "description": "Enterprise digital transformation services",
        "url": `${BASE_URL}`
    };

    return (
        <script
            type="application/ld+json"
            dangerouslySetInnerHTML={{ __html: JSON.stringify(jsonLd) }}
        />
    );
}