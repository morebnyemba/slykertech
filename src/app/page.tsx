import { Metadata } from 'next';
import HomePageClientView from './HomePageClientView';
import { generatePageMetadata, LOGO_IMAGE } from '@/lib/seo-config';
import { SITE_NAME, BASE_URL } from '@/lib/seo-config';

export const metadata: Metadata = generatePageMetadata({
    title: 'Grow Your African Business Online',
    description: 'Slyker Tech provides cutting-edge digital transformation services including cloud solutions, AI integration, and custom software development for enterprises worldwide.',
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