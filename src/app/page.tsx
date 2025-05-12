import { Metadata } from 'next';
import HomePageClientView from './HomePageClientView'; // Corrected import path
import { generatePageMetadata } from '@/lib/seo-config';
import { SITE_NAME, BASE_URL } from '@/lib/seo-config';

export const metadata: Metadata = generatePageMetadata({
    title: 'Grow Your African Business Online with Expert Web Services & Digital Solutions',
    description: 'Slyker Tech provides cutting-edge digital transformation services including cloud solutions, AI integration, and custom software development for enterprises.',
    url: 'https://slykertech.co.zw'
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