import './globals.css';
import { defaultMetadata, SOCIAL_HANDLES } from '@/lib/seo-config';
import Header from '@/components/Header';
import Footer from '@/components/Footer';

export const metadata = defaultMetadata;

export default function RootLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  let logoUrl: string | undefined;

  // Safely extract Open Graph image URL
  const ogImages = metadata.openGraph?.images;

  if (Array.isArray(ogImages) && ogImages.length > 0) {
    const firstImage = ogImages[0];
    if (typeof firstImage === 'string') {
      logoUrl = firstImage;
    } else if ('url' in firstImage) {
      logoUrl = typeof firstImage.url === 'string' ? firstImage.url : firstImage.url.toString();
    }
  } else if (typeof ogImages === 'string') {
    logoUrl = ogImages;
  }

  // Build `sameAs` social links
  const sameAsLinks = Object.entries(SOCIAL_HANDLES).map(([platform, handle]) => {
    if (handle.includes('@')) {
      return `https://twitter.com/${handle.replace('@', '')}`;
    } else if (platform === 'linkedin') {
      return `https://linkedin.com/${handle}`;
    } else {
      return `https://${platform}.com/${handle}`;
    }
  });

  return (
    <html lang="en" className="h-full">
      <head>
        <meta name="viewport" content="width=device-width, initial-scale=1" />
        <link rel="icon" href="/favicon.ico" sizes="any" />
      </head>
      <body className="relative min-h-screen flex flex-col bg-gray-50 text-gray-900 font-sans">
        {/* Background Grid */}
        <div className="background-grid-circuit z-0" />

        {/* Optional Overlay Gradient */}
        <div className="background-overlay-gradient z-10" />

        {/* Foreground Content */}
        <div className="relative z-20 flex flex-col min-h-screen">
          <Header />
          <main className="flex-grow">
            <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
              {children}
            </div>
          </main>
          <Footer />
        </div>

        {/* Structured Data */}
        <script
          type="application/ld+json"
          dangerouslySetInnerHTML={{
            __html: JSON.stringify({
              "@context": "https://schema.org",
              "@type": "Organization",
              "name": metadata.title,
              "url": metadata.metadataBase?.toString() || "https://slykertech.co.zw",
              "logo": logoUrl,
              "sameAs": sameAsLinks
            }),
          }}
        />
      </body>
    </html>
  );
}
