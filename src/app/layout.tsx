import './globals.css';
import { defaultMetadata, SOCIAL_HANDLES } from '@/lib/seo-config';
import Header from '@/components/Header';
import Footer from '@/components/Footer';
import { ThemeProvider } from '@/components/ThemeProvider';
import ClientBackgroundWrapper from '@/components/ClientBackgroundWrapper'; // Import the wrapper

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
      return `https://<span class="math-inline">\{platform\}\.com/</span>{handle}`;
    }
  });

  return (
    <html lang="en" suppressHydrationWarning className="scroll-smooth">
      <head>
        <link rel="icon" href="/images/stws.jpeg" sizes="any" />
      </head>
      <body className="min-h-screen antialiased bg-background text-foreground">
        <ThemeProvider
          attribute="class"
          defaultTheme="system"
          enableSystem
          disableTransitionOnChange={false}
        >
          {/* Client-side only background elements */}
          <ClientBackgroundWrapper /> {/* Use the wrapper component */}

          {/* Static background layers */}
          <div className="fixed inset-0 -z-50 overflow-hidden">
            <div className="background-grid-circuit" />
            <div className="background-overlay-gradient" />
            <div className="absolute inset-0 bg-[radial-gradient(ellipse_at_center,var(--tw-gradient-stops))] from-primary/5 via-transparent to-transparent dark:from-primary/10" />
          </div>

          {/* Main content container */}
          <div className="relative flex min-h-screen flex-col">
            <Header />
            <main className="flex-1">
              <div className="mx-auto w-full max-w-7xl px-4 sm:px-6 lg:px-8 py-8">
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
        </ThemeProvider>
      </body>
    </html>
  );
}