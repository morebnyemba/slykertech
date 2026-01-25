import './globals.css';
import { defaultMetadata, SOCIAL_HANDLES } from '@/lib/seo-config';
import Header from '@/components/Header';
import Footer from '@/components/Footer';
import { ThemeProvider } from '@/components/ThemeProvider';
import ClientBackgroundWrapper from '@/components/ClientBackgroundWrapper'; // Import the wrapper
import LiveChatWidget from '@/components/LiveChatWidget';
import Script from 'next/script';

export const metadata = defaultMetadata;

// Google Analytics Measurement ID - Replace with your actual GA4 Measurement ID
const GA_MEASUREMENT_ID = process.env.NEXT_PUBLIC_GA_MEASUREMENT_ID;

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
        <link rel="icon" href="/images/stws.ico" sizes="any" />
        <link rel="apple-touch-icon" href="/images/stws.png" />
        <meta name="theme-color" content="#0066cc" />
        
        {/* Google Analytics - Only load if GA_MEASUREMENT_ID is set */}
        {GA_MEASUREMENT_ID && (
          <>
            <Script
              src={`https://www.googletagmanager.com/gtag/js?id=${GA_MEASUREMENT_ID}`}
              strategy="afterInteractive"
            />
            <Script id="google-analytics" strategy="afterInteractive">
              {`
                window.dataLayer = window.dataLayer || [];
                function gtag(){dataLayer.push(arguments);}
                gtag('js', new Date());
                gtag('config', '${GA_MEASUREMENT_ID}', {
                  page_path: window.location.pathname,
                });
              `}
            </Script>
          </>
        )}
        
        {/* 
          Additional Tracking Scripts Placeholder
          Add your tracking scripts here:
          - Facebook Pixel
          - Hotjar
          - Microsoft Clarity
          - Other analytics tools
          
          Example for Facebook Pixel:
          <Script id="facebook-pixel" strategy="afterInteractive">
            {`
              !function(f,b,e,v,n,t,s)
              {if(f.fbq)return;n=f.fbq=function(){n.callMethod?
              n.callMethod.apply(n,arguments):n.queue.push(arguments)};
              if(!f._fbq)f._fbq=n;n.push=n;n.loaded=!0;n.version='2.0';
              n.queue=[];t=b.createElement(e);t.async=!0;
              t.src=v;s=b.getElementsByTagName(e)[0];
              s.parentNode.insertBefore(t,s)}(window, document,'script',
              'https://connect.facebook.net/en_US/fbevents.js');
              fbq('init', 'YOUR_PIXEL_ID');
              fbq('track', 'PageView');
            `}
          </Script>
        */}
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
            {/* Spacer to prevent content from being cut off by fixed header */}
            {/* Responsive header height adjusts based on screen size */}
            <div className="h-32 sm:h-28 md:h-24 lg:h-24 flex-shrink-0" aria-hidden="true" />
            {/* Separator line between header and content */}
            <div className="w-full h-px bg-gradient-to-r from-transparent via-gray-300 dark:via-gray-600 to-transparent" aria-hidden="true" />
            <main className="flex-1">
              <div className="mx-auto w-full max-w-7xl px-3 sm:px-4 md:px-6 lg:px-8 py-4 sm:py-6 md:py-8">
                {children}
              </div>
            </main>
            <Footer />
          </div>

          {/* Live Chat Widget - Persistent across all pages */}
          <LiveChatWidget />

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