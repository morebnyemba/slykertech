import './globals.css';
import { defaultMetadata, SOCIAL_HANDLES } from '@/lib/seo-config';
import { ThemeProvider } from '@/components/ThemeProvider';
import ConditionalLayout from '@/components/ConditionalLayout';
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
          {/* Conditional layout based on route */}
          <ConditionalLayout>
            {children}
          </ConditionalLayout>

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