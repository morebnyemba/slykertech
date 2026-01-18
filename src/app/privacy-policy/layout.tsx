import type { Metadata } from 'next';
import { generatePageMetadata } from '@/lib/seo-config';

export async function generateMetadata(): Promise<Metadata> {
  return generatePageMetadata({
    title: 'Privacy Policy',
    description: 'Learn how Slyker Tech Web Services protects your personal data and complies with global privacy standards.',
    url: '/privacy-policy',
  });
}

export default function PrivacyPolicyLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  return <>{children}</>;
}
