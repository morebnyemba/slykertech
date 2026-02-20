import type { Metadata } from 'next';
import { generatePageMetadata } from '@/lib/seo-config';
import SupportClientView from './SupportClientView';

export const metadata: Metadata = generatePageMetadata({
  title: 'Support & Help Center',
  description: 'Get help with Slyker Tech Web Services. Access FAQs, documentation, submit support tickets, and contact our technical support team across Africa.',
  url: '/support'
});

export default function SupportPage() {
  return <SupportClientView />;
}
