import { Metadata } from 'next';
import ReferralDashboard from '@/components/referrals/ReferralDashboard';
import { generatePageMetadata } from '@/lib/seo-config';

export async function generateMetadata(): Promise<Metadata> {
  return generatePageMetadata({
    title: 'Referral Program',
    description: 'Refer friends and earn rewards with our referral program',
    url: '/dashboard/referrals',
  });
}

export default function ReferralsPage() {
  return <ReferralDashboard />;
}
