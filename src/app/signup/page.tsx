import { Metadata } from 'next';
import SignupForm from '@/components/auth/SignupForm';
import { generatePageMetadata } from '@/lib/seo-config';

export async function generateMetadata(): Promise<Metadata> {
  return generatePageMetadata({
    title: 'Sign Up',
    description: 'Create your Slyker Tech account to access our services',
    url: '/signup',
  });
}

export default function SignupPage() {
  return <SignupForm />;
}
