import { Metadata } from 'next';
import MultiStepSignupForm from '@/components/auth/MultiStepSignupForm';
import { generatePageMetadata } from '@/lib/seo-config';

export async function generateMetadata(): Promise<Metadata> {
  return generatePageMetadata({
    title: 'Sign Up',
    description: 'Create your Slyker Tech Web Services account to access our services',
    url: '/signup',
  });
}

export default function SignupPage() {
  return <MultiStepSignupForm />;
}
