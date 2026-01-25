import { Metadata } from 'next';
import MultiStepLoginForm from '@/components/auth/MultiStepLoginForm';
import { generatePageMetadata } from '@/lib/seo-config';

export async function generateMetadata(): Promise<Metadata> {
  return generatePageMetadata({
    title: 'Login',
    description: 'Sign in to your Slyker Tech Web Services account',
    url: '/login',
  });
}

export default function LoginPage() {
  return <MultiStepLoginForm />;
}
