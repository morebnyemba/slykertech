import { Metadata } from 'next';
import LoginForm from '@/components/auth/LoginForm';
import { generatePageMetadata } from '@/lib/seo-config';

export async function generateMetadata(): Promise<Metadata> {
  return generatePageMetadata({
    title: 'Login',
    description: 'Sign in to your Slyker Tech account',
    url: '/login',
  });
}

export default function LoginPage() {
  return <LoginForm />;
}
