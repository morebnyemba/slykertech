import { Metadata } from 'next';
import { Suspense } from 'react';
import MultiStepLoginForm from '@/components/auth/MultiStepLoginForm';
import { generatePageMetadata } from '@/lib/seo-config';

export async function generateMetadata(): Promise<Metadata> {
  return generatePageMetadata({
    title: 'Login',
    description: 'Sign in to your Slyker Tech Web Services account',
    url: '/login',
  });
}

function LoginFormFallback() {
  return (
    <div className="min-h-screen flex items-center justify-center bg-gradient-to-br from-blue-50 via-white to-blue-50 dark:from-gray-900 dark:via-gray-800 dark:to-gray-900">
      <div className="animate-pulse text-gray-600 dark:text-gray-400">Loading...</div>
    </div>
  );
}

export default function LoginPage() {
  return (
    <Suspense fallback={<LoginFormFallback />}>
      <MultiStepLoginForm />
    </Suspense>
  );
}
