import { Metadata } from 'next';
import { Suspense } from 'react';
import MultiStepSignupForm from '@/components/auth/MultiStepSignupForm';
import { generatePageMetadata } from '@/lib/seo-config';

export async function generateMetadata(): Promise<Metadata> {
  return generatePageMetadata({
    title: 'Sign Up',
    description: 'Create your Slyker Tech Web Services account to access our services',
    url: '/signup',
  });
}

function SignupFormFallback() {
  return (
    <div className="min-h-screen flex items-center justify-center bg-gradient-to-br from-blue-50 via-white to-blue-50 dark:from-gray-900 dark:via-gray-800 dark:to-gray-900">
      <div className="animate-pulse text-gray-600 dark:text-gray-400">Loading...</div>
    </div>
  );
}

export default function SignupPage() {
  return (
    <Suspense fallback={<SignupFormFallback />}>
      <MultiStepSignupForm />
    </Suspense>
  );
}
