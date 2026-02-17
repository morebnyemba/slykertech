import { Metadata } from 'next';
import { Suspense } from 'react';
import ForgotPasswordForm from '@/components/auth/ForgotPasswordForm';
import { generatePageMetadata } from '@/lib/seo-config';

export async function generateMetadata(): Promise<Metadata> {
  return generatePageMetadata({
    title: 'Forgot Password',
    description: 'Reset your Slyker Tech Web Services account password',
    url: '/forgot-password',
  });
}

function ForgotPasswordFallback() {
  return (
    <div className="min-h-screen flex items-center justify-center bg-gradient-to-br from-blue-50 via-white to-blue-50 dark:from-gray-900 dark:via-gray-800 dark:to-gray-900">
      <div className="animate-pulse text-gray-600 dark:text-gray-400">Loading...</div>
    </div>
  );
}

export default function ForgotPasswordPage() {
  return (
    <Suspense fallback={<ForgotPasswordFallback />}>
      <ForgotPasswordForm />
    </Suspense>
  );
}
