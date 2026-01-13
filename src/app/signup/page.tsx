import { Metadata } from 'next';
import SignupForm from '@/components/auth/SignupForm';

export const metadata: Metadata = {
  title: 'Sign Up - Slyker Tech',
  description: 'Create your Slyker Tech account',
};

export default function SignupPage() {
  return <SignupForm />;
}
