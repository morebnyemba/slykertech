import { Metadata } from 'next';
import LoginForm from '@/components/auth/LoginForm';

export const metadata: Metadata = {
  title: 'Login - Slyker Tech',
  description: 'Sign in to your Slyker Tech account',
};

export default function LoginPage() {
  return <LoginForm />;
}
