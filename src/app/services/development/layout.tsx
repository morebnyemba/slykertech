import { Metadata } from 'next';

export const metadata: Metadata = {
  title: 'Development Services | Slyker Tech',
  description: 'Professional web, mobile, desktop, and hybrid application development services.',
};

export default function DevelopmentLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  return <>{children}</>;
}
