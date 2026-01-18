import { Metadata } from 'next';

export const metadata: Metadata = {
  title: 'Domain Services | Slyker Tech Web Services',
  description: 'Domain registration and transfer services with competitive pricing and free WHOIS privacy.',
};

export default function DomainsLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  return <>{children}</>;
}
