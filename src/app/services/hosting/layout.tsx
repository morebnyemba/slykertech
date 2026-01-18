import { Metadata } from 'next';

export const metadata: Metadata = {
  title: 'Web Hosting Services | Slyker Tech Web Services',
  description: 'Professional web hosting solutions including Shared, VPS, and Dedicated servers with 99.9% uptime guarantee.',
};

export default function HostingLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  return <>{children}</>;
}
