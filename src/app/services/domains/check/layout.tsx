import { Metadata } from 'next';

export const metadata: Metadata = {
  title: 'Domain Availability Checker | Slyker Tech',
  description: 'Check domain availability across multiple TLDs using WHOIS servers. Fast, accurate, and comprehensive domain search service.',
  keywords: 'domain availability, whois lookup, domain search, domain checker, tld availability',
};

export default function CheckLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  return children;
}
