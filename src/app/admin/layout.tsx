import './admin.css';
import { ThemeProvider } from '@/components/ThemeProvider';

export const metadata = {
  title: 'Admin Dashboard - Slyker Tech',
  description: 'Slyker Tech Admin Management Portal',
};

export default function AdminRootLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  return (
    <ThemeProvider
      attribute="class"
      defaultTheme="system"
      enableSystem
      disableTransitionOnChange={false}
    >
      {/* Admin layout - no header/footer from main site */}
      <div className="admin-layout">
        {children}
      </div>
    </ThemeProvider>
  );
}
