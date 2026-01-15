'use client';
import { useState, useEffect } from 'react';
import Link from 'next/link';
import Image from 'next/image';
import { useRouter } from 'next/navigation';
import { FaWhatsapp, FaPhone, FaEnvelope, FaFacebook, FaTwitter, FaUserCircle, FaSignOutAlt, FaShoppingCart, FaChevronDown } from 'react-icons/fa';
import { ThemeToggle } from '@/components/ThemeToggle'; // Import the theme toggle
import { useAuthStore } from '@/lib/stores/auth-store';
import { useCartStore } from '@/lib/stores/cart-store';

export default function Header() {
  const [menuOpen, setMenuOpen] = useState(false);
  const [, setScrolled] = useState(false);
  const [showContactModal, setShowContactModal] = useState(false);
  const [showServicesDropdown, setShowServicesDropdown] = useState(false);
  const { isAuthenticated, user, logout, token } = useAuthStore();
  const { fetchCart, getItemCount } = useCartStore();
  const router = useRouter();

  useEffect(() => {
    const handleScroll = () => {
      setScrolled(window.scrollY > 10);
    };

    window.addEventListener('scroll', handleScroll);
    return () => window.removeEventListener('scroll', handleScroll);
  }, []);

  // Fetch cart on mount and when auth state changes
  useEffect(() => {
    fetchCart(token || undefined);
  }, [fetchCart, token]);

  const handleWhatsAppClick = () => {
    const message = encodeURIComponent("Hi Slyker Tech! I'm in need of one of your services.");
    window.open(`https://wa.me/263787211325?text=${message}`, '_blank');
  };

  const handleLogout = () => {
    logout();
    router.push('/');
  };

  const cartItemCount = getItemCount();

  const serviceCategories = [
    { name: 'Web Hosting', href: '/services/hosting', description: 'Shared, VPS, Dedicated' },
    { name: 'Domain Services', href: '/services/domains', description: 'Registration & Transfer' },
    { name: 'Development', href: '/services/development', description: 'Web, Mobile, Desktop' },
    { name: 'Design Services', href: '/services/design', description: 'UI/UX, Graphics' },
  ];

  const ContactModal = () => (
    <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center p-4 z-50">
      <div className="bg-white dark:bg-gray-900 rounded-2xl p-8 max-w-md w-full">
        <div className="flex items-center gap-3 mb-4">
          <FaUserCircle className="text-blue-600 dark:text-blue-400 text-3xl" />
          <div>
            <h3 className="text-xl font-bold text-blue-900 dark:text-blue-300">
              Contact Options
            </h3>
            <p className="text-sm text-gray-500 dark:text-gray-400">
              Our client portal is launching soon!
            </p>
          </div>
        </div>

        <div className="bg-blue-50 dark:bg-blue-900/20 rounded-lg p-4 mb-6">
          <p className="text-sm text-blue-800 dark:text-blue-200 text-center">
            ⚡ Our client portal is almost complete! For now, please choose a contact method below.
          </p>
        </div>

        <div className="space-y-3">
          <button
            onClick={handleWhatsAppClick}
            className="flex items-center gap-4 p-4 bg-green-100 dark:bg-green-900 rounded-lg hover:bg-green-200 dark:hover:bg-green-800 transition-colors w-full text-left"
          >
            <div className="bg-green-600 dark:bg-green-500 p-2 rounded-full">
              <FaWhatsapp className="text-white w-5 h-5" />
            </div>
            <div>
              <p className="font-medium text-gray-800 dark:text-gray-100">WhatsApp Chat</p>
              <p className="text-sm text-gray-500 dark:text-gray-400">Instant response</p>
            </div>
          </button>

          <a
            href="tel:+263787211325"
            className="flex items-center gap-4 p-4 bg-blue-100 dark:bg-blue-900 rounded-lg hover:bg-blue-200 dark:hover:bg-blue-800 transition-colors w-full"
          >
            <div className="bg-blue-600 dark:bg-blue-500 p-2 rounded-full">
              <FaPhone className="text-white w-5 h-5" />
            </div>
            <div>
              <p className="font-medium text-gray-800 dark:text-gray-100">Call Us</p>
              <p className="text-sm text-gray-500 dark:text-gray-400">+263 78 721 1325</p>
            </div>
          </a>

          <a
            href="mailto:support@slykertech.co.zw?subject=Service Inquiry&body=Hello Slyker Tech,%0D%0A%0D%0AI'm interested in your services. Please contact me with more information.%0D%0A%0D%0ABest regards,"
            className="flex items-center gap-4 p-4 bg-purple-100 dark:bg-purple-900 rounded-lg hover:bg-purple-200 dark:hover:bg-purple-800 transition-colors w-full"
          >
            <div className="bg-purple-600 dark:bg-purple-500 p-2 rounded-full">
              <FaEnvelope className="text-white w-5 h-5" />
            </div>
            <div>
              <p className="font-medium text-gray-800 dark:text-gray-100">Email Us</p>
              <p className="text-sm text-gray-500 dark:text-gray-400">support@slykertech.co.zw</p>
            </div>
          </a>
        </div>

        <button
          onClick={() => setShowContactModal(false)}
          className="mt-6 w-full py-2 text-gray-600 dark:text-gray-300 hover:text-gray-800 dark:hover:text-gray-100 text-sm"
        >
          Close
        </button>
      </div>
    </div>
  );

  return (
    <header className="fixed w-full z-50 bg-white/95 dark:bg-gray-900/95 backdrop-blur-sm dark:backdrop-blur-sm shadow-md dark:shadow-none transition-all duration-300">
      {/* Maintenance Notice Banner */}
      <div className="w-full bg-yellow-500 dark:bg-yellow-600 text-black dark:text-white text-xs sm:text-sm py-2 px-2 sm:px-4">
        <div className="max-w-7xl mx-auto text-center font-medium">
          <span className="hidden min-[400px]:inline">⚠️ Site Under Maintenance - Some features may be temporarily unavailable</span>
          <span className="min-[400px]:hidden">⚠️ Maintenance</span>
        </div>
      </div>

      {/* Top Announcement Bar - Always Visible */}
      <div className="w-full bg-blue-800 text-white text-xs sm:text-sm py-2 px-2 sm:px-4">
        <div className="max-w-7xl mx-auto flex justify-between items-center">
          <div className="flex items-center space-x-2 sm:space-x-4 overflow-hidden">
            <a href="tel:+263787211325" className="flex items-center hover:text-blue-200 transition whitespace-nowrap text-xs sm:text-sm">
              <FaPhone className="w-3 h-3 mr-1 flex-shrink-0" />
              <span className="hidden min-[400px]:inline">+263 78 721 1325</span>
              <span className="min-[400px]:hidden">Call</span>
            </a>
            <a href="mailto:support@slykertech.co.zw" className="flex items-center hover:text-blue-200 transition text-xs sm:text-sm">
              <FaEnvelope className="w-3 h-3 mr-1 flex-shrink-0" />
              <span className="hidden sm:inline truncate max-w-[120px] md:max-w-none">support@slykertech.co.zw</span>
              <span className="sm:hidden">Email</span>
            </a>
          </div>
          <div className="flex items-center space-x-2 sm:space-x-4 flex-shrink-0">
            <a href="#" className="hover:text-blue-200 transition" aria-label="Facebook">
              <FaFacebook className="w-3 h-3 sm:w-4 sm:h-4" />
            </a>
            <a href="#" className="hover:text-blue-200 transition" aria-label="Twitter">
              <FaTwitter className="w-3 h-3 sm:w-4 sm:h-4" />
            </a>
          </div>
        </div>
      </div>

      {/* Main Navigation */}
      <div className="max-w-7xl mx-auto px-3 sm:px-6 py-2 sm:py-3 md:py-4 flex justify-between items-center">
        {/* Logo */}
        <Link href="/" className="flex items-center flex-shrink-0">
          <div className="relative w-12 h-12 sm:w-14 sm:h-14 md:w-16 md:h-16">
            <Image
              src="/images/stws.png"
              alt="Slyker Tech Web Services Logo"
              fill
              className="object-contain"
              priority
            />
          </div>
        </Link>

        {/* Mobile Navigation Controls */}
        <div className="sm:hidden flex items-center gap-1.5 min-[400px]:gap-2 sm:gap-3">
          {/* Cart Icon for Mobile */}
          <Link 
            href="/cart" 
            className="relative text-gray-800 dark:text-gray-200 hover:text-blue-700 dark:hover:text-blue-400 transition-colors p-2"
            aria-label="Shopping cart"
          >
            <FaShoppingCart className="text-lg" />
            {cartItemCount > 0 && (
              <span className="absolute -top-1 -right-1 bg-red-500 text-white text-xs rounded-full w-4 h-4 flex items-center justify-center font-bold text-[10px]">
                {cartItemCount}
              </span>
            )}
          </Link>

          {/* Theme Toggle for Mobile */}
          <ThemeToggle />

          {/* Mobile Hamburger Icon */}
          <button
            className="text-blue-700 dark:text-blue-400 hover:text-blue-800 dark:hover:text-blue-300 focus:outline-none transition-all duration-300 ease-in-out p-2"
            onClick={() => setMenuOpen(!menuOpen)}
            aria-label="Toggle menu"
          >
            <div className="relative w-6 h-6">
              <span className={`absolute h-0.5 w-6 bg-current transform transition-all duration-300 ease-in-out ${
                menuOpen ? 'rotate-45 top-3' : 'top-1'
              }`}></span>
              <span className={`absolute h-0.5 w-6 bg-current top-3 transition-all duration-200 ${
                menuOpen ? 'opacity-0' : 'opacity-100'
              }`}></span>
              <span className={`absolute h-0.5 w-6 bg-current transform transition-all duration-300 ease-in-out ${
                menuOpen ? '-rotate-45 top-3' : 'top-5'
              }`}></span>
            </div>
          </button>
        </div>

        {/* Desktop Navigation */}
        <nav className="hidden sm:flex items-center space-x-4 lg:space-x-8">
          <NavLink href="/" label="Home" />
          <NavLink href="/about" label="About" />
          
          {/* Services Dropdown */}
          <div 
            className="relative"
            onMouseEnter={() => setShowServicesDropdown(true)}
            onMouseLeave={() => setShowServicesDropdown(false)}
          >
            <button className="relative text-gray-800 dark:text-gray-200 font-medium hover:text-blue-700 dark:hover:text-blue-400 transition-colors duration-300 group flex items-center gap-1">
              Services
              <FaChevronDown className="w-3 h-3" />
              <span className="absolute bottom-0 left-0 w-0 h-0.5 bg-blue-600 transition-all duration-300 group-hover:w-full"></span>
            </button>
            
            {showServicesDropdown && (
              <div className="absolute top-full left-0 mt-2 w-64 bg-white dark:bg-gray-800 rounded-lg shadow-xl border border-gray-200 dark:border-gray-700 py-2 z-50">
                {serviceCategories.map((category) => (
                  <Link
                    key={category.name}
                    href={category.href}
                    className="block px-4 py-3 hover:bg-blue-50 dark:hover:bg-gray-700 transition-colors"
                  >
                    <div className="font-medium text-gray-900 dark:text-gray-100">{category.name}</div>
                    <div className="text-sm text-gray-600 dark:text-gray-400">{category.description}</div>
                  </Link>
                ))}
              </div>
            )}
          </div>
          
          <NavLink href="/portfolio" label="Portfolio" />
          <NavLink href="/contact" label="Contact" />

          {/* Theme Toggle for Desktop */}
          <ThemeToggle />

          {/* Cart Icon with Badge */}
          <Link 
            href="/cart" 
            className="relative text-gray-800 dark:text-gray-200 hover:text-blue-700 dark:hover:text-blue-400 transition-colors"
          >
            <FaShoppingCart className="text-xl" />
            {cartItemCount > 0 && (
              <span className="absolute -top-2 -right-2 bg-red-500 text-white text-xs rounded-full w-5 h-5 flex items-center justify-center font-bold">
                {cartItemCount}
              </span>
            )}
          </Link>

          {isAuthenticated ? (
            <>
              <Link
                href="/dashboard"
                className="flex items-center gap-2 text-gray-800 dark:text-gray-200 hover:text-blue-700 dark:hover:text-blue-400 transition-colors"
              >
                <FaUserCircle className="text-xl" />
                <span className="font-medium">{user?.first_name || 'Dashboard'}</span>
              </Link>
              <button
                onClick={handleLogout}
                className="flex items-center gap-2 text-gray-800 dark:text-gray-200 hover:text-red-600 dark:hover:text-red-400 transition-colors"
              >
                <FaSignOutAlt />
                Logout
              </button>
            </>
          ) : (
            <>
              <Link
                href="/login"
                className="text-gray-800 dark:text-gray-200 font-medium hover:text-blue-700 dark:hover:text-blue-400 transition-colors"
              >
                Login
              </Link>
              <Link
                href="/signup"
                className="bg-gradient-to-r from-blue-600 to-blue-700 text-white font-medium py-2 px-6 rounded-lg hover:shadow-lg hover:shadow-blue-200/50 transition-all duration-300 transform hover:-translate-y-0.5"
              >
                Sign Up
              </Link>
            </>
          )}
        </nav>
      </div>

      {/* Mobile Menu */}
      <div className={`sm:hidden overflow-hidden transition-all duration-300 ease-in-out ${
        menuOpen ? 'max-h-[500px] opacity-100' : 'max-h-0 opacity-0'
      }`}>
        <nav className="flex flex-col items-stretch bg-white/95 dark:bg-gray-900/95 backdrop-blur-sm dark:backdrop-blur-sm shadow-inner py-2">
          <MobileNavLink href="/" label="Home" onClick={() => setMenuOpen(false)} />
          <MobileNavLink href="/about" label="About" onClick={() => setMenuOpen(false)} />
          <MobileNavLink href="/services/hosting" label="Hosting" onClick={() => setMenuOpen(false)} />
          <MobileNavLink href="/services/domains" label="Domains" onClick={() => setMenuOpen(false)} />
          <MobileNavLink href="/services/development" label="Development" onClick={() => setMenuOpen(false)} />
          <MobileNavLink href="/portfolio" label="Portfolio" onClick={() => setMenuOpen(false)} />
          <MobileNavLink href="/contact" label="Contact" onClick={() => setMenuOpen(false)} />
          
          {isAuthenticated ? (
            <>
              <MobileNavLink href="/dashboard" label="Dashboard" onClick={() => setMenuOpen(false)} />
              <button
                onClick={() => {
                  handleLogout();
                  setMenuOpen(false);
                }}
                className="w-full text-center text-base font-medium text-red-600 dark:text-red-400 hover:text-red-700 dark:hover:text-red-300 hover:bg-red-50 dark:hover:bg-red-900/20 py-3 transition-colors duration-300 border-b border-gray-100 dark:border-gray-800"
              >
                Logout
              </button>
            </>
          ) : (
            <>
              <MobileNavLink href="/login" label="Login" onClick={() => setMenuOpen(false)} />
              <MobileNavLink href="/signup" label="Sign Up" onClick={() => setMenuOpen(false)} />
            </>
          )}
        </nav>
      </div>

      {/* Contact Modal */}
      {showContactModal && <ContactModal />}
    </header>
  );
}

// Desktop Navigation Link Component
function NavLink({ href, label }) {
  return (
    <Link
      href={href}
      className="relative text-gray-800 dark:text-gray-200 font-medium hover:text-blue-700 dark:hover:text-blue-400 transition-colors duration-300 group"
    >
      {label}
      <span className="absolute bottom-0 left-0 w-0 h-0.5 bg-blue-600 transition-all duration-300 group-hover:w-full"></span>
    </Link>
  );
}

// Mobile Navigation Link Component
function MobileNavLink({ href, label, onClick }) {
  return (
    <Link
      href={href}
      className="w-full text-center text-base font-medium text-gray-800 dark:text-gray-200 hover:text-blue-700 dark:hover:text-blue-400 hover:bg-blue-50 dark:hover:bg-gray-800 py-3 transition-colors duration-300 border-b border-gray-100 dark:border-gray-800 last:border-0"
      onClick={onClick}
    >
      {label}
    </Link>
  );
}