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
  const [showContactDropdown, setShowContactDropdown] = useState(false);
  const [showServicesDropdown, setShowServicesDropdown] = useState(false);
  const [showCompanyDropdown, setShowCompanyDropdown] = useState(false);
  const [showProductsDropdown, setShowProductsDropdown] = useState(false);
  const [expandedSections, setExpandedSections] = useState<Record<string, boolean>>({});

  const toggleSection = (section: string) => {
    setExpandedSections(prev => ({
      ...prev,
      [section]: !prev[section]
    }));
  };
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

  const companyLinks = [
    { name: 'About Us', href: '/about', description: 'Mission, vision, and leadership' },
    { name: 'Invest', href: '/invest', description: 'Opportunities and investor relations' },
    { name: 'Partner', href: '/partner', description: 'Alliances, resellers, and agencies' },
    { name: 'Our Portfolio', href: '/portfolio', description: 'Selected work and case studies' },
    { name: 'Support', href: '/support', description: 'Help center and guides' },
    { name: 'Join Us', href: '/careers', description: 'Careers and open roles' }
  ];

  const productLinks = [
    { name: 'Managed WordPress', href: '/products/managed-wordpress', description: 'Optimized WP hosting with updates' },
    { name: 'Site Builder', href: '/products/site-builder', description: 'Launch fast with drag-and-drop' },
    { name: 'Email & Collaboration Suite', href: '/products/email-suite', description: 'Business email, calendar, docs' },
    { name: 'SSL & Security Suite', href: '/products/security-suite', description: 'TLS, WAF, malware protection' },
    { name: 'CDN & Edge Caching', href: '/products/cdn', description: 'Global acceleration and caching' },
    { name: 'Backups & Disaster Recovery', href: '/products/backups', description: 'Snapshots, restores, continuity' }
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
      <div className="max-w-7xl mx-auto px-3 sm:px-4 py-1 sm:py-1.5 flex justify-between items-center">
        {/* Logo */}
        <Link href="/" className="flex items-center flex-shrink-0">
          <div className="relative w-16 h-16 sm:w-18 sm:h-18 md:w-20 md:h-20">
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

          {/* Company Dropdown */}
          <div 
            className="relative"
            onMouseEnter={() => setShowCompanyDropdown(true)}
            onMouseLeave={() => setShowCompanyDropdown(false)}
          >
            <button className="relative text-gray-800 dark:text-gray-200 font-medium hover:text-blue-700 dark:hover:text-blue-400 transition-colors duration-300 group flex items-center gap-1">
              Company
              <FaChevronDown className="w-3 h-3" />
              <span className="absolute bottom-0 left-0 w-0 h-0.5 bg-blue-600 transition-all duration-300 group-hover:w-full"></span>
            </button>

            {showCompanyDropdown && (
              <div className="absolute top-full left-0 mt-2 w-72 bg-white dark:bg-gray-800 rounded-lg shadow-xl border border-gray-200 dark:border-gray-700 py-2 z-50">
                {companyLinks.map((link, idx) => (
                  <div key={link.name}>
                    <Link
                      href={link.href}
                      className="block px-4 py-2.5 hover:bg-blue-50 dark:hover:bg-gray-700 transition-colors text-gray-900 dark:text-gray-100"
                    >
                      <div className="font-medium">{link.name}</div>
                      <div className="text-sm text-gray-600 dark:text-gray-400">{link.description}</div>
                    </Link>
                    {idx < companyLinks.length - 1 && <div className="border-t border-gray-100 dark:border-gray-700 mx-4" />}
                  </div>
                ))}
              </div>
            )}
          </div>

          {/* Products Dropdown */}
          <div 
            className="relative"
            onMouseEnter={() => setShowProductsDropdown(true)}
            onMouseLeave={() => setShowProductsDropdown(false)}
          >
            <button className="relative text-gray-800 dark:text-gray-200 font-medium hover:text-blue-700 dark:hover:text-blue-400 transition-colors duration-300 group flex items-center gap-1">
              Products
              <FaChevronDown className="w-3 h-3" />
              <span className="absolute bottom-0 left-0 w-0 h-0.5 bg-blue-600 transition-all duration-300 group-hover:w-full"></span>
            </button>

            {showProductsDropdown && (
              <div className="absolute top-full left-0 mt-2 w-80 bg-white dark:bg-gray-800 rounded-lg shadow-xl border border-gray-200 dark:border-gray-700 py-2 z-50">
                {productLinks.map((product, idx) => (
                  <div key={product.name}>
                    <Link
                      href={product.href}
                      className="block px-4 py-2.5 hover:bg-blue-50 dark:hover:bg-gray-700 transition-colors text-gray-900 dark:text-gray-100"
                    >
                      <div className="font-medium">{product.name}</div>
                      <div className="text-sm text-gray-600 dark:text-gray-400">{product.description}</div>
                    </Link>
                    {idx < productLinks.length - 1 && <div className="border-t border-gray-100 dark:border-gray-700 mx-4" />}
                  </div>
                ))}
              </div>
            )}
          </div>

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
              <div className="absolute top-full left-0 mt-2 w-72 bg-white dark:bg-gray-800 rounded-lg shadow-xl border border-gray-200 dark:border-gray-700 py-2 z-50">
                {serviceCategories.map((category, idx) => (
                  <div key={category.name}>
                    <Link
                      href={category.href}
                      className="block px-4 py-3 hover:bg-blue-50 dark:hover:bg-gray-700 transition-colors"
                    >
                      <div className="font-medium text-gray-900 dark:text-gray-100">{category.name}</div>
                      <div className="text-sm text-gray-600 dark:text-gray-400">{category.description}</div>
                    </Link>
                    {idx < serviceCategories.length - 1 && <div className="border-t border-gray-100 dark:border-gray-700 mx-4" />}
                  </div>
                ))}
              </div>
            )}
          </div>
          
          {/* Contact Dropdown */}
          <div 
            className="relative"
            onMouseEnter={() => setShowContactDropdown(true)}
            onMouseLeave={() => setShowContactDropdown(false)}
          >
            <button className="relative text-gray-800 dark:text-gray-200 font-medium hover:text-blue-700 dark:hover:text-blue-400 transition-colors duration-300 group flex items-center gap-1">
              Contact
              <FaChevronDown className="w-3 h-3" />
              <span className="absolute bottom-0 left-0 w-0 h-0.5 bg-blue-600 transition-all duration-300 group-hover:w-full"></span>
            </button>

            {showContactDropdown && (
              <div className="absolute top-full left-0 mt-2 w-64 bg-white dark:bg-gray-800 rounded-lg shadow-xl border border-gray-200 dark:border-gray-700 py-2 z-50">
                <Link href="/contact" className="block px-4 py-2.5 hover:bg-blue-50 dark:hover:bg-gray-700 transition-colors text-gray-900 dark:text-gray-100">Contact Page</Link>
                <a href="tel:+263787211325" className="block px-4 py-2.5 hover:bg-blue-50 dark:hover:bg-gray-700 transition-colors text-gray-900 dark:text-gray-100">Call +263 78 721 1325</a>
                <a href="mailto:support@slykertech.co.zw" className="block px-4 py-2.5 hover:bg-blue-50 dark:hover:bg-gray-700 transition-colors text-gray-900 dark:text-gray-100">Email Support</a>
                <a href="https://wa.me/263787211325" target="_blank" className="block px-4 py-2.5 hover:bg-blue-50 dark:hover:bg-gray-700 transition-colors text-gray-900 dark:text-gray-100">WhatsApp Chat</a>
              </div>
            )}
          </div>

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
        menuOpen ? 'max-h-[70vh] opacity-100' : 'max-h-0 opacity-0'
      }`}>
        <nav className="flex flex-col items-stretch bg-white/95 dark:bg-gray-900/95 backdrop-blur-sm dark:backdrop-blur-sm shadow-inner py-2 overflow-y-auto max-h-[calc(70vh-140px)]">
          <MobileNavLink href="/" label="Home" onClick={() => setMenuOpen(false)} />

          {/* Company Section */}
          <button
            onClick={() => toggleSection('company')}
            className={`w-full text-left px-4 pt-2 pb-1 text-xs font-semibold uppercase flex items-center justify-between transition-colors ${expandedSections['company'] ? 'text-blue-600 dark:text-blue-300' : 'text-gray-500 dark:text-gray-400'} hover:text-blue-700 dark:hover:text-blue-200`}
          >
            Company
            <span className={`transform transition-transform ${expandedSections['company'] ? 'rotate-180 text-blue-600 dark:text-blue-300' : ''}`}>
              ▼
            </span>
          </button>
          {expandedSections['company'] && (
            <>
              {companyLinks.map((link) => (
                <MobileNavLink key={link.name} href={link.href} label={link.name} onClick={() => setMenuOpen(false)} />
              ))}
            </>
          )}

          {/* Products Section */}
          <button
            onClick={() => toggleSection('products')}
            className={`w-full text-left px-4 pt-2 pb-1 text-xs font-semibold uppercase flex items-center justify-between transition-colors ${expandedSections['products'] ? 'text-blue-600 dark:text-blue-300' : 'text-gray-500 dark:text-gray-400'} hover:text-blue-700 dark:hover:text-blue-200`}
          >
            Products
            <span className={`transform transition-transform ${expandedSections['products'] ? 'rotate-180 text-blue-600 dark:text-blue-300' : ''}`}>
              ▼
            </span>
          </button>
          {expandedSections['products'] && (
            <>
              {productLinks.map((link) => (
                <MobileNavLink key={link.name} href={link.href} label={link.name} onClick={() => setMenuOpen(false)} />
              ))}
            </>
          )}

          {/* Services Section */}
          <button
            onClick={() => toggleSection('services')}
            className={`w-full text-left px-4 pt-2 pb-1 text-xs font-semibold uppercase flex items-center justify-between transition-colors ${expandedSections['services'] ? 'text-blue-600 dark:text-blue-300' : 'text-gray-500 dark:text-gray-400'} hover:text-blue-700 dark:hover:text-blue-200`}
          >
            Services
            <span className={`transform transition-transform ${expandedSections['services'] ? 'rotate-180 text-blue-600 dark:text-blue-300' : ''}`}>
              ▼
            </span>
          </button>
          {expandedSections['services'] && (
            <>
              {serviceCategories.map((service) => (
                <MobileNavLink key={service.name} href={service.href} label={service.name} onClick={() => setMenuOpen(false)} />
              ))}
            </>
          )}

          {/* Contact Section */}
          <button
            onClick={() => toggleSection('contact')}
            className={`w-full text-left px-4 pt-2 pb-1 text-xs font-semibold uppercase flex items-center justify-between transition-colors ${expandedSections['contact'] ? 'text-blue-600 dark:text-blue-300' : 'text-gray-500 dark:text-gray-400'} hover:text-blue-700 dark:hover:text-blue-200`}
          >
            Contact
            <span className={`transform transition-transform ${expandedSections['contact'] ? 'rotate-180 text-blue-600 dark:text-blue-300' : ''}`}>
              ▼
            </span>
          </button>
          {expandedSections['contact'] && (
            <>
              <MobileNavLink href="/contact" label="Contact Page" onClick={() => setMenuOpen(false)} />
              <MobileNavLink href="tel:+263787211325" label="Call +263 78 721 1325" onClick={() => setMenuOpen(false)} />
              <MobileNavLink href="mailto:support@slykertech.co.zw" label="Email Support" onClick={() => setMenuOpen(false)} />
              <MobileNavLink href="https://wa.me/263787211325" label="WhatsApp Chat" onClick={() => setMenuOpen(false)} />
            </>
          )}
          
          
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
              <div className="flex flex-col gap-2 px-4 pt-3 pb-4 border-t border-gray-100 dark:border-gray-800">
                <Link
                  href="/login"
                  onClick={() => setMenuOpen(false)}
                  className="w-full text-center text-base font-semibold text-white bg-blue-600 hover:bg-blue-700 active:bg-blue-800 rounded-md py-3 transition-colors duration-200"
                >
                  Login
                </Link>
                <Link
                  href="/signup"
                  onClick={() => setMenuOpen(false)}
                  className="w-full text-center text-base font-semibold text-white bg-blue-500 hover:bg-blue-600 active:bg-blue-700 rounded-md py-3 transition-colors duration-200"
                >
                  Sign Up
                </Link>
              </div>
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