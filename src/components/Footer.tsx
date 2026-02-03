'use client';

import Link from 'next/link';
import { FaFacebook, FaTwitter, FaLinkedin, FaInstagram, FaChevronRight,FaEnvelope,FaPhone } from 'react-icons/fa';

export default function Footer() {
  const currentYear = new Date().getFullYear();

  return (
    <footer className="relative bg-blue-700 text-white py-16 px-4 sm:px-8 overflow-hidden">
      {/* Animated Background Pattern */}
      <div className="absolute inset-0">
        <div className="absolute inset-0 bg-gradient-to-br from-blue-800 to-blue-700" />
        <div className="absolute inset-0 opacity-15">
          <svg width="100%" height="100%">
            <pattern 
              id="footer-pattern" 
              x="0" 
              y="0" 
              width="120" 
              height="120" 
              patternUnits="userSpaceOnUse"
            >
              <path
                d="M0 60 Q30 30, 60 60 T120 60"
                fill="none"
                stroke="currentColor"
                strokeWidth="1.2"
                className="text-blue-400/50"
              />
              <path
                d="M0 80 Q30 50, 60 80 T120 80"
                fill="none"
                stroke="currentColor"
                strokeWidth="1.2"
                className="text-blue-400/40"
              />
            </pattern>
            <rect width="100%" height="100%" fill="url(#footer-pattern)" />
          </svg>
        </div>
      </div>

      {/* Content Container */}
      <div className="relative max-w-7xl mx-auto z-10">
        <div className="grid grid-cols-1 md:grid-cols-4 gap-10 mb-14">
          {/* Company Info */}
          <div className="space-y-5">
            <h3 className="text-2xl font-bold text-white tracking-tight">
              Slyker Tech
              <span className="block text-blue-200 font-medium text-lg mt-1">Web Services</span>
            </h3>
            <p className="text-blue-100/90 leading-relaxed">
              Delivering exceptional digital solutions tailored for Zimbabwean businesses and beyond.
            </p>
            <div className="mt-4">
              <a href="https://hostadvice.com/hosting-company/slyker-tech-web-services-reviews/" target="_blank" rel="noopener noreferrer">
                <img 
                  style={{width: '100%', maxWidth: '200px'}} 
                  src="https://hostadvice.com/awards/2026-top-25-reseller-hosting.png" 
                  alt="Slyker Tech Web Services review - Top 25 Reseller Hosting 2026"
                  className="hover:opacity-90 transition-opacity"
                />
              </a>
            </div>
          </div>

          {/* Quick Links */}
          <div>
            <h4 className="text-lg font-semibold text-white mb-6 pb-2 border-b border-blue-600/30">
              Quick Links
            </h4>
            <ul className="space-y-3">
              {[
                { name: 'Home', href: '/' },
                { name: 'Services', href: '/services' },
                { name: 'Portfolio', href: '/portfolio' },
                { name: 'Contact', href: '/contact' }
              ].map((item) => (
                <li key={item.name}>
                  <Link 
                    href={item.href}
                    className="flex items-center text-blue-100/90 hover:text-white transition-all group"
                  >
                    <FaChevronRight className="w-3 h-3 mr-2 text-blue-300 group-hover:text-white transition-all" />
                    <span className="group-hover:translate-x-1 transition-transform">
                      {item.name}
                    </span>
                  </Link>
                </li>
              ))}
            </ul>
          </div>

          {/* Legal Links */}
          <div>
            <h4 className="text-lg font-semibold text-white mb-6 pb-2 border-b border-blue-600/30">
              Legal
            </h4>
            <ul className="space-y-3">
              {[
                { name: 'Privacy Policy', href: '/privacy-policy' },
                { name: 'Terms & Conditions', href: '/terms-and-conditions' }
              ].map((item) => (
                <li key={item.name}>
                  <Link 
                    href={item.href}
                    className="flex items-center text-blue-100/90 hover:text-white transition-all group"
                  >
                    <FaChevronRight className="w-3 h-3 mr-2 text-blue-300 group-hover:text-white transition-all" />
                    <span className="group-hover:translate-x-1 transition-transform">
                      {item.name}
                    </span>
                  </Link>
                </li>
              ))}
            </ul>
          </div>

          {/* Social Media & Contact */}
          <div>
            <h4 className="text-lg font-semibold text-white mb-6 pb-2 border-b border-blue-600/30">
              Connect With Us
            </h4>
            <div className="flex space-x-4 mb-6">
              {[
                { icon: FaFacebook, href: '#' },
                { icon: FaTwitter, href: '#' },
                { icon: FaLinkedin, href: '#' },
                { icon: FaInstagram, href: '#' }
              ].map((social, index) => (
                <a
                  key={index}
                  href={social.href}
                  className="w-10 h-10 flex items-center justify-center rounded-full bg-blue-800/50 hover:bg-blue-600 text-blue-100 hover:text-white transition-all transform hover:-translate-y-1"
                  aria-label={`${social.icon.name} page`}
                >
                  <social.icon className="w-5 h-5" />
                </a>
              ))}
            </div>
            <div className="space-y-2">
              <a href="mailto:support@slykertech.co.zw" className="flex items-center text-blue-100/90 hover:text-white transition">
                <FaEnvelope className="w-4 h-4 mr-2" />
                support@slykertech.co.zw
              </a>
              <a href="tel:+263787211325" className="flex items-center text-blue-100/90 hover:text-white transition">
                <FaPhone className="w-4 h-4 mr-2" />
                +263 78 721 1325
              </a>
            </div>
          </div>
        </div>

        {/* Copyright Section */}
        <div className="border-t border-blue-600/30 pt-8 flex flex-col md:flex-row justify-between items-center">
          <div className="text-blue-100/80 text-sm mb-4 md:mb-0">
            &copy; {currentYear} Slyker Tech Web Services. All rights reserved.
          </div>
          <div className="flex space-x-6">
            <Link href="/privacy-policy" className="text-blue-100/80 hover:text-white text-sm transition">
              Privacy Policy
            </Link>
            <Link href="/terms-and-conditions" className="text-blue-100/80 hover:text-white text-sm transition">
              Terms & Conditions
            </Link>
          </div>
        </div>
      </div>
    </footer>
  );
}