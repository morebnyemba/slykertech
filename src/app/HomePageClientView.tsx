'use client';

import React, { useState, useEffect } from 'react';
import Link from 'next/link';
import {
    FaWhatsapp, FaCode, FaMobileAlt, FaShoppingCart,
    FaPalette, FaCloud, FaShieldAlt,
    FaServer, FaUserShield,
    FaRegStar, FaCcVisa, FaCcMastercard, FaCreditCard, FaBitcoin,
    FaArrowRight, FaCheckCircle, FaSearch, FaGlobe
} from 'react-icons/fa';
import { apiService } from '@/lib/api-service';
import ServiceFAQ from '@/components/service-pages/ServiceFAQ';
import PromotionsCTA from '@/components/service-pages/PromotionsCTA';
import { companyFAQs } from '@/components/service-pages/companyFAQData';

const HomePageClientView = () => {
    const [dynamicStats, setDynamicStats] = useState({
        uptime: 99.99,
        activeClients: 5,
        engineers: 1,
        yearsExperience: 4
    });

    const [domainSearch, setDomainSearch] = useState('');
    const [selectedExtension, setSelectedExtension] = useState('.com');

    const handleWhatsAppClick = () => {
        const message = encodeURIComponent("Hi Slyker Tech Web Services! I'm interested in your services.");
        window.open(`https://wa.me/263787211325?text=${message}`, '_blank');
    };

    const handleDomainSearch = (e: React.FormEvent) => {
        e.preventDefault();
        const fullDomain = `${domainSearch}${selectedExtension}`;
        // Navigate to domains page with search query
        window.location.href = `/services/domains?search=${encodeURIComponent(fullDomain)}`;
    };

    const popularExtensions = [
        { ext: '.com', price: '$20.00' },
        { ext: '.co.zw', price: '$5.00' },
        { ext: '.net', price: '$21.00' },
        { ext: '.org', price: '$16.00' },
        { ext: '.io', price: '$39.99' },
        { ext: '.tech', price: '$29.99' }
    ];

    // Fetch dynamic data from backend
    useEffect(() => {
        const fetchDynamicData = async () => {
            try {
                const statsResponse = await apiService.getPublicStats();
                if (statsResponse.data) {
                    setDynamicStats({
                        uptime: statsResponse.data.uptime || 99.99,
                        activeClients: statsResponse.data.activeClients || 5,
                        engineers: statsResponse.data.engineers || 1,
                        yearsExperience: statsResponse.data.yearsExperience || 4
                    });
                }
            } catch (error) {
                console.error('Failed to fetch dynamic data:', error);
            }
        };

        fetchDynamicData();
    }, []);

    // Stats data
    const stats = [
        { value: `${dynamicStats.uptime}%`, label: "Uptime SLA", icon: <FaServer className="w-8 h-8" /> },
        { value: `${dynamicStats.activeClients}+`, label: "Active Clients", icon: <FaUserShield className="w-8 h-8" /> },
        { value: `${dynamicStats.engineers}`, label: "Expert Engineers", icon: <FaCode className="w-8 h-8" /> },
        { value: `${dynamicStats.yearsExperience}+`, label: "Years Experience", icon: <FaRegStar className="w-8 h-8" /> }
    ];

    // Featured services
    const featuredServices = [
        {
            title: "Custom Software Development",
            description: "Tailored solutions built with cutting-edge technologies to meet your unique business needs",
            icon: <FaCode className="w-12 h-12" />,
            href: "/services/development"
        },
        {
            title: "Mobile App Development",
            description: "Cross-platform mobile applications that engage users on iOS and Android devices",
            icon: <FaMobileAlt className="w-12 h-12" />,
            href: "/services#mobile"
        },
        {
            title: "E-commerce Solutions",
            description: "Complete online store development with payment integration and inventory management",
            icon: <FaShoppingCart className="w-12 h-12" />,
            href: "/services#ecommerce"
        },
        {
            title: "Web Design & Development",
            description: "Beautiful, responsive websites that convert visitors into customers",
            icon: <FaPalette className="w-12 h-12" />,
            href: "/services/design"
        },
        {
            title: "Cloud Solutions",
            description: "Scalable cloud infrastructure on AWS, Azure, and Google Cloud Platform",
            icon: <FaCloud className="w-12 h-12" />,
            href: "/services/hosting"
        },
        {
            title: "Cyber Security",
            description: "Comprehensive security audits, penetration testing, and compliance consulting",
            icon: <FaShieldAlt className="w-12 h-12" />,
            href: "/services#security"
        }
    ];

    // Why choose us
    const benefits = [
        {
            title: "Africa-Focused Expertise",
            description: "Deep understanding of African markets, payment systems, and infrastructure challenges",
            icon: <FaCheckCircle className="w-10 h-10" />
        },
        {
            title: "Enterprise-Grade Solutions",
            description: "Professional solutions built to scale with your business across the continent",
            icon: <FaCheckCircle className="w-10 h-10" />
        },
        {
            title: "End-to-End Support",
            description: "From ideation to deployment and beyond, we're with you every step of the way",
            icon: <FaCheckCircle className="w-10 h-10" />
        },
        {
            title: "Proven Track Record",
            description: "Successfully delivered 100+ projects across 8 African countries",
            icon: <FaCheckCircle className="w-10 h-10" />
        }
    ];

    return (
        <>
            <div className="relative z-10">
                {/* Hero Section with Custom Circuit Board Background */}
                <section className="relative py-28 px-4 sm:px-8 md:px-16 lg:px-24 text-center overflow-hidden">
                    {/* Custom Circuit Board Background */}
                    <div className="absolute inset-0 bg-white dark:bg-gray-950">
                        <div className="absolute inset-0">
                            <svg className="absolute inset-0 w-full h-full opacity-30 dark:opacity-20" xmlns="http://www.w3.org/2000/svg">
                                <pattern id="circuit-advanced" x="0" y="0" width="100" height="100" patternUnits="userSpaceOnUse">
                                    {/* Nodes */}
                                    <circle cx="10" cy="10" r="2" className="fill-blue-500 dark:fill-blue-400" />
                                    <circle cx="50" cy="10" r="2" className="fill-blue-500 dark:fill-blue-400" />
                                    <circle cx="90" cy="10" r="2" className="fill-blue-500 dark:fill-blue-400" />
                                    <circle cx="10" cy="50" r="2" className="fill-blue-500 dark:fill-blue-400" />
                                    <circle cx="50" cy="50" r="3" className="fill-purple-500 dark:fill-purple-400" />
                                    <circle cx="90" cy="50" r="2" className="fill-blue-500 dark:fill-blue-400" />
                                    <circle cx="10" cy="90" r="2" className="fill-blue-500 dark:fill-blue-400" />
                                    <circle cx="50" cy="90" r="2" className="fill-blue-500 dark:fill-blue-400" />
                                    <circle cx="90" cy="90" r="2" className="fill-blue-500 dark:fill-blue-400" />

                                    {/* Connections */}
                                    <path
                                        d="M10 10 H 30 V 50 H 10 M50 10 V 30 H 90 M50 50 H 70 V 90 M10 50 H 30 M50 50 H 70 M90 50 V 70 H 70"
                                        stroke="currentColor"
                                        strokeWidth="1"
                                        className="text-blue-500 dark:text-blue-400"
                                        fill="none"
                                    />
                                </pattern>
                                <rect width="100%" height="100%" fill="url(#circuit-advanced)" />
                            </svg>
                        </div>
                        <div className="absolute inset-0 bg-gradient-to-br from-blue-50/30 via-transparent to-purple-50/30 dark:from-blue-950/20 dark:via-transparent dark:to-purple-950/20" />
                    </div>

                    {/* Content Container */}
                    <div className="relative max-w-5xl mx-auto">
                        {/* Animated headline with gradient text */}
                        <h1 className="text-5xl sm:text-6xl md:text-7xl font-extrabold tracking-tight leading-tight mb-6">
                            <span className="bg-clip-text text-transparent bg-gradient-to-r from-blue-900 to-blue-600 dark:from-blue-400 dark:to-blue-200 animate-text-shimmer">
                                Next-Gen Tech Solutions
                            </span>
                        </h1>

                        {/* Subheading with subtle animation */}
                        <p className="mt-8 text-lg sm:text-xl text-gray-700 dark:text-gray-300 max-w-3xl mx-auto animate-fade-in-up">
                            Powering digital transformation across Africa and beyond. From Cape Town to Cairo, Lagos to Nairobi, we deliver enterprise-grade cloud solutions, AI integration, and custom software development that drives business growth globally.
                        </p>

                        {/* CTA Buttons with hover effects */}
                        <div className="mt-12 flex flex-col sm:flex-row justify-center gap-4 sm:gap-6">
                            <button
                                onClick={handleWhatsAppClick}
                                className="flex items-center justify-center gap-3 px-8 py-4 rounded-lg bg-gradient-to-r from-blue-600 to-blue-700 hover:from-blue-700 hover:to-blue-800 text-white font-semibold transition-all transform hover:-translate-y-0.5 shadow-lg hover:shadow-xl animate-pulse-slow"
                            >
                                <FaWhatsapp className="w-5 h-5" />
                                Start Your Journey
                            </button>
                            <Link
                                href="/contact"
                                className="px-8 py-4 rounded-lg border-2 border-blue-600 hover:border-blue-700 text-blue-600 hover:text-blue-700 dark:border-blue-400 dark:text-blue-400 dark:hover:bg-blue-900/30 font-semibold transition-colors backdrop-blur-sm"
                            >
                                Talk to an Expert
                            </Link>
                        </div>

                        {/* Decorative elements that match circuit theme */}
                        <div className="absolute -bottom-20 -left-20 w-40 h-40 bg-blue-400 rounded-full filter blur-3xl opacity-10 dark:opacity-5 animate-float"></div>
                        <div className="absolute -top-10 -right-10 w-32 h-32 bg-purple-400 rounded-full filter blur-3xl opacity-10 dark:opacity-5 animate-float-delay"></div>
                    </div>

                    {/* Scrolling animation indicator */}
                    <div className="absolute bottom-8 left-1/2 transform -translate-x-1/2 animate-bounce">
                        <div className="w-6 h-10 border-2 border-blue-600 dark:border-blue-400 rounded-full flex justify-center">
                            <div className="w-1 h-2 bg-blue-600 dark:bg-blue-400 rounded-full mt-2 animate-scroll-indicator"></div>
                        </div>
                    </div>
                </section>

                {/* Domain Search Section */}
                <section className="relative py-16 px-4 sm:px-8 bg-gradient-to-br from-blue-600 to-blue-800 dark:from-blue-900 dark:to-blue-950 overflow-hidden">
                    {/* Background Pattern */}
                    <div className="absolute inset-0 opacity-10">
                        <div className="absolute inset-0 bg-[radial-gradient(circle_at_1px_1px,rgba(255,255,255,0.45)_1px,transparent_0)] bg-[length:24px_24px]"></div>
                    </div>

                    <div className="relative max-w-6xl mx-auto">
                        <div className="text-center mb-10">
                            <div className="inline-flex items-center gap-2 mb-4">
                                <FaGlobe className="w-8 h-8 text-white" />
                                <h2 className="text-3xl md:text-4xl font-bold text-white">
                                    Find Your Perfect Domain
                                </h2>
                            </div>
                            <p className="text-lg text-blue-100 max-w-2xl mx-auto">
                                Start your online journey with a memorable domain name
                            </p>
                        </div>

                        {/* Search Form */}
                        <form onSubmit={handleDomainSearch} className="max-w-4xl mx-auto mb-8">
                            <div className="flex flex-col md:flex-row gap-3 bg-white dark:bg-gray-900 rounded-2xl p-3 shadow-2xl">
                                <div className="flex-1 flex items-center gap-2 px-4">
                                    <FaSearch className="text-gray-400 w-5 h-5" />
                                    <input
                                        type="text"
                                        value={domainSearch}
                                        onChange={(e) => setDomainSearch(e.target.value)}
                                        placeholder="Enter your domain name..."
                                        className="flex-1 py-3 text-lg bg-transparent border-none outline-none text-gray-900 dark:text-white placeholder-gray-400"
                                        required
                                    />
                                </div>
                                <select
                                    value={selectedExtension}
                                    onChange={(e) => setSelectedExtension(e.target.value)}
                                    className="px-4 py-3 bg-gray-50 dark:bg-gray-800 text-gray-900 dark:text-white rounded-lg border-none outline-none cursor-pointer font-medium"
                                >
                                    {popularExtensions.map((item) => (
                                        <option key={item.ext} value={item.ext}>
                                            {item.ext}
                                        </option>
                                    ))}
                                </select>
                                <button
                                    type="submit"
                                    className="px-8 py-3 bg-gradient-to-r from-blue-600 to-blue-700 hover:from-blue-700 hover:to-blue-800 text-white font-semibold rounded-lg transition-all transform hover:scale-105 shadow-lg"
                                >
                                    Search
                                </button>
                            </div>
                        </form>

                        {/* Popular Extensions */}
                        <div className="max-w-4xl mx-auto">
                            <p className="text-center text-blue-100 mb-4 text-sm font-medium">
                                Popular Extensions
                            </p>
                            <div className="grid grid-cols-2 md:grid-cols-3 lg:grid-cols-6 gap-3">
                                {popularExtensions.map((item) => (
                                    <button
                                        key={item.ext}
                                        onClick={() => setSelectedExtension(item.ext)}
                                        className={`p-4 bg-white/10 backdrop-blur-sm rounded-xl border-2 transition-all hover:bg-white/20 hover:scale-105 ${
                                            selectedExtension === item.ext
                                                ? 'border-white bg-white/20'
                                                : 'border-white/30'
                                        }`}
                                    >
                                        <div className="text-white font-bold text-lg mb-1">
                                            {item.ext}
                                        </div>
                                        <div className="text-blue-100 text-sm">
                                            {item.price}/yr
                                        </div>
                                    </button>
                                ))}
                            </div>
                        </div>

                        {/* Quick Link */}
                        <div className="text-center mt-8">
                            <Link
                                href="/services/domains"
                                className="inline-flex items-center gap-2 text-white hover:text-blue-100 transition-colors font-medium"
                            >
                                View all domain options
                                <FaArrowRight className="w-4 h-4" />
                            </Link>
                        </div>
                    </div>
                </section>

                {/* Stats Section */}
                <section className="relative py-20 bg-gray-50 dark:bg-gray-900 overflow-hidden">
                    <div className="relative max-w-6xl mx-auto px-4 sm:px-8">
                        <h2 className="text-3xl font-bold text-center text-blue-900 dark:text-blue-300 mb-16">
                            <span className="relative inline-block">
                                <span className="relative z-10 px-4">Our Impact in Numbers</span>
                                <span className="absolute bottom-0 left-0 w-full h-2 bg-blue-200 dark:bg-blue-800/50 z-0"></span>
                            </span>
                        </h2>

                        <div className="grid grid-cols-1 md:grid-cols-4 gap-8">
                            {stats.map((stat, index) => (
                                <div
                                    key={index}
                                    className="relative p-6 text-center bg-white dark:bg-gray-800 backdrop-blur-sm rounded-xl shadow-sm hover:shadow-lg transition-all duration-300 hover:-translate-y-2 border border-gray-200 dark:border-gray-700"
                                >
                                    <div className="text-4xl text-blue-600 dark:text-blue-400 mb-4 flex justify-center">
                                        {stat.icon}
                                    </div>
                                    <div className="text-4xl font-bold text-gray-900 dark:text-white mb-2">
                                        <span className="bg-clip-text text-transparent bg-gradient-to-r from-blue-600 to-blue-800 dark:from-blue-400 dark:to-blue-300">
                                            {stat.value}
                                        </span>
                                    </div>
                                    <div className="text-gray-600 dark:text-gray-300 uppercase text-sm tracking-wide font-medium">
                                        {stat.label}
                                    </div>
                                </div>
                            ))}
                        </div>
                    </div>
                </section>

                {/* Featured Services */}
                <section className="py-24 px-4 sm:px-8 bg-white dark:bg-gray-950">
                    <div className="max-w-6xl mx-auto">
                        <div className="text-center mb-16">
                            <h2 className="text-4xl font-bold text-blue-900 dark:text-blue-300 mb-4">
                                Our Services
                            </h2>
                            <p className="text-lg text-gray-600 dark:text-gray-400 max-w-2xl mx-auto">
                                Comprehensive digital solutions tailored for African businesses and beyond
                            </p>
                        </div>

                        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-8">
                            {featuredServices.map((service, index) => (
                                <Link
                                    key={index}
                                    href={service.href}
                                    className="group p-8 bg-white dark:bg-gray-900 rounded-2xl border border-gray-200 dark:border-gray-800 hover:border-blue-400 dark:hover:border-blue-300 transition-all duration-300 hover:-translate-y-2 hover:shadow-xl"
                                >
                                    <div className="text-blue-600 dark:text-blue-400 mb-6 transition-transform group-hover:scale-110">
                                        {service.icon}
                                    </div>
                                    <h3 className="text-2xl font-semibold text-gray-900 dark:text-white mb-4 group-hover:text-blue-600 dark:group-hover:text-blue-400 transition-colors">
                                        {service.title}
                                    </h3>
                                    <p className="text-gray-600 dark:text-gray-400 leading-relaxed mb-4">
                                        {service.description}
                                    </p>
                                    <div className="flex items-center text-blue-600 dark:text-blue-400 opacity-0 group-hover:opacity-100 translate-x-[-10px] group-hover:translate-x-0 transition-all duration-300">
                                        <span className="text-sm font-medium">Learn more</span>
                                        <FaArrowRight className="w-4 h-4 ml-2" />
                                    </div>
                                </Link>
                            ))}
                        </div>

                        <div className="text-center mt-12">
                            <Link
                                href="/services"
                                className="inline-flex items-center gap-2 px-8 py-4 bg-blue-600 hover:bg-blue-700 text-white font-semibold rounded-lg transition-all transform hover:scale-105"
                            >
                                View All Services
                                <FaArrowRight className="w-4 h-4" />
                            </Link>
                        </div>
                    </div>
                </section>

                {/* Why Choose Us */}
                <section className="py-24 px-4 sm:px-8 bg-gradient-to-br from-blue-50 to-purple-50 dark:from-gray-900 dark:to-blue-950">
                    <div className="max-w-6xl mx-auto">
                        <div className="text-center mb-16">
                            <h2 className="text-4xl font-bold text-blue-900 dark:text-blue-300 mb-4">
                                Why Choose Slyker Tech Web Services
                            </h2>
                            <p className="text-lg text-gray-600 dark:text-gray-400 max-w-2xl mx-auto">
                                Your trusted partner for digital transformation across Africa
                            </p>
                        </div>

                        <div className="grid grid-cols-1 md:grid-cols-2 gap-8">
                            {benefits.map((benefit, index) => (
                                <div
                                    key={index}
                                    className="flex gap-6 p-8 bg-white dark:bg-gray-800 rounded-xl border border-gray-200 dark:border-gray-700 hover:border-blue-400 dark:hover:border-blue-300 transition-all hover:shadow-lg"
                                >
                                    <div className="flex-shrink-0 text-blue-600 dark:text-blue-400">
                                        {benefit.icon}
                                    </div>
                                    <div>
                                        <h3 className="text-xl font-semibold text-gray-900 dark:text-white mb-3">
                                            {benefit.title}
                                        </h3>
                                        <p className="text-gray-600 dark:text-gray-400 leading-relaxed">
                                            {benefit.description}
                                        </p>
                                    </div>
                                </div>
                            ))}
                        </div>
                    </div>
                </section>

                {/* Payment Methods Section */}
                <section className="relative py-16 bg-white dark:bg-gray-950 overflow-hidden">
                    <div className="absolute inset-0 opacity-5 dark:opacity-[0.02]">
                        <svg className="w-full h-full" xmlns="http://www.w3.org/2000/svg">
                            <pattern id="payment-pattern" x="0" y="0" width="80" height="80" patternUnits="userSpaceOnUse">
                                <circle cx="40" cy="40" r="1.5" className="fill-blue-500" />
                            </pattern>
                            <rect width="100%" height="100%" fill="url(#payment-pattern)" />
                        </svg>
                    </div>
                    
                    <div className="relative max-w-6xl mx-auto px-4 sm:px-8">
                        <div className="text-center mb-12">
                            <h3 className="text-3xl font-bold text-blue-900 dark:text-blue-300 mb-4">
                                Flexible Payment Options
                            </h3>
                            <p className="text-lg text-gray-600 dark:text-gray-400">
                                We accept all major payment methods for your convenience
                            </p>
                        </div>

                        <div className="grid grid-cols-1 md:grid-cols-3 gap-8">
                            {/* Card Payments */}
                            <div className="p-8 bg-white/90 dark:bg-gray-900/90 backdrop-blur-sm rounded-2xl border border-gray-200 dark:border-gray-800 hover:border-blue-400 dark:hover:border-blue-300 transition-all hover:shadow-lg">
                                <div className="flex justify-center gap-4 mb-6">
                                    <FaCcVisa className="text-5xl text-blue-600 dark:text-blue-400" />
                                    <FaCcMastercard className="text-5xl text-orange-500 dark:text-orange-400" />
                                    <FaCreditCard className="text-5xl text-gray-600 dark:text-gray-400" />
                                </div>
                                <h4 className="text-xl font-semibold text-center text-gray-900 dark:text-white mb-2">
                                    Credit & Debit Cards
                                </h4>
                                <p className="text-center text-gray-600 dark:text-gray-400">
                                    Visa, Mastercard, and all major cards accepted
                                </p>
                            </div>

                            {/* Mobile Payments */}
                            <div className="p-8 bg-white/90 dark:bg-gray-900/90 backdrop-blur-sm rounded-2xl border border-gray-200 dark:border-gray-800 hover:border-blue-400 dark:hover:border-blue-300 transition-all hover:shadow-lg">
                                <div className="flex justify-center mb-6">
                                    <FaMobileAlt className="text-6xl text-green-600 dark:text-green-400" />
                                </div>
                                <h4 className="text-xl font-semibold text-center text-gray-900 dark:text-white mb-2">
                                    Mobile Money
                                </h4>
                                <p className="text-center text-gray-600 dark:text-gray-400">
                                    EcoCash, M-Pesa, and other mobile payment platforms
                                </p>
                            </div>

                            {/* Cryptocurrency */}
                            <div className="p-8 bg-white/90 dark:bg-gray-900/90 backdrop-blur-sm rounded-2xl border border-gray-200 dark:border-gray-800 hover:border-blue-400 dark:hover:border-blue-300 transition-all hover:shadow-lg">
                                <div className="flex justify-center mb-6">
                                    <FaBitcoin className="text-6xl text-orange-500 dark:text-orange-400" />
                                </div>
                                <h4 className="text-xl font-semibold text-center text-gray-900 dark:text-white mb-2">
                                    Cryptocurrency
                                </h4>
                                <p className="text-center text-gray-600 dark:text-gray-400">
                                    Bitcoin and other digital currencies accepted
                                </p>
                            </div>
                        </div>

                        <div className="mt-12 text-center">
                            <p className="text-sm text-gray-500 dark:text-gray-400">
                                🔒 All transactions are secured with industry-standard encryption
                            </p>
                        </div>
                    </div>
                </section>

                {/* Promotions CTA */}
                <PromotionsCTA />

                {/* FAQ Section */}
                <div className="bg-gray-50 dark:bg-gray-900">
                    <ServiceFAQ 
                        faqs={companyFAQs}
                        serviceName="Slyker Tech"
                    />
                    <div className="text-center pb-16">
                        <Link
                            href="/about#faqs"
                            className="inline-flex items-center gap-2 px-6 py-3 bg-blue-600 text-white font-semibold rounded-lg hover:bg-blue-700 transition-all"
                        >
                            View More FAQs
                            <FaArrowRight />
                        </Link>
                    </div>
                </div>

                {/* Final CTA */}
                <section className="py-24 px-4 sm:px-8 bg-gradient-to-br from-blue-600 to-blue-800 dark:from-blue-900 dark:to-blue-950">
                    <div className="max-w-4xl mx-auto text-center">
                        <h2 className="text-4xl md:text-5xl font-bold text-white mb-6">
                            Ready to Transform Your Business?
                        </h2>
                        <p className="text-xl text-blue-100 dark:text-blue-200 mb-8">
                            Let&apos;s discuss how we can help you achieve your digital goals
                        </p>
                        <div className="flex flex-col sm:flex-row justify-center gap-4">
                            <button
                                onClick={handleWhatsAppClick}
                                className="flex items-center justify-center gap-2 px-8 py-4 bg-white hover:bg-gray-100 text-blue-700 font-semibold rounded-lg transition-all transform hover:scale-105"
                            >
                                <FaWhatsapp className="w-5 h-5" />
                                Chat on WhatsApp
                            </button>
                            <Link
                                href="/contact"
                                className="px-8 py-4 border-2 border-white text-white hover:bg-white hover:text-blue-700 rounded-lg font-semibold transition-all"
                            >
                                Contact Our Team
                            </Link>
                        </div>
                    </div>
                </section>
            </div>
        </>
    );
};

export default HomePageClientView;
