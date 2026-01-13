'use client';

import React, { useState, useEffect } from 'react';
import Link from 'next/link';
import {
    FaWhatsapp, FaCloud, FaChartLine, FaCogs,
    FaGoogle, FaAmazon, FaShieldAlt, FaMobileAlt, FaCode, FaDesktop,
    FaServer, FaUserShield, FaNetworkWired, FaPython, FaPhp,
    FaLaravel, FaAndroid, FaApple, FaDocker, FaAws,
    FaDatabase, FaReact, FaNodeJs, FaRegStar, FaRegComments
} from 'react-icons/fa';
import {
    MdSecurity, MdSupport, MdOutlineEngineering,
    MdAutorenew, MdIntegrationInstructions
} from 'react-icons/md';
import {
    SiTesla, SiNetflix, SiSpotify, SiDjango, SiKubernetes,
    SiTerraform, SiFlutter, SiFastapi, SiSymfony,
    SiMongodb, SiRedis, SiFlask, SiPandas, SiAngular, SiVuedotjs,
    SiNextdotjs, SiGo, SiGin
} from 'react-icons/si';
import { apiService } from '@/lib/api-service';

const HomePageClientView = () => {
    const [activeFeatureSlide, setActiveFeatureSlide] = useState(0);
    const [activeSolutionSlide, setActiveSolutionSlide] = useState(0);
    const [activeTechSlide, setActiveTechSlide] = useState(0);
    const [autoFeatureSlide, setAutoFeatureSlide] = useState(true);
    const [autoSolutionSlide, setAutoSolutionSlide] = useState(true);
    const [autoTechSlide, setAutoTechSlide] = useState(true);
    const [dynamicStats, setDynamicStats] = useState({
        uptime: 99.99,
        activeClients: 5,
        engineers: 1,
        yearsExperience: 4
    });
    const [featuredServices, setFeaturedServices] = useState<any[]>([]);

    const handleWhatsAppClick = () => {
        const message = encodeURIComponent("Hi Slyker Tech! I'm interested in your services.");
        window.open(`https://wa.me/263787211325?text=${message}`, '_blank');
    };

    // Fetch dynamic data from backend
    useEffect(() => {
        const fetchDynamicData = async () => {
            try {
                // Fetch stats
                const statsResponse = await apiService.getPublicStats();
                if (statsResponse.data) {
                    setDynamicStats({
                        uptime: statsResponse.data.uptime || 99.99,
                        activeClients: statsResponse.data.activeClients || 5,
                        engineers: statsResponse.data.engineers || 1,
                        yearsExperience: statsResponse.data.yearsExperience || 4
                    });
                }

                // Fetch services
                const servicesResponse = await apiService.getPublicServices();
                if (servicesResponse.data) {
                    const services = servicesResponse.data.results || servicesResponse.data;
                    setFeaturedServices(services.slice(0, 6)); // Get first 6 services
                }
            } catch (error) {
                console.error('Failed to fetch dynamic data:', error);
                // Keep default values on error
            }
        };

        fetchDynamicData();
    }, []);

    // Stats data (now dynamic)
    const stats = [
        { value: `${dynamicStats.uptime}%`, label: "Uptime SLA", icon: <FaServer className="w-8 h-8" /> },
        { value: `${dynamicStats.activeClients}+`, label: "Active Clients", icon: <FaUserShield className="w-8 h-8" /> },
        { value: `${dynamicStats.engineers}`, label: "Expert Engineers", icon: <FaCode className="w-8 h-8" /> },
        { value: `${dynamicStats.yearsExperience}+`, label: "Years Experience", icon: <FaRegStar className="w-8 h-8" /> }
    ];

    // Feature carousel slides
    const featureSlides = [
        [
            {
                title: "Cloud Infrastructure",
                description: "Multi-cloud solutions with AWS, GCP, and Azure integration",
                icon: <FaCloud className="w-12 h-12" />
            },
            {
                title: "AI Integration",
                description: "Machine learning pipelines and predictive analytics",
                icon: <FaChartLine className="w-12 h-12" />
            },
            {
                title: "DevOps Automation",
                description: "CI/CD pipelines and Kubernetes orchestration",
                icon: <FaCogs className="w-12 h-12" />
            }
        ],
        [
            {
                title: "Enterprise Security",
                description: "End-to-end encryption and compliance frameworks",
                icon: <MdSecurity className="w-12 h-12" />
            },
            {
                title: "Global Support",
                description: "24/7 monitoring and rapid response teams",
                icon: <MdSupport className="w-12 h-12" />
            },
            {
                title: "App Modernization",
                description: "Legacy system upgrades and microservices architecture",
                icon: <MdOutlineEngineering className="w-12 h-12" />
            }
        ]
    ];

    // Solution carousel slides
    const solutionSlides = [
        [
            {
                title: "Custom Software",
                description: "Bespoke solutions for unique business challenges",
                icon: <FaCode className="w-12 h-12" />
            },
            {
                title: "Mobile Development",
                description: "Cross-platform apps with React Native & Flutter",
                icon: <FaMobileAlt className="w-12 h-12" />
            },
            {
                title: "Cloud Migration",
                description: "Secure transition to cloud environments",
                icon: <FaServer className="w-12 h-12" />
            }
        ],
        [
            {
                title: "Data Analytics",
                description: "Transform data into actionable insights",
                icon: <FaDatabase className="w-12 h-12" />
            },
            {
                title: "Cybersecurity",
                description: "Comprehensive audits & threat prevention",
                icon: <FaShieldAlt className="w-12 h-12" />
            },
            {
                title: "Digital Strategy",
                description: "End-to-end transformation services",
                icon: <MdAutorenew className="w-12 h-12" />
            }
        ],
        [
            {
                title: "API Integration",
                description: "Seamless system connectivity",
                icon: <MdIntegrationInstructions className="w-12 h-12" />
            },
            {
                title: "Enterprise Software",
                description: "Scalable operational solutions",
                icon: <FaDesktop className="w-12 h-12" />
            },
            {
                title: "Network Design",
                description: "High-performance infrastructure",
                icon: <FaNetworkWired className="w-12 h-12" />
            }
        ]
    ];

    // Technology carousel slides
    const techSlides = [
        [
            { icon: <FaPython className="w-12 h-12" />, name: "Python" },
            { icon: <SiDjango className="w-12 h-12" />, name: "Django" },
            { icon: <SiFastapi className="w-12 h-12" />, name: "FastAPI" },
            { icon: <SiFlask className="w-12 h-12" />, name: "Flask" },
            { icon: <SiPandas className="w-12 h-12" />, name: "Pandas" },
        ],
        [
            { icon: <FaReact className="w-12 h-12" />, name: "React" },
            { icon: <SiAngular className="w-12 h-12" />, name: "Angular" },
            { icon: <SiVuedotjs className="w-12 h-12" />, name: "Vue.js" },
            { icon: <FaNodeJs className="w-12 h-12" />, name: "Node.js" },
            { icon: <SiNextdotjs className="w-12 h-12" />, name: "Next.js" },
        ],
        [
            { icon: <FaPhp className="w-12 h-12" />, name: "PHP" },
            { icon: <SiSymfony className="w-12 h-12" />, name: "Symfony" },
            { icon: <FaLaravel className="w-12 h-12" />, name: "Laravel" },
            { icon: <SiGo className="w-12 h-12" />, name: "Go" },
            { icon: <SiGin className="w-12 h-12" />, name: "Gin" },
        ],
        [
            { icon: <FaDatabase className="w-12 h-12" />, name: "PostgreSQL" },
            { icon: <SiMongodb className="w-12 h-12" />, name: "MongoDB" },
            { icon: <SiRedis className="w-12 h-12" />, name: "Redis" },
            { icon: <FaDocker className="w-12 h-12" />, name: "Docker" },
            { icon: <SiKubernetes className="w-12 h-12" />, name: "Kubernetes" },
        ],
        [
            { icon: <FaAws className="w-12 h-12" />, name: "AWS" },
            { icon: <SiTerraform className="w-12 h-12" />, name: "Terraform" },
            { icon: <FaAndroid className="w-12 h-12" />, name: "Android" },
            { icon: <FaApple className="w-12 h-12" />, name: "iOS" },
            { icon: <SiFlutter className="w-12 h-12" />, name: "Flutter" },
        ]
    ];

    // Testimonials data
    const testimonials = [
        {
            text: "Reduced our infrastructure costs by 40% while improving performance metrics across the board.",
            author: "CTO, TechNova",
            role: "Enterprise SaaS Platform"
        },
        {
            text: "Implemented AI solutions that delivered actionable insights within weeks of deployment.",
            author: "Head of Analytics, FinCorp",
            role: "Financial Services"
        }
    ];

    // Carousel effects
    useEffect(() => {
        if (autoFeatureSlide) {
            const interval = setInterval(() => {
                setActiveFeatureSlide((prev) => (prev + 1) % featureSlides.length);
            }, 5000);
            return () => clearInterval(interval);
        }
    }, [autoFeatureSlide, featureSlides.length]);

    useEffect(() => {
        if (autoSolutionSlide) {
            const interval = setInterval(() => {
                setActiveSolutionSlide((prev) => (prev + 1) % solutionSlides.length);
            }, 6000);
            return () => clearInterval(interval);
        }
    }, [autoSolutionSlide, solutionSlides.length]);

    useEffect(() => {
        if (autoTechSlide) {
            const interval = setInterval(() => {
                setActiveTechSlide((prev) => (prev + 1) % techSlides.length);
            }, 5000);
            return () => clearInterval(interval);
        }
    }, [autoTechSlide, techSlides.length]);

    return (
        <> {/* Added a fragment here */}
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
                            Empowering enterprises with full-stack digital transformation services across cloud, AI, and modern application development.
                        </p>

                        {/* CTA Buttons with hover effects */}
                        <div className="mt-12 flex flex-col sm:flex-row justify-center gap-4 sm:gap-6">
                            <button
                                onClick={handleWhatsAppClick}
                                className="flex items-center justify-center gap-3 px-8 py-4 rounded-lg bg-gradient-to-r from-blue-600 to-blue-700 hover:from-blue-700 hover:to-blue-800 text-white font-semibold transition-all transform hover:-translate-y-0.5 shadow-lg hover:shadow-xl animate-pulse-slow"
                            >
                                <FaWhatsapp className="w-5 h-5" />
                                Get Started Now
                            </button>
                            <Link
                                href="/services"
                                className="px-8 py-4 rounded-lg border-2 border-blue-600 hover:border-blue-700 text-blue-600 hover:text-blue-700 dark:border-blue-400 dark:text-blue-400 dark:hover:bg-blue-900/30 font-semibold transition-colors backdrop-blur-sm"
                            >
                                Explore Our Services
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

                {/* Stats Section with Animated Circuit Nodes */}
                <section className="relative py-20 bg-white dark:bg-gray-950 overflow-hidden">
                    {/* Circuit node background animation */}
                    <div className="absolute inset-0 opacity-10 dark:opacity-[0.05]">
                        <div className="absolute inset-0 flex items-center justify-center">
                            <div className="relative w-full h-full">
                                {/* Animated circuit nodes */}
                                {[...Array(12)].map((_, i) => (
                                    <div
                                        key={i}
                                        className="absolute rounded-full bg-blue-400 dark:bg-blue-500 opacity-20"
                                        style={{
                                            width: `${Math.random() * 10 + 5}px`,
                                            height: `${Math.random() * 10 + 5}px`,
                                            left: `${Math.random() * 100}%`,
                                            top: `${Math.random() * 100}%`,
                                            animation: `pulse ${Math.random() * 6 + 4}s infinite alternate`,
                                            animationDelay: `${Math.random() * 5}s`
                                        }}
                                    />
                                ))}
                            </div>
                        </div>
                    </div>

                    {/* Subtle circuit connections */}
                    <div className="absolute inset-0 opacity-5 dark:opacity-[0.03] pointer-events-none">
                        <svg className="w-full h-full" xmlns="http://www.w3.org/2000/svg">
                            <path
                                d="M10,10 Q50,20 90,10 T170,50 Q190,90 250,90"
                                stroke="currentColor"
                                strokeWidth="1"
                                strokeDasharray="5,5"
                                className="text-blue-400 dark:text-blue-500"
                                fill="none"
                            />
                            <path
                                d="M50,150 Q100,100 150,150 T250,100"
                                stroke="currentColor"
                                strokeWidth="1"
                                strokeDasharray="5,5"
                                className="text-purple-400 dark:text-purple-500"
                                fill="none"
                            />
                        </svg>
                    </div>

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
                                    className="relative p-6 text-center bg-white/90 dark:bg-gray-900/90 backdrop-blur-sm rounded-xl shadow-sm hover:shadow-lg transition-all duration-300 hover:-translate-y-2 border border-gray-200 dark:border-gray-800 overflow-hidden group"
                                >
                                    {/* Animated highlight on hover */}
                                    <div className="absolute inset-0 bg-gradient-to-br from-blue-50 to-purple-50 dark:from-blue-900/30 dark:to-purple-900/30 opacity-0 group-hover:opacity-100 transition-opacity duration-300"></div>

                                    {/* Circuit node decoration */}
                                    <div className="absolute top-4 right-4 w-3 h-3 rounded-full bg-blue-500 dark:bg-blue-400 opacity-70"></div>

                                    <div className="relative z-10">
                                        <div className="text-4xl text-blue-600 dark:text-blue-400 mb-4 transition-transform duration-300 group-hover:scale-110">
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
                                </div>
                            ))}
                        </div>
                    </div>
                </section>

                {/* Features Carousel with Circuit Connections */}
                <section className="relative py-24 px-4 sm:px-8 bg-gray-50 dark:bg-gray-900 overflow-hidden">
                    {/* Animated circuit background */}
                    <div className="absolute inset-0 opacity-10 dark:opacity-[0.03]">
                        <svg className="w-full h-full" xmlns="http://www.w3.org/2000/svg">
                            <pattern id="circuit-pattern" x="0" y="0" width="150" height="150" patternUnits="userSpaceOnUse">
                                {/* Circuit nodes */}
                                <circle cx="20" cy="20" r="2" className="fill-blue-500 dark:fill-blue-400" />
                                <circle cx="75" cy="20" r="3" className="fill-purple-500 dark:fill-purple-400" />
                                <circle cx="130" cy="20" r="2" className="fill-blue-500 dark:fill-blue-400" />
                                <circle cx="20" cy="75" r="2" className="fill-blue-500 dark:fill-blue-400" />
                                <circle cx="130" cy="75" r="2" className="fill-blue-500 dark:fill-blue-400" />
                                <circle cx="20" cy="130" r="2" className="fill-blue-500 dark:fill-blue-400" />
                                <circle cx="75" cy="130" r="3" className="fill-purple-500 dark:fill-purple-400" />
                                <circle cx="130" cy="130" r="2" className="fill-blue-500 dark:fill-blue-400" />

                                {/* Circuit connections */}
                                <path
                                    d="M20 20 H75 V75 H130 M75 20 V75 M20 75 H75 V130"
                                    stroke="currentColor"
                                    strokeWidth="1"
                                    className="text-blue-500 dark:text-blue-400"
                                    fill="none"
                                />
                            </pattern>
                            <rect width="100%" height="100%" fill="url(#circuit-pattern)" />
                        </svg>
                    </div>

                    <div className="relative max-w-6xl mx-auto">
                        <div className="text-center mb-16">
                            <h2 className="text-4xl font-bold text-blue-900 dark:text-blue-300 mb-4">
                                <span className="relative inline-block">
                                    <span className="relative z-10 px-4">Core Capabilities</span>
                                    <span className="absolute bottom-0 left-0 w-full h-2 bg-blue-200 dark:bg-blue-800/50 z-0"></span>
                                </span>
                            </h2>
                            <p className="text-lg text-gray-600 dark:text-gray-400 max-w-2xl mx-auto">
                                Advanced solutions powering your digital transformation journey
                            </p>
                        </div>

                        <div
                            className="relative overflow-hidden mb-12"
                            onMouseEnter={() => setAutoFeatureSlide(false)}
                            onMouseLeave={() => setAutoFeatureSlide(true)}
                        >
                            <div
                                className="flex transition-transform duration-700 ease-[cubic-bezier(0.16,1,0.3,1)]"
                                style={{ transform: `translateX(-${activeFeatureSlide * 100}%)` }}
                            >
                                {featureSlides.map((slide, index) => (
                                    <div
                                        key={index}
                                        className="min-w-full grid grid-cols-1 md:grid-cols-3 gap-8 px-4"
                                    >
                                        {slide.map((feature, i) => (
                                            <div
                                                key={i}
                                                className="group relative p-8 bg-white/90 dark:bg-gray-800/90 backdrop-blur-sm rounded-2xl border border-gray-200 dark:border-gray-700 hover:border-blue-400 dark:hover:border-blue-300 transition-all duration-500 hover:-translate-y-2 hover:shadow-lg overflow-hidden"
                                            >
                                                {/* Animated circuit connection on hover */}
                                                <div className="absolute inset-0 overflow-hidden opacity-0 group-hover:opacity-100 transition-opacity duration-500">
                                                    <svg className="absolute w-full h-full" xmlns="http://www.w3.org/2000/svg">
                                                        <path
                                                            d="M-20,50 Q50,0 120,50"
                                                            stroke="currentColor"
                                                            strokeWidth="1"
                                                            className="text-blue-400 dark:text-blue-300"
                                                            fill="none"
                                                            strokeDasharray="10,5"
                                                        />
                                                    </svg>
                                                </div>

                                                {/* Feature icon with gradient background */}
                                                <div className="relative z-10 mb-6">
                                                    <div className="absolute -top-2 -left-2 w-16 h-16 rounded-xl bg-gradient-to-br from-blue-100 to-purple-100 dark:from-blue-900/50 dark:to-purple-900/50 opacity-80"></div>
                                                    <div className="relative text-4xl text-blue-600 dark:text-blue-400">
                                                        {feature.icon}
                                                    </div>
                                                </div>

                                                <h3 className="relative z-10 text-2xl font-semibold mb-4 text-gray-900 dark:text-white group-hover:text-blue-600 dark:group-hover:text-blue-400 transition-colors">
                                                    {feature.title}
                                                </h3>
                                                <p className="relative z-10 text-gray-600 dark:text-gray-400 leading-relaxed">
                                                    {feature.description}
                                                </p>

                                                {/* Animated read more indicator */}
                                                <div className="relative z-10 mt-6 flex items-center text-blue-600 dark:text-blue-400 opacity-0 group-hover:opacity-100 translate-x-[-10px] group-hover:translate-x-0 transition-all duration-300">
                                                    <span className="text-sm font-medium">Learn more</span>
                                                    <svg className="w-4 h-4 ml-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                                                        <path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M9 5l7 7-7 7"></path>
                                                    </svg>
                                                </div>
                                            </div>
                                        ))}
                                    </div>
                                ))}
                            </div>
                        </div>

                        {/* Custom pagination with circuit nodes */}
                        <div className="flex justify-center items-center gap-4">
                            {featureSlides.map((_, index) => (
                                <button
                                    key={index}
                                    className="relative w-12 h-12 flex items-center justify-center group"
                                    onClick={() => {
                                        setActiveFeatureSlide(index);
                                        setAutoFeatureSlide(false);
                                        setTimeout(() => setAutoFeatureSlide(true), 5000);
                                    }}
                                >
                                    <div className={`absolute w-3 h-3 rounded-full transition-all ${index === activeFeatureSlide ? 'bg-blue-600 dark:bg-blue-400 scale-150' : 'bg-gray-300 dark:bg-gray-600'}`}></div>
                                    <div className={`absolute w-8 h-8 rounded-full border-2 transition-all ${index === activeFeatureSlide ? 'border-blue-400 dark:border-blue-300' : 'border-transparent'}`}></div>
                                    {index === activeFeatureSlide && (
                                        <div className="absolute w-12 h-12 rounded-full border-2 border-blue-200 dark:border-blue-800 animate-ping-slow opacity-0"></div>
                                    )}
                                </button>
                            ))}
                        </div>
                    </div>
                </section>
                {/* Solutions Carousel with Wave Pattern */}
                <section className="relative py-24 px-4 sm:px-8 bg-white dark:bg-gray-900 overflow-hidden">
                    <div className="absolute inset-0 opacity-10 dark:opacity-[0.03]">
                        <div className="absolute inset-0 bg-[url('https://bgvault.tech/wave.svg')] bg-[length:200px_200px]"></div>
                    </div>
                    <div className="relative max-w-6xl mx-auto">
                        <h2 className="text-4xl font-bold text-center text-blue-900 dark:text-blue-300 mb-12">
                            Digital Services
                        </h2>
                        <div
                            className="relative overflow-hidden"
                            onMouseEnter={() => setAutoSolutionSlide(false)}
                            onMouseLeave={() => setAutoSolutionSlide(true)}
                        >
                            <div className="flex transition-transform duration-500 ease-in-out"
                                style={{ transform: `translateX(-${activeSolutionSlide * 100}%)` }}
                            >
                                {solutionSlides.map((slide, index) => (
                                    <div
                                        key={index}
                                        className="min-w-full grid grid-cols-1 md:grid-cols-3 gap-8 px-4"
                                    >
                                        {slide.map((solution, i) => (
                                            <div key={i} className="group p-8 border border-gray-200 dark:border-gray-700 rounded-2xl bg-white/90 dark:bg-gray-800/90 backdrop-blur-sm hover:border-blue-600 dark:hover:border-blue-400 transition-all duration-300 hover:-translate-y-2 hover:shadow-lg">
                                                <div className="text-blue-700 dark:text-blue-400 mb-6">
                                                    {solution.icon}
                                                </div>
                                                <h3 className="text-2xl font-semibold mb-4 text-gray-900 dark:text-white">{solution.title}</h3>
                                                <p className="text-gray-600 dark:text-gray-400 leading-relaxed">{solution.description}</p>
                                            </div>
                                        ))}
                                    </div>
                                ))}
                            </div>
                        </div>
                        <div className="flex justify-center mt-8 gap-2">
                            {solutionSlides.map((_, index) => (
                                <button
                                    key={index}
                                    className={`w-3 h-3 rounded-full ${index === activeSolutionSlide ? 'bg-blue-700' : 'bg-gray-300'}`}
                                    onClick={() => {
                                        setActiveSolutionSlide(index);
                                        setAutoSolutionSlide(false);
                                        setTimeout(() => setAutoSolutionSlide(true), 6000);
                                    }}
                                />
                            ))}
                        </div>
                    </div>
                </section>

                {/* Technology Stack with Hexagon Pattern */}
                <section className="relative py-20 bg-gradient-to-r from-blue-50 to-purple-50 dark:from-gray-900 dark:to-blue-950 overflow-hidden">
                    <div className="absolute inset-0 opacity-10 dark:opacity-[0.03]">
                        <div className="absolute inset-0 bg-[url('https://bgvault.tech/hexagon.svg')] bg-[length:150px_150px]"></div>
                    </div>
                    <div className="relative max-w-6xl mx-auto px-4 sm:px-8">
                        <h3 className="text-3xl font-bold text-center text-blue-900 dark:text-blue-300 mb-12">
                            Technology Ecosystem
                        </h3>
                        <div
                            className="relative overflow-hidden"
                            onMouseEnter={() => setAutoTechSlide(false)}
                            onMouseLeave={() => setAutoTechSlide(true)}
                        >
                            <div className="flex transition-transform duration-500 ease-in-out"
                                style={{ transform: `translateX(-${activeTechSlide * 100}%)` }}
                            >
                                {techSlides.map((slide, index) => (
                                    <div
                                        key={index}
                                        className="min-w-full grid grid-cols-2 md:grid-cols-5 gap-8 px-4"
                                    >
                                        {slide.map((tech, i) => (
                                            <div key={i} className="p-6 bg-white/90 dark:bg-gray-900/90 backdrop-blur-sm rounded-xl text-center hover:shadow-lg transition border border-gray-200 dark:border-gray-800">
                                                <div className="text-4xl text-blue-600 dark:text-blue-400 mb-4">
                                                    {tech.icon}
                                                </div>
                                                <div className="text-gray-700 dark:text-gray-300 font-medium">
                                                    {tech.name}
                                                </div>
                                            </div>
                                        ))}
                                    </div>
                                ))}
                            </div>
                        </div>
                        <div className="flex justify-center mt-8 gap-2">
                            {techSlides.map((_, index) => (
                                <button
                                    key={index}
                                    className={`w-3 h-3 rounded-full ${index === activeTechSlide ? 'bg-blue-600' : 'bg-gray-300'}`}
                                    onClick={() => {
                                        setActiveTechSlide(index);
                                        setAutoTechSlide(false);
                                        setTimeout(() => setAutoTechSlide(true), 5000);
                                    }}
                                />
                            ))}
                        </div>
                    </div>
                </section>

                {/* Testimonials with Bubble Pattern */}
                <section className="relative py-24 bg-white dark:bg-gray-950 overflow-hidden">
                    <div className="absolute inset-0 opacity-10 dark:opacity-[0.03]">
                        <div className="absolute inset-0 bg-[url('https://bgvault.tech/bubble.svg')] bg-[length:180px_180px]"></div>
                    </div>
                    <div className="relative max-w-6xl mx-auto px-4 sm:px-8">
                        <h3 className="text-3xl font-bold text-center text-blue-900 dark:text-blue-400 mb-12">
                            Client Success Stories
                        </h3>
                        <div className="grid grid-cols-1 md:grid-cols-2 gap-8">
                            {testimonials.map((testimonial, index) => (
                                <div key={index} className="p-8 bg-white/90 dark:bg-gray-900/90 backdrop-blur-sm rounded-2xl border border-gray-200 dark:border-gray-800">
                                    <div className="flex gap-2 text-yellow-400 mb-4">
                                        {[...Array(5)].map((_, i) => <FaRegStar key={i} />)}
                                    </div>
                                    <p className="text-lg text-gray-700 dark:text-gray-300 mb-6">
                                        &quot;{testimonial.text}&quot;
                                    </p>
                                    <div className="flex items-center gap-4">
                                        <FaRegComments className="text-3xl text-blue-600 dark:text-blue-400" />
                                        <div>
                                            <div className="font-semibold text-blue-900 dark:text-blue-300">
                                                {testimonial.author}
                                            </div>
                                            <div className="text-sm text-gray-600 dark:text-gray-400">
                                                {testimonial.role}
                                            </div>
                                        </div>
                                    </div>
                                </div>
                            ))}
                        </div>
                    </div>
                </section>

                {/* Trusted By with Diamond Pattern */}
                <section className="relative py-20 bg-gradient-to-r from-blue-50 to-purple-50 dark:from-gray-900 dark:to-blue-950 overflow-hidden">
                    <div className="absolute inset-0 opacity-10 dark:opacity-[0.03]">
                        <div className="absolute inset-0 bg-[url('https://bgvault.tech/diamond.svg')] bg-[length:100px_100px]"></div>
                    </div>
                    <div className="relative max-w-6xl mx-auto px-4 sm:px-8">
                        <h3 className="text-2xl font-semibold text-center text-blue-800 dark:text-blue-200 mb-12">
                            Trusted by Industry Leaders
                        </h3>
                        <div className="grid grid-cols-2 md:grid-cols-5 gap-8 items-center justify-center">
                            {[FaGoogle, FaAmazon, SiNetflix, SiSpotify, SiTesla].map((Icon, i) => (
                                <div key={i} className="p-4 grayscale hover:grayscale-0 transition-all duration-300">
                                    <Icon className="w-24 h-24 mx-auto text-blue-600 dark:text-blue-400" />
                                </div>
                            ))}
                        </div>
                    </div>
                </section>

                {/* Final CTA with Floating Dots Pattern */}
                <section className="relative py-24 px-4 sm:px-8 bg-gradient-to-br from-blue-100 to-blue-200 dark:from-blue-900 dark:to-blue-800 overflow-hidden">
                    <div className="absolute inset-0 opacity-10 dark:opacity-[0.03]">
                        <div className="absolute inset-0 bg-[url('https://bgvault.tech/floating-dots.svg')] bg-[length:100px_100px]"></div>
                    </div>
                    <div className="relative max-w-4xl mx-auto text-center">
                        <div className="bg-white/90 dark:bg-gray-900/90 backdrop-blur-sm rounded-2xl p-8 shadow-xl border border-gray-200 dark:border-gray-700">
                            <h4 className="text-3xl md:text-4xl font-bold text-gray-900 dark:text-white mb-4">
                                Ready to Transform Your Business?
                            </h4>
                            <p className="mb-8 text-gray-700 dark:text-gray-300 text-lg">
                                Our experts are ready to help you achieve your digital goals
                            </p>
                            <div className="flex flex-col sm:flex-row justify-center gap-4">
                                <button
                                    onClick={handleWhatsAppClick}
                                    className="flex items-center justify-center gap-2 px-8 py-4 bg-gradient-to-r from-blue-600 to-blue-700 hover:from-blue-700 hover:to-blue-800 text-white font-semibold rounded-lg transition-all transform hover:scale-105"
                                >
                                    <FaWhatsapp className="w-5 h-5" />
                                    Chat on WhatsApp
                                </button>
                                <Link
                                    href="/contact"
                                    className="px-8 py-4 border-2 border-blue-600 text-blue-600 dark:border-blue-400 dark:text-blue-400 hover:bg-blue-50 dark:hover:bg-blue-900/20 rounded-lg font-semibold transition-colors"
                                >
                                    Contact Our Team
                                </Link>
                            </div>
                        </div>
                    </div>
                </section>
            </div>
        </>
    );
};

export default HomePageClientView;