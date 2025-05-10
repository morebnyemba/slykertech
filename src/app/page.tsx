'use client';

import { useState, useEffect } from 'react';
import Link from 'next/link';
import Head from 'next/head';
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

export default function HomePage() {
  const [activeFeatureSlide, setActiveFeatureSlide] = useState(0);
  const [activeSolutionSlide, setActiveSolutionSlide] = useState(0);
  const [activeTechSlide, setActiveTechSlide] = useState(0);
  const [autoFeatureSlide, setAutoFeatureSlide] = useState(true);
  const [autoSolutionSlide, setAutoSolutionSlide] = useState(true);
  const [autoTechSlide, setAutoTechSlide] = useState(true);

  const handleWhatsAppClick = () => {
    const message = encodeURIComponent("Hi Slyker Tech! I'm interested in your services.");
    window.open(`https://wa.me/263787211325?text=${message}`, '_blank');
  };

  // Stats data
  const stats = [
    { value: "99.99%", label: "Uptime SLA", icon: <FaServer className="w-8 h-8" /> },
    { value: "5K+", label: "Active Clients", icon: <FaUserShield className="w-8 h-8" /> },
    { value: "150+", label: "Expert Engineers", icon: <FaCode className="w-8 h-8" /> },
    { value: "12+", label: "Years Experience", icon: <FaRegStar className="w-8 h-8" /> }
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
    <>
      <Head>
        <title>Slyker Tech | Enterprise Digital Transformation Services</title>
        <meta name="description" content="Slyker Tech provides cutting-edge digital transformation services including cloud solutions, AI integration, and custom software development for enterprises." />
        <meta name="keywords" content="digital transformation, cloud services, AI integration, custom software development, enterprise solutions" />
        <meta property="og:title" content="Slyker Tech | Enterprise Digital Transformation Services" />
        <meta property="og:description" content="Cutting-edge digital transformation services for modern enterprises" />
        <meta property="og:type" content="website" />
        <meta property="og:url" content="https://slykertech.co.zw" />
        <link rel="canonical" href="https://slykertech.co.zw" />
      </Head>

      <div className="relative z-10">
        {/* Hero Section with Grid Pattern */}
        <section className="relative py-28 px-4 sm:px-8 md:px-16 lg:px-24 text-center bg-gradient-to-b from-blue-50 to-white dark:from-blue-950/80 dark:to-gray-900 overflow-hidden">
          <div className="absolute inset-0 opacity-10 dark:opacity-[0.03]">
            <div className="absolute inset-0 bg-[url('https://bgvault.tech/grid.svg')] bg-[length:100px_100px]"></div>
          </div>
          <div className="relative max-w-5xl mx-auto">
            <h1 className="text-5xl sm:text-6xl md:text-7xl font-extrabold tracking-tight text-blue-900 dark:text-white leading-tight">
              Next-Gen <span className="text-blue-600 dark:text-blue-400">Tech Solutions</span>
            </h1>
            <p className="mt-8 text-lg sm:text-xl text-gray-800 dark:text-gray-300 max-w-3xl mx-auto">
              Empowering enterprises with full-stack digital transformation services across cloud, AI, and modern application development.
            </p>
            <div className="mt-12 flex flex-col sm:flex-row justify-center gap-4 sm:gap-6">
              <button
                onClick={handleWhatsAppClick}
                className="flex items-center justify-center gap-2 px-8 py-4 rounded-lg bg-gradient-to-r from-blue-600 to-blue-700 hover:from-blue-700 hover:to-blue-800 text-white font-semibold transition-all transform hover:-translate-y-0.5 shadow-lg hover:shadow-xl"
              >
                <FaWhatsapp className="w-5 h-5" />
                Get Started Now
              </button>
              <Link
                href="/services"
                className="px-8 py-4 rounded-lg border-2 border-blue-600 hover:border-blue-700 text-blue-600 hover:text-blue-700 dark:border-blue-400 dark:text-blue-400 dark:hover:bg-blue-900/30 font-semibold transition-colors"
              >
                Explore Our Services
              </Link>
            </div>
          </div>
        </section>

        {/* Stats Section with Connected Dots Pattern */}
        <section className="relative py-20 bg-white dark:bg-gray-950 overflow-hidden">
          <div className="absolute inset-0 opacity-5 dark:opacity-[0.02]">
            <div className="absolute inset-0 bg-[url('https://bgvault.tech/connected-dots.svg')] bg-[length:120px_120px]"></div>
          </div>
          <div className="relative max-w-6xl mx-auto grid grid-cols-1 md:grid-cols-4 gap-8 px-4 sm:px-8">
            {stats.map((stat, index) => (
              <div key={index} className="p-6 text-center bg-white/90 dark:bg-gray-900/90 backdrop-blur-sm rounded-xl shadow-sm hover:shadow-md transition-shadow border border-gray-200 dark:border-gray-800">
                <div className="text-3xl text-blue-600 dark:text-blue-400 mb-4">
                  {stat.icon}
                </div>
                <div className="text-4xl font-bold text-gray-900 dark:text-white mb-2">
                  {stat.value}
                </div>
                <div className="text-gray-600 dark:text-gray-300 uppercase text-sm tracking-wide font-medium">
                  {stat.label}
                </div>
              </div>
            ))}
          </div>
        </section>

        {/* Features Carousel with Circuit Pattern */}
        <section className="relative py-24 px-4 sm:px-8 bg-gray-50 dark:bg-gray-950 overflow-hidden">
          <div className="absolute inset-0 opacity-10 dark:opacity-[0.03]">
            <div className="absolute inset-0 bg-[url('https://bgvault.tech/circuit.svg')] bg-[length:150px_150px]"></div>
          </div>
          <div className="relative max-w-6xl mx-auto">
            <h2 className="text-4xl font-bold text-center text-blue-900 dark:text-white mb-12">
              Core Features
            </h2>
            <div 
              className="relative overflow-hidden"
              onMouseEnter={() => setAutoFeatureSlide(false)}
              onMouseLeave={() => setAutoFeatureSlide(true)}
            >
              <div className="flex transition-transform duration-500 ease-in-out"
                style={{ transform: `translateX(-${activeFeatureSlide * 100}%)` }}
              >
                {featureSlides.map((slide, index) => (
                  <div 
                    key={index}
                    className="min-w-full grid grid-cols-1 md:grid-cols-3 gap-8 px-4"
                  >
                    {slide.map((feature, i) => (
                      <div key={i} className="group p-8 border border-gray-200 dark:border-gray-700 rounded-2xl bg-white/90 dark:bg-gray-900/90 backdrop-blur-sm hover:border-blue-600 dark:hover:border-blue-400 transition-all duration-300 hover:-translate-y-2 hover:shadow-lg">
                        <div className="text-blue-600 dark:text-blue-400 mb-6">
                          {feature.icon}
                        </div>
                        <h3 className="text-2xl font-semibold mb-4 text-gray-900 dark:text-white">{feature.title}</h3>
                        <p className="text-gray-600 dark:text-gray-400 leading-relaxed">{feature.description}</p>
                      </div>
                    ))}
                  </div>
                ))}
              </div>
            </div>
            <div className="flex justify-center mt-8 gap-2">
              {featureSlides.map((_, index) => (
                <button
                  key={index}
                  className={`w-3 h-3 rounded-full ${index === activeFeatureSlide ? 'bg-blue-600' : 'bg-gray-300'}`}
                  onClick={() => {
                    setActiveFeatureSlide(index);
                    setAutoFeatureSlide(false);
                    setTimeout(() => setAutoFeatureSlide(true), 5000);
                  }}
                />
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
}