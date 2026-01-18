// app/services/servicesData.tsx
import { JSX } from 'react';
import { 
  FaCode, FaCloud, 
  
  FaChartLine, FaMobileAlt,
  FaDatabase, FaShieldAlt, FaShoppingCart,
  FaPalette, FaVideo, FaHeadset
} from 'react-icons/fa';
import { 
  
  MdDashboard} from 'react-icons/md';
import { SiGoogleads } from 'react-icons/si';

export interface Service {
  title: string;
  icon: JSX.Element;
  description: string;
  features: string[];
  keywords: string;
  comingSoon?: boolean;
  href?: string; // Link to detailed page
}

const servicesData: Service[] = [
  {
    title: 'Custom Software Development',
    icon: <FaCode className="w-12 h-12" />,
    description: 'Bespoke software solutions tailored to your business requirements',
    features: [
      'Web application development',
      'Desktop software solutions',
      'Legacy system modernization',
      'API development',
      'Microservices architecture'
    ],
    keywords: 'Custom Software, Enterprise Solutions, Business Applications',
    href: '/services/development'
  },
  {
    title: 'Mobile App Development',
    icon: <FaMobileAlt className="w-12 h-12" />,
    description: 'Cross-platform mobile applications for iOS and Android',
    features: [
      'React Native development',
      'Flutter applications',
      'Native iOS/Android apps',
      'Mobile UI/UX design',
      'App store optimization'
    ],
    keywords: 'Mobile Apps, iOS, Android, Hybrid Apps',
    href: '/services/development'
  },
  {
    title: 'E-commerce Solutions',
    icon: <FaShoppingCart className="w-12 h-12" />,
    description: 'Complete online store development and integration',
    features: [
      'Shopping cart systems',
      'Payment gateway integration',
      'Product catalog management',
      'Order processing systems',
      'Multi-vendor platforms'
    ],
    keywords: 'E-commerce, Online Store, WooCommerce, Shopify',
    href: '/services/development'
  },
  {
    title: 'Web Design & Development',
    icon: <FaPalette className="w-12 h-12" />,
    description: 'Beautiful, functional websites that convert visitors',
    features: [
      'Responsive web design',
      'CMS development (WordPress, etc.)',
      'Landing page creation',
      'UI/UX optimization',
      'Website maintenance'
    ],
    keywords: 'Web Design, Frontend Development, WordPress',
    href: '/services/design'
  },
  {
    title: 'Cloud Services',
    icon: <FaCloud className="w-12 h-12" />,
    description: 'Scalable cloud infrastructure solutions',
    features: [
      'AWS/Azure/GCP setup',
      'Cloud migration',
      'Serverless architecture',
      'Container orchestration',
      'Cloud security'
    ],
    keywords: 'Cloud Computing, AWS, Azure, Google Cloud',
    href: '/services/hosting'
  },
  {
    title: 'Database Solutions',
    icon: <FaDatabase className="w-12 h-12" />,
    description: 'Optimized database architecture and management',
    features: [
      'Database design',
      'SQL/NoSQL solutions',
      'Data migration',
      'Performance tuning',
      'Backup solutions'
    ],
    keywords: 'Database, MySQL, MongoDB, PostgreSQL',
    href: '/services/hosting'
  },
  {
    title: 'Cyber Security',
    icon: <FaShieldAlt className="w-12 h-12" />,
    description: 'Comprehensive protection for your digital assets',
    features: [
      'Vulnerability assessments',
      'Penetration testing',
      'Security audits',
      'Compliance consulting',
      'Incident response'
    ],
    keywords: 'Cyber Security, Penetration Testing, Compliance'
  },
  {
    title: 'Digital Marketing',
    icon: <SiGoogleads className="w-12 h-12" />,
    description: 'Data-driven marketing strategies for growth',
    features: [
      'SEO services',
      'PPC advertising',
      'Social media marketing',
      'Content marketing',
      'Analytics & reporting'
    ],
    keywords: 'Digital Marketing, SEO, PPC, Social Media'
  },
  {
    title: 'Business Intelligence',
    icon: <FaChartLine className="w-12 h-12" />,
    description: 'Turn your data into actionable insights',
    features: [
      'Data visualization',
      'Dashboard development',
      'Predictive analytics',
      'KPI tracking',
      'Data warehousing'
    ],
    keywords: 'BI, Analytics, Data Visualization, Power BI'
  },
  {
    title: 'IT Support & Maintenance',
    icon: <FaHeadset className="w-12 h-12" />,
    description: 'Reliable technical support for your business',
    features: [
      '24/7 monitoring',
      'Help desk services',
      'System maintenance',
      'Backup management',
      'Disaster recovery'
    ],
    keywords: 'IT Support, Managed Services, Help Desk'
  },
  {
    title: 'Video Production',
    icon: <FaVideo className="w-12 h-12" />,
    description: 'Professional video content for your brand',
    features: [
      'Promotional videos',
      'Product demos',
      'Animation services',
      'Video editing',
      'Live streaming'
    ],
    keywords: 'Video Production, Animation, Editing'
  },
  {
    title: 'Client Portal',
    icon: <MdDashboard className="w-12 h-12" />,
    description: 'Dedicated platform for client collaboration',
    features: [
      'Project tracking',
      'Secure document sharing',
      'Real-time messaging',
      'Billing integration',
      'Support ticketing'
    ],
    keywords: 'Client Portal, Project Management',
    comingSoon: true
  }
];

export default servicesData;