/** @type {import('next-sitemap').IConfig} */
module.exports = {
  siteUrl: process.env.NEXT_PUBLIC_WEBSITE_URL || 'https://slykertech.co.zw',
  generateRobotsTxt: true,
  robotsTxtOptions: {
    policies: [
      {
        userAgent: '*',
        allow: '/',
        disallow: ['/private/*', '/admin/*'],
      },
    ],
  },
  changefreq: 'daily',
  priority: 0.7,
  exclude: ['/secret-page', '/api/*'],
  transform: async (config, path) => {
    if (path.startsWith('/blog/')) {
      return {
        loc: path,
        changefreq: 'weekly',
        priority: 0.9,
        lastmod: new Date().toISOString(),
      };
    }

    return {
      loc: path,
      changefreq: config.changefreq,
      priority: config.priority,
      lastmod: new Date().toISOString(),
    };
  },
  sitemapSize: 50000,
  // trailingSlash: false,
  // autoLastmod: true,
  // autoPriority: true,
};