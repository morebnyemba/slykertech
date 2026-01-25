/**
 * Domain utility functions
 * Helpers for domain validation, parsing, and normalization
 */

import type { DomainValidation } from '@/types/domain';

/**
 * Validate domain name format
 * @param domain - Domain name to validate
 * @returns Validation result with error message if invalid
 */
export function validateDomain(domain: string): DomainValidation {
  // Remove leading/trailing whitespace
  const trimmed = domain.trim();

  // Check if empty
  if (!trimmed) {
    return {
      isValid: false,
      error: 'Domain name cannot be empty',
    };
  }

  // Basic domain regex pattern
  // Allows: letters, numbers, hyphens, and dots
  // Must have at least one dot (TLD separator)
  const domainRegex = /^(?:[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?\.)+[a-zA-Z]{2,}$/;

  if (!domainRegex.test(trimmed)) {
    return {
      isValid: false,
      error: 'Invalid domain format',
    };
  }

  // Check length constraints
  if (trimmed.length > 253) {
    return {
      isValid: false,
      error: 'Domain name too long (max 253 characters)',
    };
  }

  // Check individual label lengths
  const labels = trimmed.split('.');
  for (const label of labels) {
    if (label.length > 63) {
      return {
        isValid: false,
        error: 'Domain label too long (max 63 characters per label)',
      };
    }
    if (label.startsWith('-') || label.endsWith('-')) {
      return {
        isValid: false,
        error: 'Domain labels cannot start or end with hyphens',
      };
    }
  }

  return { isValid: true };
}

/**
 * Extract TLD from domain name
 * @param domain - Full domain name
 * @returns TLD string (e.g., 'com' from 'example.com')
 */
export function extractTld(domain: string): string {
  const normalized = normalizeDomain(domain);
  const parts = normalized.split('.');
  
  if (parts.length < 2) {
    throw new Error('Invalid domain format');
  }
  
  return parts[parts.length - 1];
}

/**
 * Normalize domain name
 * Removes protocol, www prefix, trailing slashes, and converts to lowercase
 * @param domain - Domain name to normalize
 * @returns Normalized domain name
 */
export function normalizeDomain(domain: string): string {
  let normalized = domain.toLowerCase().trim();
  
  // Remove protocol
  normalized = normalized.replace(/^https?:\/\//, '');
  
  // Remove www. prefix
  normalized = normalized.replace(/^www\./, '');
  
  // Remove trailing slash
  normalized = normalized.replace(/\/$/, '');
  
  // Remove path and query parameters
  normalized = normalized.split('/')[0];
  normalized = normalized.split('?')[0];
  
  return normalized;
}

/**
 * Parse multiple domain names from text
 * Supports comma, space, and newline separators
 * @param text - Text containing domain names
 * @returns Array of normalized domain names
 */
export function parseMultipleDomains(text: string): string[] {
  // Split by comma, semicolon, space, or newline
  const domains = text
    .split(/[,;\s\n]+/)
    .map(d => normalizeDomain(d))
    .filter(d => d.length > 0);
  
  // Remove duplicates
  return Array.from(new Set(domains));
}

/**
 * Format domain for display
 * @param domain - Domain name
 * @returns Formatted domain name
 */
export function formatDomain(domain: string): string {
  return normalizeDomain(domain);
}

/**
 * Get domain without TLD (second-level domain)
 * @param domain - Full domain name
 * @returns Domain without TLD (e.g., 'example' from 'example.com')
 */
export function getDomainWithoutTld(domain: string): string {
  const normalized = normalizeDomain(domain);
  const parts = normalized.split('.');
  
  if (parts.length < 2) {
    return normalized;
  }
  
  // Return all parts except the last one (TLD)
  return parts.slice(0, -1).join('.');
}

/**
 * Check if domain has subdomain
 * @param domain - Domain name to check
 * @returns True if domain has subdomain, false otherwise
 */
export function hasSubdomain(domain: string): boolean {
  const normalized = normalizeDomain(domain);
  const parts = normalized.split('.');
  
  // A domain with subdomain has at least 3 parts (e.g., sub.example.com)
  return parts.length > 2;
}

/**
 * Validate multiple domains
 * @param domains - Array of domain names
 * @returns Object with valid and invalid domains
 */
export function validateMultipleDomains(domains: string[]): {
  valid: string[];
  invalid: { domain: string; error: string }[];
} {
  const valid: string[] = [];
  const invalid: { domain: string; error: string }[] = [];

  for (const domain of domains) {
    const validation = validateDomain(domain);
    if (validation.isValid) {
      valid.push(normalizeDomain(domain));
    } else {
      invalid.push({
        domain,
        error: validation.error || 'Invalid domain',
      });
    }
  }

  return { valid, invalid };
}

/**
 * Generate domain variations
 * @param baseName - Base domain name without TLD
 * @param tlds - Array of TLDs to generate variations for
 * @returns Array of domain variations
 */
export function generateDomainVariations(baseName: string, tlds: string[]): string[] {
  const normalized = baseName.toLowerCase().trim();
  return tlds.map(tld => `${normalized}.${tld.replace(/^\./, '')}`);
}
