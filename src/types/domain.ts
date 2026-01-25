/**
 * Type definitions for domain search functionality
 */

/**
 * WHOIS server configuration entry
 */
export interface WhoisServerConfig {
  server: string;
  uri: string;
  available: string;
}

/**
 * Map of TLDs to their WHOIS configurations
 */
export interface WhoisConfigFile {
  whois_servers: Record<string, WhoisServerConfig>;
}

/**
 * Domain search request
 */
export interface DomainSearchRequest {
  domains: string[];
}

/**
 * Single domain search result
 */
export interface DomainSearchResult {
  domain: string;
  available: boolean;
  tld: string;
  whoisServer: string;
  message?: string;
  error?: string;
  cached?: boolean;
}

/**
 * Domain search API response
 */
export interface DomainSearchResponse {
  results: DomainSearchResult[];
}

/**
 * WHOIS service response (from Python backend)
 */
export interface WhoisServiceResponse {
  domain: string;
  available: boolean;
  tld: string;
  whoisServer: string;
  message?: string;
  error?: string;
  cached?: boolean;
}

/**
 * Domain validation result
 */
export interface DomainValidation {
  isValid: boolean;
  error?: string;
}

/**
 * TLD information
 */
export interface TldInfo {
  tld: string;
  description?: string;
  supported: boolean;
}
