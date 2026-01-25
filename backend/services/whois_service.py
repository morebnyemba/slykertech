"""
WHOIS Service
Implements socket and HTTP-based WHOIS queries for domain availability checking
"""

import socket
import re
import requests
from typing import Dict, Optional, Tuple
from urllib.parse import urlparse
import logging
from concurrent.futures import ThreadPoolExecutor, as_completed
from datetime import datetime, timedelta
from threading import Lock

from config.whois_config import whois_config

logger = logging.getLogger(__name__)


class WhoisService:
    """Service for querying WHOIS servers"""
    
    def __init__(self, timeout: int = 10, max_retries: int = 2, cache_ttl: int = 3600, enable_cache: bool = True):
        """
        Initialize WHOIS service
        
        Args:
            timeout: Query timeout in seconds (default: 10)
            max_retries: Maximum number of retry attempts (default: 2)
            cache_ttl: Cache time-to-live in seconds (default: 3600 = 1 hour)
            enable_cache: Enable result caching (default: True)
        """
        self.timeout = timeout
        self.max_retries = max_retries
        self.cache_ttl = cache_ttl
        self.enable_cache = enable_cache
        
        # Initialize cache
        self._cache: Dict[str, Tuple[Dict, datetime]] = {}
        self._cache_lock = Lock()
        
        # Configure HTTP session for connection pooling
        self.session = requests.Session()
        adapter = requests.adapters.HTTPAdapter(
            pool_connections=10,
            pool_maxsize=20,
            max_retries=max_retries
        )
        self.session.mount('http://', adapter)
        self.session.mount('https://', adapter)
    
    def _get_from_cache(self, domain: str) -> Optional[Dict]:
        """
        Get cached result for a domain
        
        Args:
            domain: Domain name (normalized)
        
        Returns:
            Cached result dictionary or None if not found/expired
        """
        if not self.enable_cache:
            return None
        
        with self._cache_lock:
            if domain in self._cache:
                result, timestamp = self._cache[domain]
                if datetime.now() - timestamp < timedelta(seconds=self.cache_ttl):
                    logger.debug(f"Cache hit for {domain}")
                    return result.copy()
                else:
                    # Remove expired entry
                    del self._cache[domain]
                    logger.debug(f"Cache expired for {domain}")
        return None
    
    def _save_to_cache(self, domain: str, result: Dict) -> None:
        """
        Save result to cache
        
        Args:
            domain: Domain name (normalized)
            result: Result dictionary to cache
        """
        if not self.enable_cache:
            return
        
        with self._cache_lock:
            self._cache[domain] = (result.copy(), datetime.now())
            logger.debug(f"Cached result for {domain}")
    
    def clear_cache(self) -> None:
        """Clear all cached results"""
        with self._cache_lock:
            self._cache.clear()
            logger.info("Cache cleared")
    
    def get_cache_stats(self) -> Dict:
        """
        Get cache statistics
        
        Returns:
            Dictionary with cache size and oldest entry age
        """
        with self._cache_lock:
            cache_size = len(self._cache)
            oldest_age = None
            if cache_size > 0:
                oldest_timestamp = min(ts for _, ts in self._cache.values())
                oldest_age = (datetime.now() - oldest_timestamp).total_seconds()
            return {
                'size': cache_size,
                'oldest_age_seconds': oldest_age,
                'ttl_seconds': self.cache_ttl,
                'enabled': self.enable_cache
            }
    
    def extract_tld(self, domain: str) -> str:
        """
        Extract TLD from domain name, supporting both single-level TLDs (e.g., 'com')
        and second-level TLDs (e.g., 'co.zw', 'co.za', 'co.ke')
        
        Args:
            domain: Full domain name (e.g., 'example.com' or 'example.co.zw')
        
        Returns:
            TLD string (e.g., 'com' or 'co.zw')
        """
        domain = domain.lower().strip()
        parts = domain.split('.')
        if len(parts) < 2:
            raise ValueError(f"Invalid domain format: {domain}")
        
        # Check for second-level TLDs first (e.g., co.zw, co.za, co.ke, org.uk)
        # These require at least 3 parts: name.second.tld
        # For deeply nested subdomains like 'sub.example.co.zw' (4+ parts),
        # this still works correctly since we always check the last two parts
        if len(parts) >= 3:
            second_level_tld = f"{parts[-2]}.{parts[-1]}"
            # Check if this second-level TLD exists in our config
            if whois_config.is_tld_supported(second_level_tld):
                return second_level_tld
        
        # Fall back to single-level TLD
        return parts[-1]
    
    def normalize_domain(self, domain: str) -> str:
        """
        Normalize domain name
        
        Args:
            domain: Domain name to normalize
        
        Returns:
            Normalized domain name
        """
        domain = domain.lower().strip()
        # Remove protocol if present
        domain = re.sub(r'^https?://', '', domain)
        # Remove trailing slash
        domain = domain.rstrip('/')
        # Remove www. prefix
        domain = re.sub(r'^www\.', '', domain)
        return domain
    
    def query_socket_whois(self, domain: str, server: str, port: int = 43) -> str:
        """
        Query WHOIS server using socket connection
        
        Args:
            domain: Domain name to query
            server: WHOIS server hostname
            port: WHOIS server port (default: 43)
        
        Returns:
            WHOIS response string
        
        Raises:
            socket.timeout: If query times out
            socket.error: If socket connection fails
        """
        sock = None
        try:
            # Create socket connection
            sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            sock.settimeout(self.timeout)
            
            # Connect to WHOIS server
            sock.connect((server, port))
            
            # Send query
            query = f"{domain}\r\n"
            sock.send(query.encode('utf-8'))
            
            # Receive response
            response = b""
            while True:
                chunk = sock.recv(4096)
                if not chunk:
                    break
                response += chunk
            
            # Decode response
            try:
                return response.decode('utf-8')
            except UnicodeDecodeError:
                # Fallback to latin-1 for legacy servers
                return response.decode('latin-1')
                
        except socket.timeout:
            logger.error(f"WHOIS query timeout for {domain} on {server}")
            raise
        except socket.error as e:
            logger.error(f"Socket error querying {domain} on {server}: {e}")
            raise
        except Exception as e:
            logger.error(f"Unexpected error querying {domain} on {server}: {e}")
            raise
        finally:
            # Ensure socket is closed
            if sock:
                try:
                    sock.close()
                except:
                    pass
    
    def query_http_whois(self, domain: str, uri: str) -> str:
        """
        Query WHOIS server using HTTP/HTTPS
        
        Args:
            domain: Domain name to query
            uri: HTTP/HTTPS URI for WHOIS query
        
        Returns:
            WHOIS response string
        
        Raises:
            requests.RequestException: If HTTP request fails
        """
        try:
            # Check if URI ends with a query parameter expecting a value (e.g., ?s= or ?search= or &domain=)
            # Pattern matches: ?key= or &key= at the end of the URI
            append_domain_pattern = re.compile(r'[\?&][a-zA-Z_][a-zA-Z0-9_]*=$')
            if append_domain_pattern.search(uri):
                # URI expects domain appended directly (e.g., ?s=example.co.zw)
                full_url = uri + domain
                response = self.session.get(
                    full_url,
                    timeout=self.timeout,
                    headers={'User-Agent': 'SlykerTech-WHOIS/1.0'}
                )
            else:
                # Standard approach: use query parameters
                response = self.session.get(
                    uri,
                    params={'domain': domain},
                    timeout=self.timeout,
                    headers={'User-Agent': 'SlykerTech-WHOIS/1.0'}
                )
            response.raise_for_status()
            return response.text
        except requests.RequestException as e:
            logger.error(f"HTTP WHOIS query failed for {domain} at {uri}: {e}")
            raise
    
    def check_availability(self, whois_response: str, available_pattern: str) -> bool:
        """
        Check if domain is available based on WHOIS response
        
        Args:
            whois_response: Raw WHOIS response text
            available_pattern: Pattern to search for indicating availability
        
        Returns:
            True if domain is available, False otherwise
        """
        # First, try the configured pattern (case-insensitive)
        if re.search(available_pattern, whois_response, re.IGNORECASE):
            return True
        
        # Fallback: Check common "domain available" patterns that WHOIS servers return
        # These patterns indicate the domain is NOT registered (i.e., available)
        common_available_patterns = [
            r'NOT\s*FOUND',
            r'No\s+match',
            r'No\s+data\s+found',
            r'No\s+entries\s+found',
            r'No\s+object\s+found',
            r'Domain\s+not\s+found',
            r'Object\s+not\s+found',
            r'Status:\s*free',
            r'Status:\s*available',
            r'is\s+free',
            r'is\s+available',
            r'AVAILABLE',
            r'No\s+information',
        ]
        
        for pattern in common_available_patterns:
            if re.search(pattern, whois_response, re.IGNORECASE):
                return True
        
        return False
    
    def query_domain(self, domain: str) -> Dict:
        """
        Query domain availability
        
        Args:
            domain: Domain name to query
        
        Returns:
            Dictionary with query results:
            {
                'domain': str,
                'available': bool,
                'tld': str,
                'whoisServer': str,
                'message': str (optional),
                'error': str (optional)
            }
        """
        try:
            # Normalize domain
            domain = self.normalize_domain(domain)
            
            # Check cache first
            cached_result = self._get_from_cache(domain)
            if cached_result is not None:
                cached_result['cached'] = True
                return cached_result
            
            # Extract TLD
            tld = self.extract_tld(domain)
            
            # Get WHOIS server configuration
            config = whois_config.get_whois_server(tld)
            if not config:
                return {
                    'domain': domain,
                    'available': False,
                    'tld': tld,
                    'whoisServer': 'unknown',
                    'error': f'TLD .{tld} is not supported'
                }
            
            server = config['server']
            uri = config['uri']
            available_pattern = config['available']
            
            # Parse URI to determine query method
            parsed_uri = urlparse(uri)
            
            whois_response = None
            for attempt in range(self.max_retries + 1):
                try:
                    if parsed_uri.scheme == 'socket':
                        # Socket-based query
                        port = parsed_uri.port or 43
                        whois_response = self.query_socket_whois(
                            domain, 
                            parsed_uri.hostname or server, 
                            port
                        )
                    elif parsed_uri.scheme in ('http', 'https'):
                        # HTTP-based query
                        whois_response = self.query_http_whois(domain, uri)
                    else:
                        return {
                            'domain': domain,
                            'available': False,
                            'tld': tld,
                            'whoisServer': server,
                            'error': f'Unsupported URI scheme: {parsed_uri.scheme}'
                        }
                    
                    # If we got a response, break out of retry loop
                    if whois_response:
                        break
                        
                except (socket.timeout, socket.error, requests.RequestException) as e:
                    if attempt < self.max_retries:
                        logger.warning(
                            f"Retry {attempt + 1}/{self.max_retries} for {domain}: {e}"
                        )
                        continue
                    else:
                        return {
                            'domain': domain,
                            'available': False,
                            'tld': tld,
                            'whoisServer': server,
                            'error': f'Query failed after {self.max_retries} retries: {str(e)}'
                        }
            
            if not whois_response:
                return {
                    'domain': domain,
                    'available': False,
                    'tld': tld,
                    'whoisServer': server,
                    'error': 'No response received from WHOIS server'
                }
            
            # Check availability
            is_available = self.check_availability(whois_response, available_pattern)
            
            result = {
                'domain': domain,
                'available': is_available,
                'tld': tld,
                'whoisServer': server,
                'message': 'Domain is available' if is_available else 'Domain is registered',
                'cached': False
            }
            
            # Cache the result
            self._save_to_cache(domain, result)
            
            return result
            
        except ValueError as e:
            return {
                'domain': domain,
                'available': False,
                'tld': '',
                'whoisServer': 'unknown',
                'error': str(e)
            }
        except Exception as e:
            logger.error(f"Unexpected error querying domain {domain}: {e}")
            return {
                'domain': domain,
                'available': False,
                'tld': '',
                'whoisServer': 'unknown',
                'error': f'Internal error: {str(e)}'
            }
    
    def query_multiple_domains(self, domains: list, parallel: bool = True, max_workers: int = 5) -> list:
        """
        Query multiple domains for availability
        
        Args:
            domains: List of domain names to query
            parallel: Use parallel processing (default: True)
            max_workers: Maximum number of parallel workers (default: 5)
        
        Returns:
            List of query result dictionaries
        """
        if not parallel or len(domains) == 1:
            # Sequential processing for single domain or when parallel is disabled
            results = []
            for domain in domains:
                result = self.query_domain(domain)
                results.append(result)
            return results
        
        # Parallel processing with ThreadPoolExecutor
        results = []
        with ThreadPoolExecutor(max_workers=min(max_workers, len(domains))) as executor:
            # Submit all domain queries
            future_to_domain = {
                executor.submit(self.query_domain, domain): domain 
                for domain in domains
            }
            
            # Collect results as they complete
            for future in as_completed(future_to_domain):
                domain = future_to_domain[future]
                try:
                    result = future.result()
                    results.append(result)
                except Exception as e:
                    logger.error(f"Error querying domain {domain} in parallel: {e}")
                    results.append({
                        'domain': domain,
                        'available': False,
                        'tld': '',
                        'whoisServer': 'unknown',
                        'error': f'Query failed: {str(e)}'
                    })
        
        # Sort results to match input order
        domain_to_result = {r['domain']: r for r in results}
        ordered_results = []
        for domain in domains:
            normalized = self.normalize_domain(domain)
            if normalized in domain_to_result:
                ordered_results.append(domain_to_result[normalized])
            else:
                # Fallback for domains that weren't processed
                ordered_results.append({
                    'domain': domain,
                    'available': False,
                    'tld': '',
                    'whoisServer': 'unknown',
                    'error': 'Domain not processed'
                })
        
        return ordered_results
    
    def __del__(self):
        """Clean up resources"""
        if hasattr(self, 'session'):
            self.session.close()


# Export singleton instance
whois_service = WhoisService()
