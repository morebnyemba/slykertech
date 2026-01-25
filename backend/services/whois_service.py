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

from ..config.whois_config import whois_config

logger = logging.getLogger(__name__)


class WhoisService:
    """Service for querying WHOIS servers"""
    
    def __init__(self, timeout: int = 10, max_retries: int = 2):
        """
        Initialize WHOIS service
        
        Args:
            timeout: Query timeout in seconds (default: 10)
            max_retries: Maximum number of retry attempts (default: 2)
        """
        self.timeout = timeout
        self.max_retries = max_retries
        self.session = requests.Session()
        # Configure session for connection pooling
        adapter = requests.adapters.HTTPAdapter(
            pool_connections=10,
            pool_maxsize=20,
            max_retries=max_retries
        )
        self.session.mount('http://', adapter)
        self.session.mount('https://', adapter)
    
    def extract_tld(self, domain: str) -> str:
        """
        Extract TLD from domain name
        
        Args:
            domain: Full domain name (e.g., 'example.com')
        
        Returns:
            TLD string (e.g., 'com')
        """
        domain = domain.lower().strip()
        parts = domain.split('.')
        if len(parts) < 2:
            raise ValueError(f"Invalid domain format: {domain}")
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
            # Some WHOIS HTTP APIs use query parameters
            # This is a simple implementation; may need customization per provider
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
        # Case-insensitive search for availability pattern
        return bool(re.search(available_pattern, whois_response, re.IGNORECASE))
    
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
            
            return {
                'domain': domain,
                'available': is_available,
                'tld': tld,
                'whoisServer': server,
                'message': 'Domain is available' if is_available else 'Domain is registered'
            }
            
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
    
    def query_multiple_domains(self, domains: list) -> list:
        """
        Query multiple domains for availability
        
        Note: Currently processes domains sequentially. For better performance
        with large batches, consider implementing parallel processing using
        concurrent.futures.ThreadPoolExecutor or asyncio.
        
        Args:
            domains: List of domain names to query
        
        Returns:
            List of query result dictionaries
        """
        results = []
        for domain in domains:
            result = self.query_domain(domain)
            results.append(result)
        return results
    
    def __del__(self):
        """Clean up resources"""
        if hasattr(self, 'session'):
            self.session.close()


# Export singleton instance
whois_service = WhoisService()
