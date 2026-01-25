"""
WHOIS Configuration Manager
Loads and manages WHOIS server configuration from dist.whois.json

Supports the extensions-based format:
[
    {
        "extensions": ".com,.net,.org",
        "uri": "socket://whois.example.com",
        "available": "No match for"
    }
]
"""

import json
import os
from typing import Dict, List, Optional
from pathlib import Path


class WhoisConfig:
    """Manages WHOIS server configuration"""
    
    _instance = None
    _config: List = []
    _tld_map: Dict[str, Dict] = {}
    _config_loaded = False
    
    def __new__(cls):
        if cls._instance is None:
            cls._instance = super(WhoisConfig, cls).__new__(cls)
        return cls._instance
    
    def __init__(self):
        if not self._config_loaded:
            self.load_config()
    
    def load_config(self) -> None:
        """Load WHOIS configuration from dist.whois.json"""
        # Get the backend root directory (one level up from config/)
        # In Docker, this is /app. Locally, this is backend/
        current_dir = Path(__file__).resolve().parent
        backend_root = current_dir.parent
        config_path = backend_root / 'dist.whois.json'
        
        if not config_path.exists():
            raise FileNotFoundError(
                f"WHOIS configuration file not found at {config_path}"
            )
        
        try:
            with open(config_path, 'r', encoding='utf-8') as f:
                self._config = json.load(f)
            self._build_tld_map()
            self._config_loaded = True
        except json.JSONDecodeError as e:
            raise ValueError(f"Invalid JSON in WHOIS configuration: {e}")
        except Exception as e:
            raise Exception(f"Error loading WHOIS configuration: {e}")
    
    def _build_tld_map(self) -> None:
        """Build a lookup map from TLD to server config"""
        from urllib.parse import urlparse
        
        self._tld_map = {}
        for entry in self._config:
            if not isinstance(entry, dict):
                continue
            extensions = entry.get('extensions', '')
            uri = entry.get('uri', '')
            available = entry.get('available', '')
            
            # Parse the server hostname from the URI
            parsed = urlparse(uri)
            server = parsed.hostname or parsed.netloc.split(':')[0]
            
            # Split extensions and map each TLD
            for ext in extensions.split(','):
                tld = ext.strip().lstrip('.')
                if tld:
                    self._tld_map[tld.lower()] = {
                        'server': server,
                        'uri': uri,
                        'available': available
                    }
    
    def get_whois_server(self, tld: str) -> Optional[Dict[str, str]]:
        """
        Get WHOIS server configuration for a given TLD
        
        Args:
            tld: Top-level domain (e.g., 'com', 'org', 'net')
        
        Returns:
            Dictionary with server, uri, and available pattern, or None if not found
        """
        tld = tld.lower().lstrip('.')
        return self._tld_map.get(tld)
    
    def get_all_tlds(self) -> list:
        """Get list of all supported TLDs"""
        return sorted(self._tld_map.keys())
    
    def validate_config(self) -> bool:
        """
        Validate the loaded configuration
        
        Returns:
            True if configuration is valid, False otherwise
        """
        if not self._config:
            return False
        
        if not isinstance(self._config, list):
            return False
        
        # Validate each entry
        for entry in self._config:
            if not isinstance(entry, dict):
                return False
            if 'extensions' not in entry or 'uri' not in entry or 'available' not in entry:
                return False
            if not entry['uri'].startswith(('socket://', 'http://', 'https://')):
                return False
        
        return True
    
    def is_tld_supported(self, tld: str) -> bool:
        """Check if a TLD is supported"""
        tld = tld.lower().lstrip('.')
        return tld in self._tld_map


# Export singleton instance
whois_config = WhoisConfig()
