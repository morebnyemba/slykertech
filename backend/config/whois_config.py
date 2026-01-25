"""
WHOIS Configuration Manager
Loads and manages WHOIS server configuration from dist.whois.json
"""

import json
import os
from typing import Dict, Optional
from pathlib import Path


class WhoisConfig:
    """Manages WHOIS server configuration"""
    
    _instance = None
    _config: Dict = {}
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
        # Get the project root directory (one level up from backend/config)
        current_dir = Path(__file__).resolve().parent
        project_root = current_dir.parent.parent
        config_path = project_root / 'dist.whois.json'
        
        if not config_path.exists():
            raise FileNotFoundError(
                f"WHOIS configuration file not found at {config_path}"
            )
        
        try:
            with open(config_path, 'r', encoding='utf-8') as f:
                self._config = json.load(f)
            self._config_loaded = True
        except json.JSONDecodeError as e:
            raise ValueError(f"Invalid JSON in WHOIS configuration: {e}")
        except Exception as e:
            raise Exception(f"Error loading WHOIS configuration: {e}")
    
    def get_whois_server(self, tld: str) -> Optional[Dict[str, str]]:
        """
        Get WHOIS server configuration for a given TLD
        
        Args:
            tld: Top-level domain (e.g., 'com', 'org', 'net')
        
        Returns:
            Dictionary with server, uri, and available pattern, or None if not found
        """
        tld = tld.lower().lstrip('.')
        whois_servers = self._config.get('whois_servers', {})
        return whois_servers.get(tld)
    
    def get_all_tlds(self) -> list:
        """Get list of all supported TLDs"""
        whois_servers = self._config.get('whois_servers', {})
        return sorted(whois_servers.keys())
    
    def validate_config(self) -> bool:
        """
        Validate the loaded configuration
        
        Returns:
            True if configuration is valid, False otherwise
        """
        if not self._config:
            return False
        
        whois_servers = self._config.get('whois_servers')
        if not whois_servers or not isinstance(whois_servers, dict):
            return False
        
        # Validate each TLD entry
        for tld, config in whois_servers.items():
            if not isinstance(config, dict):
                return False
            if 'server' not in config or 'uri' not in config or 'available' not in config:
                return False
            if not config['uri'].startswith(('socket://', 'http://', 'https://')):
                return False
        
        return True
    
    def is_tld_supported(self, tld: str) -> bool:
        """Check if a TLD is supported"""
        tld = tld.lower().lstrip('.')
        whois_servers = self._config.get('whois_servers', {})
        return tld in whois_servers


# Export singleton instance
whois_config = WhoisConfig()
