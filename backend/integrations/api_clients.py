"""
API client wrappers for external service integrations
"""
import requests
from typing import Dict, Optional, Any
from requests.auth import HTTPBasicAuth


class cPanelAPIClient:
    """Client for interacting with cPanel UAPI/API2"""
    
    def __init__(self, host: str, username: str, api_token: str, port: int = 2083):
        """
        Initialize cPanel API client
        
        Args:
            host: cPanel server hostname
            username: cPanel username
            api_token: cPanel API token
            port: cPanel port (default 2083 for SSL)
        """
        self.host = host
        self.username = username
        self.api_token = api_token
        self.port = port
        self.base_url = f"https://{host}:{port}/execute"
    
    def _make_request(self, module: str, function: str, params: Dict = None) -> Dict:
        """Make a request to cPanel UAPI"""
        url = f"{self.base_url}/{module}/{function}"
        
        headers = {
            'Authorization': f'cpanel {self.username}:{self.api_token}'
        }
        
        try:
            response = requests.get(url, headers=headers, params=params or {}, verify=True)
            response.raise_for_status()
            return response.json()
        except requests.exceptions.RequestException as e:
            return {"error": str(e), "status": "failed"}
    
    def get_account_summary(self) -> Dict:
        """Get account summary information"""
        return self._make_request('StatsBar', 'get_stats')
    
    def list_domains(self) -> Dict:
        """List all domains"""
        return self._make_request('DomainInfo', 'list_domains')
    
    def list_email_accounts(self, domain: str = None) -> Dict:
        """List email accounts for a domain"""
        params = {'domain': domain} if domain else {}
        return self._make_request('Email', 'list_pops', params)
    
    def create_email_account(self, email: str, password: str, quota: int = 250) -> Dict:
        """Create a new email account"""
        params = {
            'email': email,
            'password': password,
            'quota': quota
        }
        return self._make_request('Email', 'add_pop', params)
    
    def list_databases(self) -> Dict:
        """List all databases"""
        return self._make_request('Mysql', 'list_databases')
    
    def create_database(self, name: str) -> Dict:
        """Create a new database"""
        return self._make_request('Mysql', 'create_database', {'name': name})
    
    def get_disk_usage(self) -> Dict:
        """Get disk usage information"""
        return self._make_request('Quota', 'get_quota_info')
    
    def list_subdomains(self) -> Dict:
        """List all subdomains"""
        return self._make_request('SubDomain', 'list_subdomains')
    
    def create_account(self, username: str, domain: str, password: str, 
                      plan: str = 'default', quota: int = 1024, bwlimit: int = 10240) -> Dict:
        """
        Create a new cPanel account (requires WHM API access)
        
        Args:
            username: Account username
            domain: Primary domain for the account
            password: Account password
            plan: Hosting plan name
            quota: Disk quota in MB
            bwlimit: Bandwidth limit in MB
        
        Returns:
            Dict with success status and account details or error message
        """
        # This uses WHM API which is different from cPanel UAPI
        whm_url = f"https://{self.host}:2087/json-api/createacct"
        
        headers = {
            'Authorization': f'WHM {self.username}:{self.api_token}'
        }
        
        params = {
            'username': username,
            'domain': domain,
            'password': password,
            'plan': plan,
            'quota': quota,
            'bwlimit': bwlimit,
            'ip': 'n',  # Assign shared IP
            'cgi': 1,
            'hasshell': 0,
            'cpmod': 'paper_lantern'  # cPanel theme
        }
        
        try:
            response = requests.get(whm_url, headers=headers, params=params, verify=True, timeout=30)
            response.raise_for_status()
            data = response.json()
            
            if data.get('metadata', {}).get('result') == 1:
                return {'success': True, 'data': data}
            else:
                return {'success': False, 'error': data.get('metadata', {}).get('reason', 'Unknown error')}
        except requests.exceptions.RequestException as e:
            return {'success': False, 'error': str(e)}


class DirectAdminAPIClient:
    """Client for interacting with DirectAdmin API"""
    
    def __init__(self, host: str, username: str, password: str, port: int = 2222):
        """
        Initialize DirectAdmin API client
        
        Args:
            host: DirectAdmin server hostname
            username: DirectAdmin username
            password: DirectAdmin password
            port: DirectAdmin port (default 2222)
        """
        self.host = host
        self.username = username
        self.password = password
        self.port = port
        self.base_url = f"https://{host}:{port}"
        self.auth = HTTPBasicAuth(username, password)
    
    def _make_request(self, endpoint: str, method: str = 'GET', data: Dict = None) -> Dict:
        """Make a request to DirectAdmin API"""
        url = f"{self.base_url}/{endpoint}"
        
        try:
            if method == 'GET':
                response = requests.get(url, auth=self.auth, params=data or {}, verify=True)
            else:
                response = requests.post(url, auth=self.auth, data=data or {}, verify=True)
            
            response.raise_for_status()
            
            # DirectAdmin returns text responses, parse accordingly
            return {"status": "success", "data": response.text}
        except requests.exceptions.RequestException as e:
            return {"error": str(e), "status": "failed"}
    
    def get_user_info(self) -> Dict:
        """Get user information"""
        return self._make_request('CMD_API_SHOW_USER_CONFIG')
    
    def get_user_usage(self) -> Dict:
        """Get user usage statistics"""
        return self._make_request('CMD_API_SHOW_USER_USAGE')
    
    def list_domains(self) -> Dict:
        """List all domains"""
        return self._make_request('CMD_API_SHOW_DOMAINS')
    
    def list_databases(self) -> Dict:
        """List all databases"""
        return self._make_request('CMD_API_DATABASES')
    
    def create_database(self, name: str, user: str, password: str) -> Dict:
        """Create a new database"""
        data = {
            'action': 'create',
            'name': name,
            'user': user,
            'passwd': password,
            'passwd2': password
        }
        return self._make_request('CMD_API_DATABASES', 'POST', data)
    
    def list_email_accounts(self) -> Dict:
        """List email accounts"""
        return self._make_request('CMD_API_POP')
    
    def create_email_account(self, email: str, password: str, quota: int = 250) -> Dict:
        """Create a new email account"""
        data = {
            'action': 'create',
            'user': email.split('@')[0],
            'passwd': password,
            'passwd2': password,
            'quota': quota
        }
        return self._make_request('CMD_API_POP', 'POST', data)
    
    def create_account(self, username: str, email: str, password: str, 
                      domain: str, quota: int = 1024, bandwidth: int = 10240) -> Dict:
        """
        Create a new DirectAdmin user account (requires admin access)
        
        Args:
            username: Account username
            email: User email address
            password: Account password
            domain: Primary domain for the account
            quota: Disk quota in MB
            bandwidth: Bandwidth limit in MB
        
        Returns:
            Dict with success status and account details or error message
        """
        data = {
            'action': 'create',
            'username': username,
            'email': email,
            'passwd': password,
            'passwd2': password,
            'domain': domain,
            'package': 'default',
            'ip': 'shared',
            'notify': 'no',
            'quota': quota,
            'bandwidth': bandwidth
        }
        
        try:
            result = self._make_request('CMD_API_ACCOUNT_USER', 'POST', data)
            if result.get('status') == 'success':
                return {'success': True, 'data': result}
            else:
                return {'success': False, 'error': result.get('error', 'Unknown error')}
        except Exception as e:
            return {'success': False, 'error': str(e)}


class CloudflareAPIClient:
    """Client for interacting with Cloudflare API"""
    
    def __init__(self, api_token: str, email: str = None):
        """
        Initialize Cloudflare API client
        
        Args:
            api_token: Cloudflare API token
            email: Cloudflare account email (optional for API tokens)
        """
        self.api_token = api_token
        self.email = email
        self.base_url = "https://api.cloudflare.com/client/v4"
        
        self.headers = {
            'Authorization': f'Bearer {api_token}',
            'Content-Type': 'application/json'
        }
    
    def _make_request(self, endpoint: str, method: str = 'GET', data: Dict = None) -> Dict:
        """Make a request to Cloudflare API"""
        url = f"{self.base_url}/{endpoint}"
        
        try:
            if method == 'GET':
                response = requests.get(url, headers=self.headers)
            elif method == 'POST':
                response = requests.post(url, headers=self.headers, json=data)
            elif method == 'PUT':
                response = requests.put(url, headers=self.headers, json=data)
            elif method == 'DELETE':
                response = requests.delete(url, headers=self.headers)
            
            response.raise_for_status()
            return response.json()
        except requests.exceptions.RequestException as e:
            return {"error": str(e), "success": False}
    
    def list_zones(self) -> Dict:
        """List all zones"""
        return self._make_request('zones')
    
    def get_zone(self, zone_id: str) -> Dict:
        """Get zone details"""
        return self._make_request(f'zones/{zone_id}')
    
    def list_dns_records(self, zone_id: str) -> Dict:
        """List DNS records for a zone"""
        return self._make_request(f'zones/{zone_id}/dns_records')
    
    def create_dns_record(self, zone_id: str, record_type: str, name: str, 
                         content: str, ttl: int = 3600, priority: int = None) -> Dict:
        """Create a DNS record"""
        data = {
            'type': record_type,
            'name': name,
            'content': content,
            'ttl': ttl
        }
        if priority:
            data['priority'] = priority
        
        return self._make_request(f'zones/{zone_id}/dns_records', 'POST', data)
    
    def update_dns_record(self, zone_id: str, record_id: str, record_type: str, 
                         name: str, content: str, ttl: int = 3600) -> Dict:
        """Update a DNS record"""
        data = {
            'type': record_type,
            'name': name,
            'content': content,
            'ttl': ttl
        }
        return self._make_request(f'zones/{zone_id}/dns_records/{record_id}', 'PUT', data)
    
    def delete_dns_record(self, zone_id: str, record_id: str) -> Dict:
        """Delete a DNS record"""
        return self._make_request(f'zones/{zone_id}/dns_records/{record_id}', 'DELETE')


class NamecheapAPIClient:
    """Client for interacting with Namecheap API"""
    
    def __init__(self, api_user: str, api_key: str, username: str, client_ip: str):
        """
        Initialize Namecheap API client
        
        Args:
            api_user: Namecheap API username
            api_key: Namecheap API key
            username: Namecheap account username
            client_ip: Whitelisted IP address for API access
        """
        self.api_user = api_user
        self.api_key = api_key
        self.username = username
        self.client_ip = client_ip
        self.base_url = "https://api.namecheap.com/xml.response"
    
    def _make_request(self, command: str, params: Dict = None) -> Dict:
        """Make a request to Namecheap API"""
        request_params = {
            'ApiUser': self.api_user,
            'ApiKey': self.api_key,
            'UserName': self.username,
            'ClientIp': self.client_ip,
            'Command': command,
        }
        
        if params:
            request_params.update(params)
        
        try:
            response = requests.get(self.base_url, params=request_params, timeout=(10, 30))
            response.raise_for_status()
            
            # Parse XML response
            import xml.etree.ElementTree as ET
            root = ET.fromstring(response.text)
            
            # Check for errors
            if root.get('Status') == 'ERROR':
                errors = root.findall('.//{http://api.namecheap.com/xml.response}Error')
                error_messages = [error.text for error in errors]
                return {"success": False, "errors": error_messages}
            
            return {"success": True, "data": root}
        except requests.exceptions.RequestException as e:
            return {"success": False, "error": str(e)}
    
    def check_domain(self, domain: str) -> Dict:
        """Check domain availability"""
        return self._make_request('namecheap.domains.check', {'DomainList': domain})
    
    def get_domain_info(self, domain: str) -> Dict:
        """Get domain information"""
        return self._make_request('namecheap.domains.getInfo', {'DomainName': domain})
    
    def list_domains(self, page: int = 1, page_size: int = 20) -> Dict:
        """List all domains"""
        params = {
            'Page': page,
            'PageSize': page_size,
        }
        return self._make_request('namecheap.domains.getList', params)
    
    def register_domain(self, domain: str, years: int = 1, **kwargs) -> Dict:
        """
        Register a new domain
        
        Args:
            domain: Domain name to register
            years: Number of years to register
            **kwargs: Additional registration parameters (contact info, nameservers, etc.)
        """
        params = {
            'DomainName': domain,
            'Years': years,
        }
        params.update(kwargs)
        return self._make_request('namecheap.domains.create', params)
    
    def renew_domain(self, domain: str, years: int = 1) -> Dict:
        """Renew a domain"""
        params = {
            'DomainName': domain,
            'Years': years,
        }
        return self._make_request('namecheap.domains.renew', params)
    
    def get_dns_hosts(self, domain: str) -> Dict:
        """Get DNS host records for a domain"""
        sld, tld = domain.rsplit('.', 1)
        params = {
            'SLD': sld,
            'TLD': tld,
        }
        return self._make_request('namecheap.domains.dns.getHosts', params)
    
    def set_dns_hosts(self, domain: str, hosts: list) -> Dict:
        """
        Set DNS host records for a domain
        
        Args:
            domain: Domain name
            hosts: List of host records [{'HostName': '@', 'RecordType': 'A', 'Address': '1.2.3.4', 'TTL': 1800}]
        """
        sld, tld = domain.rsplit('.', 1)
        params = {
            'SLD': sld,
            'TLD': tld,
        }
        
        # Add host records to params
        for i, host in enumerate(hosts, start=1):
            params[f'HostName{i}'] = host.get('HostName', '@')
            params[f'RecordType{i}'] = host.get('RecordType', 'A')
            params[f'Address{i}'] = host.get('Address', '')
            params[f'TTL{i}'] = host.get('TTL', 1800)
            if 'MXPref' in host:
                params[f'MXPref{i}'] = host['MXPref']
        
        return self._make_request('namecheap.domains.dns.setHosts', params)
    
    def set_nameservers(self, domain: str, nameservers: list) -> Dict:
        """
        Set nameservers for a domain
        
        Args:
            domain: Domain name
            nameservers: List of nameserver hostnames
        """
        params = {
            'DomainName': domain,
            'Nameservers': ','.join(nameservers),
        }
        return self._make_request('namecheap.domains.dns.setCustom', params)
    
    def get_domain_lock(self, domain: str) -> Dict:
        """Get domain lock status"""
        return self._make_request('namecheap.domains.getRegistrarLock', {'DomainName': domain})
    
    def set_domain_lock(self, domain: str, lock: bool = True) -> Dict:
        """Set domain registrar lock"""
        params = {
            'DomainName': domain,
            'LockAction': 'LOCK' if lock else 'UNLOCK',
        }
        return self._make_request('namecheap.domains.setRegistrarLock', params)
    
    def get_whois_guard(self, domain: str) -> Dict:
        """Get WhoisGuard information"""
        return self._make_request('namecheap.whoisguard.getList', {'DomainName': domain})
    
    def enable_whois_guard(self, domain: str) -> Dict:
        """Enable WhoisGuard for a domain"""
        return self._make_request('namecheap.whoisguard.enable', {'DomainName': domain})
    
    def disable_whois_guard(self, domain: str) -> Dict:
        """Disable WhoisGuard for a domain"""
        return self._make_request('namecheap.whoisguard.disable', {'DomainName': domain})

