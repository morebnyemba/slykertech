#!/usr/bin/env python
"""
Environment Configuration Validation Script

This script checks that all required environment variables are properly configured
before the application starts. It provides helpful error messages if configuration
is missing or incorrect.
"""
import os
import sys
from pathlib import Path


def check_database_config():
    """Check database configuration."""
    database_url = os.environ.get('DATABASE_URL')
    
    if not database_url:
        print("❌ ERROR: DATABASE_URL environment variable is not set!")
        print("\nDatabase configuration is required for the application to run.")
        print("\nFor Docker deployments:")
        print("  1. Ensure your .env file exists in the project root")
        print("  2. Set DB_PASSWORD in your .env file")
        print("  3. Run: docker-compose down && docker volume rm <project>_postgres_data")
        print("  4. Run: docker-compose up -d")
        print("\nOr use the automated fix script:")
        print("  ./fix-db-auth.sh")
        return False
    
    # Extract password from DATABASE_URL
    if 'postgresql://' in database_url or 'postgres://' in database_url:
        try:
            # Parse the URL to check if password is set
            from urllib.parse import urlparse
            parsed = urlparse(database_url)
            
            if not parsed.password:
                print("❌ ERROR: Database password is missing from DATABASE_URL!")
                print("\nPlease ensure DB_PASSWORD is set in your .env file.")
                return False
                
            if parsed.password == 'devpassword':
                print("⚠️  WARNING: Using default database password 'devpassword'")
                print("   For production deployments, please use a strong password!")
        except Exception as e:
            print(f"⚠️  WARNING: Could not validate DATABASE_URL format: {e}")
    
    return True


def check_secret_key():
    """Check Django secret key."""
    secret_key = os.environ.get('SECRET_KEY')
    
    if not secret_key:
        print("⚠️  WARNING: SECRET_KEY not set, using default (insecure!)")
        return True  # Non-fatal, Django has a default
    
    if 'django-insecure' in secret_key:
        print("⚠️  WARNING: Using insecure default SECRET_KEY!")
        print("   For production, generate a new secret key.")
    
    return True


def check_debug_mode():
    """Check debug mode setting."""
    debug = os.environ.get('DEBUG', 'True').lower()
    
    if debug in ('true', '1', 'yes'):
        print("⚠️  WARNING: DEBUG mode is enabled!")
        print("   This should be disabled in production for security.")
    
    return True


def main():
    """Main validation function."""
    print("=" * 60)
    print("Environment Configuration Check")
    print("=" * 60)
    print()
    
    all_checks_passed = True
    
    # Run all checks
    if not check_database_config():
        all_checks_passed = False
    
    if not check_secret_key():
        all_checks_passed = False
    
    if not check_debug_mode():
        all_checks_passed = False
    
    print()
    print("=" * 60)
    
    if all_checks_passed:
        print("✅ All configuration checks passed!")
        print("=" * 60)
        return 0
    else:
        print("❌ Configuration validation failed!")
        print("=" * 60)
        print("\nPlease fix the configuration errors above before starting the application.")
        print("\nFor help, see:")
        print("  - DOCKER_SETUP_README.md")
        print("  - DB_AUTH_ERROR_FIX.md")
        return 1


if __name__ == "__main__":
    sys.exit(main())
