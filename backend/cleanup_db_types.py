#!/usr/bin/env python
"""
Database cleanup script to fix PostgreSQL type conflicts.
This script removes stale custom types from pg_type that can cause
duplicate key constraint violations during migrations.
"""
import os
import sys
import psycopg2
from psycopg2 import sql


def cleanup_stale_types():
    """Remove stale PostgreSQL types that can conflict with migrations."""
    
    # Get database connection parameters from environment
    db_name = os.environ.get('DB_NAME', 'slykertech')
    db_user = os.environ.get('DB_USER', 'slykertech')
    db_password = os.environ.get('DB_PASSWORD', '')
    db_host = os.environ.get('DB_HOST', 'db')
    db_port = os.environ.get('DB_PORT', '5432')
    
    # Parse DATABASE_URL if available (takes precedence)
    database_url = os.environ.get('DATABASE_URL')
    if database_url:
        try:
            from urllib.parse import urlparse
            result = urlparse(database_url)
            db_name = result.path[1:]  # Remove leading '/'
            db_user = result.username
            db_password = result.password
            db_host = result.hostname
            db_port = result.port or 5432
        except Exception as e:
            print(f"Warning: Failed to parse DATABASE_URL: {e}")
            print("Falling back to individual DB environment variables")
    
    try:
        # Connect to the database
        conn = psycopg2.connect(
            dbname=db_name,
            user=db_user,
            password=db_password,
            host=db_host,
            port=db_port
        )
        conn.autocommit = True
        cursor = conn.cursor()
        
        print("Checking for stale PostgreSQL types...")
        
        # Check if accounts_user type exists
        cursor.execute("""
            SELECT COUNT(*) FROM pg_type t
            JOIN pg_namespace n ON t.typnamespace = n.oid
            WHERE t.typname = 'accounts_user' AND n.nspname = 'public'
        """)
        
        count = cursor.fetchone()[0]
        
        if count > 0:
            print(f"Found {count} stale 'accounts_user' type(s). Attempting cleanup...")
            
            # Check if the accounts_user table exists
            cursor.execute("""
                SELECT EXISTS (
                    SELECT FROM information_schema.tables 
                    WHERE table_schema = 'public' 
                    AND table_name = 'accounts_user'
                )
            """)
            
            table_exists = cursor.fetchone()[0]
            
            if not table_exists:
                # Safe to drop the type since the table doesn't exist
                print("Table 'accounts_user' does not exist. Dropping stale type...")
                try:
                    cursor.execute("DROP TYPE IF EXISTS accounts_user CASCADE")
                    print("✅ Successfully removed stale 'accounts_user' type")
                except Exception as e:
                    print(f"⚠️  Warning: Could not drop type: {e}")
                    print("This may be normal if the type is in use")
            else:
                print("⚠️  Table 'accounts_user' exists. Skipping type cleanup.")
                print("This is expected if migrations have already been applied.")
        else:
            print("✅ No stale types found. Database is clean.")
        
        cursor.close()
        conn.close()
        
        return True
        
    except psycopg2.OperationalError as e:
        print(f"⚠️  Warning: Could not connect to database: {e}")
        print("Database may not be ready yet. This is normal during startup.")
        return False
    except Exception as e:
        print(f"⚠️  Warning: Error during cleanup: {e}")
        print("Continuing with migrations...")
        return False


if __name__ == '__main__':
    success = cleanup_stale_types()
    sys.exit(0 if success else 1)
