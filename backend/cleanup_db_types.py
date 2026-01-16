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
from urllib.parse import urlparse


def cleanup_stale_types():
    """Remove stale PostgreSQL types that can conflict with migrations."""
    
    # List of type names to check and clean up
    # Add more type names here if needed for other models
    TYPE_NAMES_TO_CLEANUP = [
        'accounts_user',
        # Add other custom type names here if needed
    ]
    
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
            result = urlparse(database_url)
            
            # Validate that we have all required components
            url_valid = True
            missing_components = []
            
            if not result.path or len(result.path) <= 1:
                missing_components.append('database name')
                url_valid = False
            else:
                db_name = result.path[1:]  # Remove leading '/'
            
            if not result.username:
                missing_components.append('username')
                url_valid = False
            else:
                db_user = result.username
            
            if not result.password:
                missing_components.append('password')
                url_valid = False
            else:
                db_password = result.password
            
            if not result.hostname:
                missing_components.append('hostname')
                url_valid = False
            else:
                db_host = result.hostname
            
            if result.port:
                db_port = result.port
            
            if not url_valid:
                print(f"Warning: DATABASE_URL missing: {', '.join(missing_components)}")
                print("Falling back to individual DB environment variables")
                
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
        
        cleanup_performed = False
        
        # Check and clean each type name
        for type_name in TYPE_NAMES_TO_CLEANUP:
            # Check if type exists
            cursor.execute("""
                SELECT COUNT(*) FROM pg_type t
                JOIN pg_namespace n ON t.typnamespace = n.oid
                WHERE t.typname = %s AND n.nspname = 'public'
            """, (type_name,))
            
            count = cursor.fetchone()[0]
            
            if count > 0:
                print(f"Found {count} stale '{type_name}' type(s). Attempting cleanup...")
                
                # Check if the corresponding table exists
                cursor.execute("""
                    SELECT EXISTS (
                        SELECT FROM information_schema.tables 
                        WHERE table_schema = 'public' 
                        AND table_name = %s
                    )
                """, (type_name,))
                
                table_exists = cursor.fetchone()[0]
                
                if not table_exists:
                    # Safe to drop the type since the table doesn't exist
                    print(f"Table '{type_name}' does not exist. Dropping stale type...")
                    try:
                        cursor.execute(sql.SQL("DROP TYPE IF EXISTS {} CASCADE").format(
                            sql.Identifier(type_name)
                        ))
                        print(f"✅ Successfully removed stale '{type_name}' type")
                        cleanup_performed = True
                    except Exception as e:
                        print(f"⚠️  Warning: Could not drop type '{type_name}': {e}")
                        print("This may be normal if the type is in use")
                else:
                    print(f"⚠️  Table '{type_name}' exists. Skipping type cleanup.")
                    print("This is expected if migrations have already been applied.")
        
        if not cleanup_performed:
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
