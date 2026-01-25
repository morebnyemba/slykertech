#!/usr/bin/env python
"""
Database cleanup script to fix PostgreSQL type conflicts.
This script removes stale custom types from pg_type that can cause
duplicate key constraint violations during migrations.
"""
import os
import sys
import re
import glob
import psycopg2
from psycopg2 import sql
from urllib.parse import urlparse


def get_pending_migration_models(cursor, backend_path):
    """
    Find models that should be created by pending (unapplied) migrations.
    
    This function parses migration files to find CreateModel operations
    and checks if those migrations have been applied. Returns a set of
    table names (in app_model format) that will be created by pending migrations.
    
    Args:
        cursor: Database cursor for querying django_migrations
        backend_path: Path to the backend directory containing apps
        
    Returns:
        set: Table names (e.g., 'reseller_agencypartner') that will be created
             by pending migrations
    """
    pending_tables = set()
    
    try:
        # Check if django_migrations table exists
        cursor.execute("""
            SELECT EXISTS (
                SELECT FROM information_schema.tables 
                WHERE table_schema = 'public' 
                AND table_name = 'django_migrations'
            )
        """)
        if not cursor.fetchone()[0]:
            # No migrations have ever been run
            return pending_tables
        
        # Get all applied migrations
        cursor.execute("SELECT app, name FROM django_migrations")
        applied_migrations = set((row[0], row[1]) for row in cursor.fetchall())
        
        # Find all migration files
        migration_pattern = os.path.join(backend_path, '*', 'migrations', '*.py')
        migration_files = glob.glob(migration_pattern)
        
        for migration_file in migration_files:
            # Skip __init__.py
            if '__init__' in migration_file:
                continue
            
            # Extract app name and migration name
            parts = migration_file.split(os.sep)
            try:
                migrations_idx = parts.index('migrations')
                app_name = parts[migrations_idx - 1]
                migration_name = os.path.basename(migration_file).replace('.py', '')
            except (ValueError, IndexError):
                continue
            
            # Check if this migration has been applied
            if (app_name, migration_name) in applied_migrations:
                continue
            
            # This is a pending migration - parse it for CreateModel operations
            try:
                with open(migration_file, 'r') as f:
                    content = f.read()
                
                # Find all CreateModel operations
                # Pattern handles various formatting: migrations.CreateModel(name='ModelName',
                # or migrations.CreateModel(name="ModelName", with varying whitespace
                create_model_pattern = r"migrations\.CreateModel\s*\(\s*name\s*=\s*['\"](\w+)['\"]"
                models = re.findall(create_model_pattern, content)
                
                for model_name in models:
                    # Convert to table name format: app_modelname (lowercase)
                    table_name = f"{app_name}_{model_name.lower()}"
                    pending_tables.add(table_name)
                    
            except IOError as e:
                # Log file read errors for debugging but continue processing
                print(f"⚠️  Warning: Could not read migration file '{migration_file}': {e}")
                continue
            except Exception as e:
                # Log unexpected errors for debugging but continue processing
                print(f"⚠️  Warning: Error parsing migration file '{migration_file}': {e}")
                continue
                
    except psycopg2.Error as e:
        print(f"⚠️  Warning: Database error while checking migrations: {e}")
    except IOError as e:
        print(f"⚠️  Warning: File system error while checking migrations: {e}")
    except Exception as e:
        print(f"⚠️  Warning: Unexpected error checking pending migrations: {e}")
        
    return pending_tables


def cleanup_stale_types():
    """
    Remove stale PostgreSQL types that can conflict with migrations.
    
    A 'stale type' is a custom PostgreSQL type (typically created by Django for
    custom user models or enum fields) that exists in the pg_type catalog but:
    1. Whose corresponding table does not exist, OR
    2. Whose corresponding table exists but the migration that creates it
       hasn't been recorded in django_migrations (incomplete migration)
    
    This situation occurs when:
    - Migrations are interrupted mid-execution
    - Database volumes contain remnants from incomplete migrations
    - Manual database operations left orphaned types
    
    Cleanup Criteria:
    - Type exists in pg_type catalog
    - Either: table does NOT exist, OR migration not recorded as applied
    - Type name follows Django naming pattern (app_model format)
    
    Returns:
        bool: True if cleanup completed successfully (or no cleanup needed),
              False if database connection failed or couldn't be established.
    
    Raises:
        No exceptions are raised; all errors are caught and logged.
    """
    
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
        
        # Determine backend path for migration parsing
        # Script is expected to be in /app/ when running in Docker
        backend_path = os.path.dirname(os.path.abspath(__file__))
        
        # Get tables that should be created by pending migrations
        pending_migration_tables = get_pending_migration_models(cursor, backend_path)
        if pending_migration_tables:
            print(f"Found {len(pending_migration_tables)} table(s) from pending migrations to check")
        
        # Query all custom types in the public schema that match Django naming pattern
        # Django typically creates types with format: appname_modelname
        # Examples: accounts_user, investments_investmentpackage, billing_invoice
        # The pattern '%_%' matches any string with at least one underscore (all Django tables)
        # Additional safety checks: composite types only, exclude pg_ prefix, verify table doesn't exist
        cursor.execute("""
            SELECT t.typname
            FROM pg_type t
            JOIN pg_namespace n ON t.typnamespace = n.oid
            WHERE n.nspname = 'public'
            AND t.typtype = 'c'  -- composite type (used for tables)
            AND t.typname LIKE '%_%'  -- contains underscore (Django pattern)
            AND t.typname NOT LIKE 'pg_%'  -- exclude PostgreSQL internal types
            ORDER BY t.typname
        """)
        
        stale_types = cursor.fetchall()
        
        if not stale_types:
            print("✅ No custom types found. Database is clean.")
        else:
            print(f"Found {len(stale_types)} custom type(s) to check...")
            
            # Check each type to see if it's stale
            for (type_name,) in stale_types:
                # Check if the corresponding table exists
                # Django table names match the type names
                cursor.execute("""
                    SELECT EXISTS (
                        SELECT FROM information_schema.tables 
                        WHERE table_schema = 'public' 
                        AND table_name = %s
                    )
                """, (type_name,))
                
                table_exists = cursor.fetchone()[0]
                
                # A type is stale if:
                # 1. No corresponding table exists, OR
                # 2. The table exists but should be created by a pending migration
                #    (meaning it was created by an incomplete migration)
                is_from_pending_migration = type_name in pending_migration_tables
                
                if not table_exists:
                    # Safe to drop the type since the table doesn't exist
                    print(f"Found stale '{type_name}' type (no table). Attempting cleanup...")
                    try:
                        cursor.execute(sql.SQL("DROP TYPE IF EXISTS {} CASCADE").format(
                            sql.Identifier(type_name)
                        ))
                        print(f"✅ Successfully removed stale '{type_name}' type")
                        cleanup_performed = True
                    except Exception as e:
                        print(f"⚠️  Warning: Could not drop type '{type_name}': {e}")
                        print("This may be normal if the type is in use")
                elif is_from_pending_migration:
                    # Table exists but migration hasn't been recorded as applied
                    # This indicates an incomplete migration - drop both table and type
                    print(f"Found stale '{type_name}' from incomplete migration. Attempting cleanup...")
                    try:
                        # Drop the table first (which will also drop the type in most cases)
                        cursor.execute(sql.SQL("DROP TABLE IF EXISTS {} CASCADE").format(
                            sql.Identifier(type_name)
                        ))
                        # Also explicitly drop the type in case it wasn't dropped with the table
                        cursor.execute(sql.SQL("DROP TYPE IF EXISTS {} CASCADE").format(
                            sql.Identifier(type_name)
                        ))
                        print(f"✅ Successfully removed stale '{type_name}' table and type from incomplete migration")
                        cleanup_performed = True
                    except Exception as e:
                        print(f"⚠️  Warning: Could not drop '{type_name}': {e}")
                        print("This may require manual intervention")
                else:
                    # Table exists and migration was applied - this is normal
                    pass  # Don't print for every table, reduces noise
            
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
