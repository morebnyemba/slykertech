# PostgreSQL Migration Error Fix - Duplicate Type Constraint

## Error Description

When starting the backend service, you may encounter this error during database migrations:

```
psycopg2.errors.UniqueViolation: duplicate key value violates unique constraint "pg_type_typname_nsp_index"
DETAIL: Key (typname, typnamespace)=(investments_investmentpackage, 2200) already exists.
```

Or similar errors with other model types like `accounts_user`, `services_service`, etc.

## What Causes This Error

This error occurs when PostgreSQL's internal type catalog (`pg_type`) contains stale or duplicate type definitions. This typically happens when:

1. **Interrupted Migrations**: A previous migration was interrupted or failed partway through
2. **Database Volume Reuse**: The database volume contains remnants from previous incomplete migrations
3. **Manual Schema Changes**: Direct database modifications that left orphaned types
4. **Concurrent Migration Attempts**: Multiple migration processes running simultaneously

## How This Is Fixed Automatically

The system now includes an automatic cleanup mechanism:

1. **Cleanup Script** (`backend/cleanup_db_types.py`): 
   - Runs before every migration
   - Automatically detects ALL stale PostgreSQL types (not just hardcoded ones)
   - Queries all custom types matching Django's naming pattern (appname_modelname)
   - Safely removes types that don't have corresponding tables
   - Logs all actions for transparency

2. **Docker Entrypoint** (`backend/docker-entrypoint.sh`):
   - Automatically runs the cleanup script during container startup
   - Proceeds with migrations after cleanup
   - Handles errors gracefully

3. **Database Reset Script** (`fix-db-auth.sh`):
   - Includes cleanup step in the database reset workflow
   - Ensures clean slate for migrations

## Manual Intervention (If Needed)

If you still encounter issues, you can manually run the cleanup:

### Option 1: Using Docker
```bash
docker-compose exec backend python /app/cleanup_db_types.py
docker-compose exec backend python manage.py migrate
```

### Option 2: Database Reset (Nuclear Option)
If the cleanup doesn't work, reset the entire database:

```bash
./fix-db-auth.sh
```

**⚠️ WARNING**: This deletes all database data!

### Option 3: Manual SQL Cleanup
For advanced users, connect to PostgreSQL and run:

```sql
-- Check for ALL problematic types
SELECT t.typname, n.nspname 
FROM pg_type t 
JOIN pg_namespace n ON t.typnamespace = n.oid 
WHERE t.typtype = 'c' AND n.nspname = 'public'
AND NOT EXISTS (
    SELECT FROM information_schema.tables 
    WHERE table_schema = 'public' AND table_name = t.typname
);

-- Drop stale types (replace 'type_name' with actual stale type)
DROP TYPE IF EXISTS investments_investmentpackage CASCADE;
DROP TYPE IF EXISTS accounts_user CASCADE;

-- Then retry migrations
```

## Prevention Tips

1. **Always let migrations complete**: Don't interrupt `docker-compose up` during migrations
2. **Use atomic operations**: The cleanup script is now built-in, so restarts are safe
3. **Clean database for fresh starts**: Use `./fix-db-auth.sh` for complete resets
4. **Monitor logs**: Check `docker-compose logs backend` for migration issues

## Technical Details

### What is `pg_type_typname_nsp_index`?

This is PostgreSQL's internal unique index that ensures each type name is unique within a namespace. The error means:
- Type name: e.g., `investments_investmentpackage` or `accounts_user`
- Namespace: `2200` (typically the `public` schema)
- Already exists in the catalog

### Why Does This Happen with Django Models?

Django creates PostgreSQL composite types for:
- Each model/table in the database
- Enum fields (like `user_type`, `status`)
- Custom field types

If migrations are interrupted, these types can remain in `pg_type` even if the table creation failed, causing conflicts on retry. The cleanup script now automatically detects and removes ANY stale types, not just specific ones.

## Related Documentation

- [Database Authentication Error Fix](DB_AUTH_ERROR_FIX.md)
- [Docker Setup Guide](DOCKER_SETUP_README.md)
- [Database Troubleshooting](TROUBLESHOOTING_DB_AUTH.md)

## Verification

After the fix is applied, you should see in your logs:

```
Cleaning up stale database types...
✅ No stale types found. Database is clean.

Running database migrations...
Operations to perform:
  Apply all migrations: accounts, admin, auth, ...
Running migrations:
  Applying auth.0010_alter_group_name_max_length... OK
  Applying auth.0011_update_proxy_permissions... OK
  ...
✅ Migrations completed
```

If you see this, your database is healthy and migrations are working correctly!
