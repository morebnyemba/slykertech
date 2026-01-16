# Database Authentication Error - Quick Fix

## Error Message

```
FATAL: password authentication failed for user "slykertech"
```

## Quick Fix (One-Line Solution)

```bash
./fix-db-auth.sh
```

This script will:
1. Stop all containers
2. Remove the database volume with incorrect password
3. Restart services with the correct password from your .env file
4. Run database migrations

## Manual Fix

If you prefer to fix it manually:

### Step 1: Stop containers
```bash
docker-compose down
```

### Step 2: Remove database volume
```bash
docker volume rm slykertech_postgres_data
```

### Step 3: Ensure .env file exists and has correct password
```bash
# Check if .env exists
cat .env | grep DB_PASSWORD

# If not, create it
cp .env.example .env
# Edit .env and set DB_PASSWORD=your_password
```

### Step 4: Start services
```bash
docker-compose up -d --build
```

### Step 5: Wait for database to initialize (15 seconds)
```bash
sleep 15
```

### Step 6: Run migrations
```bash
docker-compose exec backend python manage.py migrate
```

### Step 7: Create superuser (optional)
```bash
docker-compose exec backend python manage.py createsuperuser
```

## Prevention

To prevent this error in the future:

1. **Always use the same password**: Once you set `DB_PASSWORD` in your `.env` file, don't change it unless you also delete the database volume.

2. **Use docker-setup.sh for initial setup**: This script ensures everything is configured correctly from the start.
   ```bash
   ./docker-setup.sh
   ```

3. **Keep .env file safe**: Don't delete or modify the `.env` file without understanding the consequences.

4. **Document your password**: Store your database password securely (e.g., in a password manager).

## Why This Happens

PostgreSQL initializes the database with the password provided in the `POSTGRES_PASSWORD` environment variable **only on first run**. If the database volume already exists with a different password, PostgreSQL won't change it, but your application will try to connect with the new password from the `.env` file, causing authentication to fail.

## Related Documentation

- [Full Docker Setup Guide](DOCKER_SETUP_README.md)
- [Main README](README.md)
- [Environment Setup Guide](ENV_SETUP_GUIDE.md)
