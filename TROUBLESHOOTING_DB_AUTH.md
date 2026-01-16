# Database Password Authentication Error - Solutions

## The Problem

You're seeing this error:
```
FATAL: password authentication failed for user "slykertech"
```

This error occurs when the PostgreSQL database was initialized with a different password than what your application is currently using.

## ⚡ Quick Fix

Run the automated fix script:

```bash
./fix-db-auth.sh
```

**⚠️ WARNING:** This will delete all existing database data!

## 🔍 Understanding the Issue

PostgreSQL initializes the database password **only once** when the database volume is first created. The password comes from the `POSTGRES_PASSWORD` environment variable at that time.

If you later change the `DB_PASSWORD` in your `.env` file or if the `.env` file is missing, your application will try to connect with a different password than what the database expects, causing authentication to fail.

## 📝 Solutions

### Solution 1: Use the Automated Fix Script (Recommended)

This is the easiest and safest approach:

```bash
./fix-db-auth.sh
```

The script will:
1. Check if `.env` file exists (create it if missing)
2. Stop all containers
3. Remove the database volume
4. Restart services with the correct password
5. Run migrations automatically

### Solution 2: Manual Fix

If you prefer to do it manually:

1. **Stop all containers:**
   ```bash
   docker-compose down
   ```

2. **Remove the PostgreSQL volume:**
   ```bash
   # Find the volume name
   docker volume ls | grep postgres_data
   
   # Remove it (replace with your actual volume name)
   docker volume rm slykertech_postgres_data
   ```

3. **Ensure .env file exists and is configured:**
   ```bash
   # Check if .env exists
   ls -la .env
   
   # If not, create it from example
   cp .env.example .env
   
   # Edit .env and set a strong password
   nano .env  # or vim, code, etc.
   ```

4. **Start services:**
   ```bash
   docker-compose up -d
   ```

5. **Run migrations:**
   ```bash
   docker-compose exec backend python manage.py migrate
   ```

### Solution 3: First-Time Setup

If you're setting up the project for the first time, use:

```bash
./docker-setup.sh
```

This script handles everything:
- Creates `.env` file with proper configuration
- Sets up database with correct password
- Runs all necessary migrations
- Optionally creates superuser

## 🚫 Prevention

To avoid this error in the future:

1. **Never change `DB_PASSWORD` in .env after initial setup** - If you must change it, you need to reset the database volume

2. **Always use the setup scripts:**
   - For first-time setup: `./docker-setup.sh`
   - For fixing auth issues: `./fix-db-auth.sh`

3. **Keep your .env file safe:**
   - Don't delete it
   - Don't change DB_PASSWORD without resetting the database
   - Store your password securely (e.g., password manager)

4. **Use consistent passwords across team:**
   - Document your development database password
   - Share it securely with your team
   - Consider using a team password manager

## 📚 Environment Variables Explanation

### In docker-compose.yml:

```yaml
db:
  environment:
    POSTGRES_PASSWORD: ${DB_PASSWORD:-devpassword}
```

This reads `DB_PASSWORD` from `.env` file. If missing, it uses `devpassword` as default.

### In backend service:

```yaml
backend:
  environment:
    - DATABASE_URL=postgresql://slykertech:${DB_PASSWORD:-devpassword}@db:5432/slykertech
```

The backend connects using the same password from `.env`.

### The Problem Scenario:

1. Database is created with password "oldpassword"
2. Later, .env is changed or missing, so it uses "devpassword"
3. Application tries to connect with "devpassword"
4. Database expects "oldpassword"
5. **❌ Authentication fails!**

## 🔧 Advanced Troubleshooting

### Check if .env file exists:
```bash
cat .env | grep DB_PASSWORD
```

### Check Docker volumes:
```bash
docker volume ls | grep postgres
```

### View database logs:
```bash
docker-compose logs db
```

### View backend logs:
```bash
docker-compose logs backend
```

### Connect to database directly to test password:
```bash
docker-compose exec db psql -U slykertech -d slykertech
```

### Check environment variables in running container:
```bash
docker-compose exec backend env | grep DATABASE_URL
```

## 🆘 Still Having Issues?

If you're still experiencing problems:

1. Check that Docker and Docker Compose are up to date
2. Ensure you have enough disk space for Docker volumes
3. Try completely removing Docker data and starting fresh:
   ```bash
   docker-compose down -v
   docker system prune -a --volumes
   ./docker-setup.sh
   ```

4. Review the documentation:
   - [DOCKER_SETUP_README.md](DOCKER_SETUP_README.md)
   - [README.md](README.md)

## 📊 Environment Validation

The backend now includes automatic environment validation when it starts. If configuration is missing or incorrect, you'll see helpful error messages explaining what needs to be fixed.

The validation checks:
- ✅ DATABASE_URL is set
- ✅ Database password is not empty
- ⚠️ Warning if using default/insecure passwords
- ⚠️ Warning if DEBUG mode is enabled in production

This helps catch configuration issues early before they cause runtime errors.
