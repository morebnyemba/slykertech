# Gemini AI Configuration Guide

## Overview

The Gemini AI integration for livechat supports **two configuration methods**:

1. **Database Storage (Recommended)** - Encrypted API key stored in database via Django admin
2. **Environment Variables (Fallback)** - API key from `.env` file

The system automatically tries database first, then falls back to environment variables.

---

## Method 1: Database Configuration (Recommended)

### Why Use Database Storage?

✅ **Centralized Management** - Configure via Django admin interface  
✅ **Encrypted Storage** - API key automatically encrypted  
✅ **Multi-Environment** - Easy switching between sandbox/production  
✅ **No Redeployment** - Change settings without rebuilding containers  
✅ **Audit Trail** - Track when configurations were created/updated  

### Setup Steps

#### 1. Access Django Admin
Navigate to: `https://yourdomain.com/admin/integrations/apiconfiguration/`

#### 2. Create Gemini Configuration
Click **"Add API Configuration"** and fill in:

| Field | Value |
|-------|-------|
| **Provider** | Select "Google Gemini AI" |
| **Name** | "Production Gemini" (or your preferred name) |
| **API Key** | Your Gemini API key (starts with `AIza...`) |
| **Config Data** | See JSON below |
| **Is Active** | ✓ Checked |
| **Is Sandbox** | ☐ Unchecked (for production) |

**Config Data (JSON):**
```json
{
  "model": "gemini-1.5-flash",
  "temperature": 0.6,
  "max_tokens": 512
}
```

#### 3. Get Gemini API Key
- Visit: https://aistudio.google.com/app/apikey
- Create or copy your API key
- Paste into the **API Key** field in Django admin

#### 4. Save Configuration
Click **"Save"** - the API key will be automatically encrypted before storage.

### Using Python/Django Shell

Alternatively, create configuration via Django shell:

```python
from integrations.models import APIConfiguration

# Create Gemini configuration
config = APIConfiguration.objects.create(
    provider='gemini',
    name='Production Gemini AI',
    is_active=True,
    is_sandbox=False,
    config_data={
        'model': 'gemini-1.5-flash',
        'temperature': 0.6,
        'max_tokens': 512
    }
)

# Set API key (automatically encrypted)
config.set_api_key('AIzaYourGeminiAPIKeyHere')
config.save()

print("✓ Gemini configuration created successfully!")
```

### Verify Configuration

```python
from integrations.models import APIConfiguration

config = APIConfiguration.objects.get(provider='gemini', is_active=True)
print(f"Provider: {config.provider}")
print(f"Name: {config.name}")
print(f"Model: {config.config_data.get('model')}")
print(f"API Key (encrypted): {config.encrypted_api_key[:20]}...")
print(f"API Key (decrypted): {config.get_api_key()[:10]}...")  # Shows first 10 chars
```

### Update Configuration

To change settings without creating new entry:

```python
from integrations.models import APIConfiguration

config = APIConfiguration.objects.get(provider='gemini', is_active=True)

# Update model
config.config_data['model'] = 'gemini-1.5-pro'
config.config_data['max_tokens'] = 1024

# Update API key if needed
config.set_api_key('NewAPIKeyHere')

config.save()
print("✓ Configuration updated!")
```

---

## Method 2: Environment Variables (Fallback)

### When to Use

- Quick testing/development
- CI/CD pipelines
- When database access is not available
- Legacy compatibility

### Setup Steps

#### 1. Edit `.env` File

Add to your `.env` file (or Docker environment):

```env
# ===========================================
# GEMINI AI SETTINGS (Livechat)
# ===========================================
# Get your API key from: https://aistudio.google.com/app/apikey
GEMINI_API_KEY=AIzaYourGeminiAPIKeyHere
GEMINI_MODEL=gemini-1.5-flash
GEMINI_TEMPERATURE=0.6
GEMINI_MAX_TOKENS=512
```

#### 2. Restart Backend Service

```bash
docker compose restart backend
```

Or for local development:
```bash
# Restart Django server
python manage.py runserver
```

### Docker Compose Configuration

Already configured in `docker-compose.yml`:

```yaml
backend:
  environment:
    - GEMINI_API_KEY=${GEMINI_API_KEY:-}
    - GEMINI_MODEL=${GEMINI_MODEL:-gemini-1.5-flash}
    - GEMINI_TEMPERATURE=${GEMINI_TEMPERATURE:-0.6}
    - GEMINI_MAX_TOKENS=${GEMINI_MAX_TOKENS:-512}
```

---

## Configuration Precedence

The system checks configurations in this order:

1. **Database** (`APIConfiguration` with `provider='gemini'`, `is_active=True`)
2. **Environment Variables** (`GEMINI_API_KEY`, etc.)
3. **Error** (if neither exists)

### Example Code Flow

```python
def get_gemini_config():
    # 1. Try database first
    try:
        config = APIConfiguration.objects.filter(
            provider='gemini', is_active=True
        ).first()
        
        if config:
            return {
                'api_key': config.get_api_key(),  # Decrypted
                'model': config.config_data.get('model', 'gemini-1.5-flash'),
                ...
            }
    except:
        pass  # Database not available or no config
    
    # 2. Fallback to environment variables
    api_key = os.getenv("GEMINI_API_KEY")
    if api_key:
        return {
            'api_key': api_key,
            'model': os.getenv("GEMINI_MODEL", "gemini-1.5-flash"),
            ...
        }
    
    # 3. No configuration found
    return None
```

---

## Configuration Options

### Model Selection

| Model | Description | Use Case |
|-------|-------------|----------|
| `gemini-1.5-flash` | Fast, cost-effective | **Default** - Livechat responses |
| `gemini-1.5-pro` | Higher quality, slower | Complex queries, detailed analysis |
| `gemini-1.0-pro` | Legacy model | Compatibility |

### Temperature (0.0 - 2.0)

- **0.0-0.3**: Deterministic, factual, consistent
- **0.4-0.7**: Balanced creativity and consistency (default: 0.6)
- **0.8-1.0**: More creative, varied responses
- **1.1-2.0**: Highly creative, unpredictable

### Max Tokens

- **256**: Short, concise responses
- **512**: Standard responses (default)
- **1024**: Detailed explanations
- **2048**: Long-form content

---

## Security Best Practices

### ✅ DO:
- Store API keys in database (encrypted automatically)
- Use environment variables only for development
- Rotate API keys periodically
- Monitor API usage in Google AI Studio
- Set `is_active=False` to disable without deleting

### ❌ DON'T:
- Commit API keys to version control
- Share API keys in plain text
- Use production keys in development
- Leave unused configurations active

---

## Testing Configuration

### Test Database Configuration

```python
from livechat.gemini_client import get_gemini_config

config = get_gemini_config()
if config:
    print("✓ Configuration loaded successfully!")
    print(f"  Model: {config['model']}")
    print(f"  Temperature: {config['temperature']}")
    print(f"  API Key: {'*' * 20}{config['api_key'][-5:]}")
else:
    print("✗ No configuration found!")
```

### Test Gemini Response

```python
from livechat.gemini_client import generate_gemini_response

try:
    response = generate_gemini_response(
        message="Hello, test message",
        department="support",
        visitor_name="Test User"
    )
    print("✓ Gemini is working!")
    print(f"Response: {response[:100]}...")
except Exception as e:
    print(f"✗ Error: {e}")
```

---

## Troubleshooting

### Error: "Gemini API key is not configured"

**Cause:** No database config AND no environment variable  
**Solution:**
1. Check database: `APIConfiguration.objects.filter(provider='gemini', is_active=True).exists()`
2. Check environment: `echo $GEMINI_API_KEY` (Linux) or `$env:GEMINI_API_KEY` (Windows)
3. Add configuration using Method 1 or Method 2 above

### Error: "Invalid API key"

**Cause:** API key is incorrect or expired  
**Solution:**
1. Get new key from https://aistudio.google.com/app/apikey
2. Update in database:
   ```python
   config = APIConfiguration.objects.get(provider='gemini')
   config.set_api_key('NewKeyHere')
   config.save()
   ```

### Error: "Model not found"

**Cause:** Incorrect model name  
**Solution:** Use valid model names:
- `gemini-1.5-flash`
- `gemini-1.5-pro`
- `gemini-1.0-pro`

### Configuration Not Loading

1. **Check Database:**
   ```python
   from integrations.models import APIConfiguration
   config = APIConfiguration.objects.filter(provider='gemini').first()
   print(f"Active: {config.is_active if config else 'No config'}")
   ```

2. **Check Decryption:**
   ```python
   api_key = config.get_api_key()
   print(f"Can decrypt: {api_key is not None}")
   ```

3. **Check Environment:**
   ```python
   import os
   print(f"GEMINI_API_KEY: {os.getenv('GEMINI_API_KEY', 'Not set')}")
   ```

---

## Migration from Environment Variables

If you're currently using `.env` configuration:

### Step 1: Note Current Settings
```bash
grep GEMINI .env
```

### Step 2: Create Database Configuration
```python
from integrations.models import APIConfiguration
import os

config = APIConfiguration.objects.create(
    provider='gemini',
    name='Production Gemini AI',
    is_active=True,
    config_data={
        'model': os.getenv('GEMINI_MODEL', 'gemini-1.5-flash'),
        'temperature': float(os.getenv('GEMINI_TEMPERATURE', '0.6')),
        'max_tokens': int(os.getenv('GEMINI_MAX_TOKENS', '512')),
    }
)
config.set_api_key(os.getenv('GEMINI_API_KEY'))
config.save()
```

### Step 3: Test
Send a test message via livechat widget and verify response.

### Step 4: Remove from .env (Optional)
Once database config is verified, you can remove from `.env`:
```bash
# Comment out or remove these lines:
# GEMINI_API_KEY=...
# GEMINI_MODEL=...
# GEMINI_TEMPERATURE=...
# GEMINI_MAX_TOKENS=...
```

---

## API Usage Monitoring

### Check API Key Status
Visit: https://aistudio.google.com/app/apikey

### Monitor Usage
- Free tier: 15 requests/minute
- Track requests in Django logs:
  ```bash
  docker compose logs backend | grep "Gemini"
  ```

### Rate Limiting
If you hit rate limits, consider:
- Upgrading to paid tier
- Implementing request caching
- Adding rate limiting to livechat widget

---

## Related Documentation

- [Livechat Deployment Checklist](LIVECHAT_DEPLOYMENT_CHECKLIST.md)
- [Database Configuration Guide](DATABASE_CONFIGS_AND_PROJECT_TRACKING.md)
- [Livechat Review Summary](LIVECHAT_REVIEW_SUMMARY.md)
- [Google AI Studio](https://aistudio.google.com/app/apikey)

---

**Last Updated:** February 3, 2026  
**Configuration Method:** Database-first with environment fallback  
**Encryption:** Automatic via Fernet (cryptography library)
