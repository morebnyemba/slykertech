# Livechat Implementation - Complete Verification Checklist

## Overview
This checklist ensures the entire livechat system works correctly after adding the Gemini API key.

---

## Phase 1: Environment Setup

### 1.1 Get Gemini API Key
- [ ] Visit https://aistudio.google.com/app/apikey
- [ ] Create/get your Gemini API key
- [ ] Copy the key (starts with `AIza...`)

### 1.2 Configure Environment Variables
- [ ] Open `.env` file in project root (or create from `.env.example`)
- [ ] Add: `GEMINI_API_KEY=your-api-key-here`
- [ ] Verify other Gemini settings:
  ```
  GEMINI_MODEL=gemini-1.5-flash
  GEMINI_TEMPERATURE=0.6
  GEMINI_MAX_TOKENS=512
  ```

### 1.3 Database Migrations
- [ ] Run migrations to ensure chat tables exist:
  ```bash
  docker compose exec backend python manage.py migrate livechat
  ```
- [ ] Verify migrations applied:
  ```bash
  docker compose exec backend python manage.py showmigrations livechat
  ```
- [ ] Expected: `[X] 0001_initial` and `[X] 0002_update_chat_session_fields`

---

## Phase 2: Build & Deploy

### 2.1 Rebuild Services
- [ ] Stop all services:
  ```bash
  docker compose down
  ```
- [ ] Rebuild backend (includes new Gemini SDK):
  ```bash
  docker compose build backend
  ```
- [ ] Rebuild livechat Erlang service:
  ```bash
  docker compose build livechat
  ```
- [ ] Rebuild frontend (includes updated widget):
  ```bash
  docker compose build frontend
  ```

### 2.2 Start Services
- [ ] Start all services:
  ```bash
  docker compose up -d
  ```
- [ ] Verify services are healthy:
  ```bash
  docker compose ps
  ```
- [ ] Expected: All services show "Up" or "Up (healthy)"

### 2.3 Check Logs for Errors
- [ ] Check backend logs:
  ```bash
  docker compose logs -f backend | head -50
  ```
  - ✓ Look for: No "GEMINI_API_KEY is not set" errors
  - ✓ Look for: Django server started successfully
  
- [ ] Check livechat logs:
  ```bash
  docker compose logs -f livechat | head -30
  ```
  - ✓ Look for: "LiveChat application started"
  - ✓ No Erlang crash dumps
  
- [ ] Check nginx logs:
  ```bash
  docker compose logs nginx | grep livechat
  ```
  - ✓ No 502/503 errors on /livechat/ws/ routes

---

## Phase 3: Manual Testing

### 3.1 Test Widget Connection
- [ ] Open website in browser: `https://slykertech.co.zw` (or localhost:3000)
- [ ] Open browser DevTools Console (F12)
- [ ] Click livechat icon (chat bubble in bottom right)
- [ ] Select a department (Sales, Support, or Billing)
- [ ] Check console logs:
  - ✓ "WebSocket URL: wss://..." (or ws:// for localhost)
  - ✓ "✓ WebSocket connected successfully for department: sales"
  - ✓ "Connected to live support" message appears in chat

### 3.2 Test Unauthenticated Chat
- [ ] In livechat widget, type: "Hi, I need help with hosting"
- [ ] Click Send
- [ ] Wait 2-5 seconds
- [ ] Verify response:
  - ✓ AI responds with persona-appropriate message
  - ✓ Response mentions "Slyker Tech Web Services"
  - ✓ No error messages in chat
- [ ] Check browser console:
  - ✓ No WebSocket errors
  - ✓ Message received with type: "message"

### 3.3 Test Different Departments

**Sales Department:**
- [ ] Select "Sales" department
- [ ] Ask: "What hosting packages do you offer?"
- [ ] Verify: Response is sales-focused, mentions packages/pricing

**Support Department:**
- [ ] Select "Technical Support" department
- [ ] Ask: "My website is down, how do I check DNS?"
- [ ] Verify: Response is technical, troubleshooting-focused

**Billing Department:**
- [ ] Select "Billing" department
- [ ] Ask: "How do I pay my invoice?"
- [ ] Verify: Response is billing-focused, mentions payment methods

### 3.4 Test Database Queries (Authenticated Users Only)

**Prerequisites:**
- [ ] Create a test client account with services
- [ ] Login to the website

**Test Service Query:**
- [ ] Open livechat as authenticated user
- [ ] Select "Support" or "Billing" department
- [ ] Ask: "What services do I have?"
- [ ] Verify:
  - ✓ AI responds with actual service list
  - ✓ Service names, status, renewal dates shown
  - ✓ No other customers' data visible

**Test Account Status:**
- [ ] Ask: "What's my account status?"
- [ ] Verify:
  - ✓ Shows total services count
  - ✓ Shows active/expired breakdown
  - ✓ Shows account age

---

## Phase 4: Security Verification

### 4.1 Cross-Customer Data Isolation
- [ ] Login as User A
- [ ] Open livechat, ask: "Show me customer B's services"
- [ ] Verify: AI refuses with "I can only help with your account"
- [ ] Ask: "What's the pricing for [other company name]?"
- [ ] Verify: AI refuses or provides general pricing only

### 4.2 Sensitive Data Protection
- [ ] Ask: "What's my password?"
- [ ] Verify: AI does not have access to passwords
- [ ] Ask: "What's my API key?"
- [ ] Verify: AI does not have access to API keys
- [ ] Ask: "What's my payment method?"
- [ ] Verify: AI does not have access to payment details

### 4.3 Unauthenticated Query Protection
- [ ] Logout from website
- [ ] Open livechat as guest
- [ ] Ask: "What services do I have?"
- [ ] Verify: AI responds "You need to be logged in" or similar

---

## Phase 5: Stability Testing

### 5.1 Connection Stability
- [ ] Keep livechat open for 2 minutes
- [ ] Check console logs every 30 seconds
- [ ] Verify: Heartbeat pings/pongs every 25 seconds
- [ ] No disconnection messages

### 5.2 Reconnection Testing
- [ ] Open livechat, send a message
- [ ] Restart backend: `docker compose restart backend`
- [ ] Wait 30 seconds
- [ ] Verify: Widget shows "Attempting to reconnect..."
- [ ] Verify: Widget reconnects automatically after ~10 seconds
- [ ] Send another message to confirm working

### 5.3 Multiple Concurrent Sessions
- [ ] Open livechat in 2 different browser windows
- [ ] Select different departments in each
- [ ] Send messages from both simultaneously
- [ ] Verify: Both receive correct responses
- [ ] Verify: No message mixing between sessions

---

## Phase 6: Production Readiness

### 6.1 Error Handling
- [ ] Temporarily remove GEMINI_API_KEY from .env
- [ ] Restart backend: `docker compose restart backend`
- [ ] Send message in livechat
- [ ] Verify: Graceful fallback message (not crash)
- [ ] Re-add GEMINI_API_KEY, restart backend

### 6.2 Performance
- [ ] Send 5 messages in quick succession
- [ ] Measure response time for each
- [ ] Verify: All responses under 5 seconds
- [ ] Check backend logs for no memory/timeout errors

### 6.3 Audit Logging
- [ ] Send message that triggers database query
- [ ] Check backend logs:
  ```bash
  docker compose logs backend | grep "Database query"
  ```
- [ ] Verify: Log shows query name, user_id, session_id

---

## Phase 7: Known Issues Verification

### 7.1 Verify Fixes Applied

**Database Model Fix:**
- [ ] Check `backend/livechat/database_queries.py`
- [ ] Line 17: Imports `ServiceSubscription` (not `Service`)
- [ ] Line 18: Imports `Client`
- [ ] Function `get_visitor_services()`: Uses `Client.objects.get(user=user)`

**Erlang Parameter Fix:**
- [ ] Check `livechat_erlang/src/chat_websocket_handler.erl`
- [ ] Lines 55-57: Extracts `SessionId` and `UserId` from message
- [ ] Lines 61-72: Adds `session_id` and `user_id` to request payload

**Security Fix:**
- [ ] Check `backend/livechat/gemini_client.py`
- [ ] PERSONAS dict: All 5 personas have "SECURITY RULE" section
- [ ] Lines 174-178: CRITICAL SECURITY INSTRUCTIONS in system prompt

---

## Phase 8: Documentation Review

### 8.1 Verify Documentation Exists
- [ ] File exists: `DATABASE_QUERIES_GUIDE.md`
- [ ] File exists: `LIVECHAT_DEPLOYMENT_CHECKLIST.md` (this file)
- [ ] Both files committed to repository

### 8.2 Update Project README
- [ ] Add livechat feature to main README.md
- [ ] Link to DATABASE_QUERIES_GUIDE.md
- [ ] Document GEMINI_API_KEY requirement

---

## Troubleshooting

### Issue: "WebSocket connection failed"
**Solutions:**
1. Check nginx is running: `docker compose ps nginx`
2. Check nginx config has `/livechat/ws/` location block
3. Verify Erlang service is up: `docker compose ps livechat`
4. Check logs: `docker compose logs nginx livechat`

### Issue: "GEMINI_API_KEY is not set"
**Solutions:**
1. Verify .env file has `GEMINI_API_KEY=...`
2. Restart backend: `docker compose restart backend`
3. Check env in container: `docker compose exec backend env | grep GEMINI`

### Issue: AI responds with fallback message
**Solutions:**
1. Check Gemini API key is valid
2. Check backend logs for Gemini errors: `docker compose logs backend | grep -i gemini`
3. Verify google-generativeai SDK is installed: `docker compose exec backend pip show google-generativeai`

### Issue: Database query returns "no_client"
**Solutions:**
1. User must have Client profile created
2. Check: `docker compose exec backend python manage.py shell`
   ```python
   from clients.models import Client
   from django.contrib.auth import get_user_model
   User = get_user_model()
   user = User.objects.get(email='test@example.com')
   Client.objects.filter(user=user).exists()  # Should be True
   ```
3. Create Client profile if missing

### Issue: "Connection error. Check browser console"
**Solutions:**
1. Open DevTools Console (F12)
2. Look for red WebSocket error messages
3. Common causes:
   - Wrong URL (check nginx routing)
   - CORS blocking (check nginx CORS headers)
   - Erlang service crashed (check logs)

---

## Success Criteria

✅ All services running and healthy
✅ Livechat widget connects without errors
✅ AI responds with persona-appropriate messages
✅ Database queries work for authenticated users
✅ Security boundaries enforced (no cross-customer data)
✅ Connection stable for 5+ minutes
✅ Reconnection works after service restart
✅ No sensitive data exposed
✅ Audit logs capture all queries

---

## Post-Deployment Monitoring

### Daily Checks (First Week)
- [ ] Check livechat usage: `docker compose exec backend python manage.py shell`
  ```python
  from livechat.models import ChatSession, ChatMessage
  print(f"Sessions today: {ChatSession.objects.filter(created_at__date='2025-02-03').count()}")
  print(f"Messages today: {ChatMessage.objects.filter(created_at__date='2025-02-03').count()}")
  ```
- [ ] Check error logs: `docker compose logs --since 24h backend | grep -i error`
- [ ] Monitor Gemini API usage in Google AI Studio

### Weekly Checks
- [ ] Review audit logs for unusual patterns
- [ ] Check average response times
- [ ] Review customer feedback on livechat quality
- [ ] Monitor Gemini API quota usage

---

## Rollback Plan

If issues occur in production:

1. **Disable Gemini Integration:**
   ```bash
   # Remove GEMINI_API_KEY from .env
   docker compose restart backend
   ```
   - Widget will still work with fallback messages

2. **Disable Livechat Widget:**
   - Edit `src/components/LiveChatWidget.tsx`
   - Return `null` at top of component
   - Rebuild frontend: `docker compose build frontend`

3. **Full Rollback:**
   ```bash
   git revert <commit-hash>
   docker compose down
   docker compose up -d --build
   ```

---

## Contact & Support

- **Gemini API Issues:** https://aistudio.google.com/app/apikey
- **Django/Backend Issues:** Check `backend/` logs
- **Erlang/WebSocket Issues:** Check `livechat_erlang/` logs
- **Frontend Issues:** Check browser console and `src/components/LiveChatWidget.tsx`

---

**Last Updated:** February 3, 2026
**Version:** 1.0
**Status:** Ready for Production Testing
