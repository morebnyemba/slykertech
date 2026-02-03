# Database Query System for Livechat AI

## Overview

The livechat AI personas can now execute **secure database queries** to provide personalized, account-aware responses. All queries are:

- ✅ **Scoped to current visitor only** - Cannot access other customers' data
- ✅ **Whitelisted** - Only approved queries and fields available
- ✅ **Audited** - All queries logged for security compliance
- ✅ **Field-restricted** - Sensitive data (passwords, API keys, payment methods) blocked

## Available Queries

### 1. `get_visitor_services`
Get all services owned by the current visitor.

**Usage by AI:**
```json
{
  "reply": "Let me check your services...",
  "action": "query_database",
  "action_payload": {
    "query": "get_visitor_services"
  }
}
```

**Response Example:**
```json
{
  "status": "success",
  "services": [
    {
      "id": 1,
      "service_type": "shared_hosting",
      "domain_name": "example.com",
      "status": "active",
      "renewal_date": "2025-12-15",
      "next_billing_date": "2025-12-15"
    },
    {
      "id": 2,
      "service_type": "domain",
      "domain_name": "example2.com",
      "status": "active",
      "renewal_date": "2026-03-20"
    }
  ],
  "count": 2
}
```

**When AI uses this:**
- Customer asks "What services do I have?"
- Sales asking "Which hosting plans are you currently using?"
- Support needs to see all services to troubleshoot

---

### 2. `get_visitor_account_status`
Get account summary including total services, active/expired count, spending, and account age.

**Usage by AI:**
```json
{
  "reply": "Let me pull up your account summary...",
  "action": "query_database",
  "action_payload": {
    "query": "get_visitor_account_status"
  }
}
```

**Response Example:**
```json
{
  "status": "success",
  "account": {
    "total_services": 5,
    "active_services": 4,
    "expired_services": 1,
    "total_spending": "N/A",
    "last_purchase": "2025-11-30",
    "account_created": "2023-05-15T08:30:00Z"
  }
}
```

**When AI uses this:**
- Billing asking for account overview
- Support confirming account status before escalation
- Sales looking at customer value/history

---

### 3. `get_visitor_chat_history`
Get recent chat messages from current conversation (max 20 messages).

**Usage by AI:**
```json
{
  "reply": "Let me review our conversation...",
  "action": "query_database",
  "action_payload": {
    "query": "get_visitor_chat_history",
    "params": { "limit": 10 }
  }
}
```

**Response Example:**
```json
{
  "status": "success",
  "messages": [
    {
      "sender_type": "user",
      "message": "I need help with my hosting renewal",
      "created_at": "2025-02-03T10:00:00Z"
    },
    {
      "sender_type": "ai",
      "message": "I'd be happy to help with your renewal...",
      "created_at": "2025-02-03T10:01:00Z"
    }
  ],
  "count": 2
}
```

**When AI uses this:**
- Support reviewing conversation context
- Confirming what was discussed earlier
- Checking for repeated questions

---

### 4. `get_service_details`
Get detailed information about a specific service by ID.

**Usage by AI:**
```json
{
  "reply": "Let me get the details for that service...",
  "action": "query_database",
  "action_payload": {
    "query": "get_service_details",
    "params": { "service_id": 123 }
  }
}
```

**Response Example:**
```json
{
  "status": "success",
  "service": {
    "id": 123,
    "service_type": "vps",
    "domain_name": "mysite.com",
    "status": "active",
    "renewal_date": "2025-08-10",
    "next_billing_date": "2025-08-10",
    "created_at": "2023-06-01T14:30:00Z"
  }
}
```

**When AI uses this:**
- Support troubleshooting specific service
- Billing inquiries about particular service
- Transfer human with service context

---

## Security Model

### 1. Visitor Verification
Each query requires:
- `session_id` - Current chat session ID
- `visitor_name` - Current visitor name
- These must match the session record

### 2. Authentication Check
If visitor is not authenticated (no `user_id`):
- Cannot query services, billing, or sensitive data
- Can only view chat history and general info
- Returns `status: "unauthenticated"` for protected queries

### 3. Field Whitelisting
**Allowed fields per model:**

**Service:**
- id, service_type, domain_name, status, renewal_date, next_billing_date, created_at

**ChatSession:**
- visitor_name, department, status, created_at, last_active_at

**ChatMessage:**
- sender_type, message (truncated to 500 chars), created_at

**Blocked fields (NEVER returned):**
- password, api_key, billing_address, credit_card, payment_method, email, phone, ip_address, token, secret

### 4. Query Audit Logging
All executed queries are logged with:
- Query name and timestamp
- User ID (if authenticated)
- Session ID
- Result status

Example log:
```
Database query: get_visitor_services for user_id=42, session_id=abc123
Database query: get_service_details for service_id=5, user_id=42
```

---

## AI Persona Integration

### Sales Persona
✅ Can use database queries to:
- Check what services customer currently has
- Understand renewal dates for upsell opportunities
- Review account history

### Support Persona
✅ Can use database queries to:
- Retrieve customer services for troubleshooting
- Check account status
- Review conversation context
- Find specific service details

### Billing Persona
✅ Can use database queries to:
- Get account summary for billing inquiries
- Check service status and renewal dates
- Retrieve account history

### Management Persona
✅ Can use database queries to:
- Pull full account data for escalated issues
- Review service history
- Check account activity and timing

### Default Persona
⚠️ Limited query access:
- Can query chat history
- Cannot access service/billing data without proper department

---

## Implementation Details

### Flow Diagram

```
Customer Message
    ↓
Gemini AI (with database query instructions)
    ↓
AI decides: Do I need customer data?
    ├─→ NO: Return reply only
    └─→ YES: Return action="query_database"
    ↓
Bridge API receives action
    ↓
execute_database_query() called with:
    - query name (whitelisted)
    - session_id, visitor_name (verification)
    - params (additional query params)
    ↓
database_queries.py validates:
    ✓ Query name is whitelisted
    ✓ Visitor owns this session
    ✓ Visitor is authenticated (if needed)
    ✓ Fields are whitelisted
    ↓
Query executed (only on current visitor's data)
    ↓
Result returned to bridge → Gemini context → AI uses for response
    ↓
Response with data-driven insights sent to customer
```

### Response Format

When AI uses a database query, the action result is structured as:

```json
{
  "action": "query_database",
  "action_result": {
    "success": true,
    "query": "get_visitor_services",
    "data": {
      "status": "success",
      "services": [...]
    },
    "message": "Query executed."
  }
}
```

The `action_result.data` contains the actual query result that Gemini can reference.

---

## Testing Queries

### Manual Test (via API)

```bash
curl -X POST http://localhost:8000/api/livechat/ai_response/ \
  -H "Content-Type: application/json" \
  -d '{
    "message": "What services do I have?",
    "visitor_name": "John Doe",
    "department": "support",
    "session_id": "test-session-123",
    "user_id": 42
  }'
```

### In Livechat Widget

Simply ask a question that requires data:
- "What services do I have?"
- "What's my account status?"
- "Can you check my hosting renewal date?"
- "Tell me about service #5"

The AI will automatically decide whether to query the database.

---

## Limitations & Edge Cases

### Cannot Query
- ❌ Other customers' data (enforced by session verification)
- ❌ Sensitive fields (passwords, API keys, payment info)
- ❌ Data outside the whitelisted queries
- ❌ Data if visitor is not authenticated (for protected queries)

### Empty Results
If visitor has no services or query returns empty:
```json
{
  "status": "success",
  "services": [],
  "count": 0
}
```
AI should respond naturally: "You don't have any services yet" or offer to help create one.

### Errors
If query fails:
```json
{
  "status": "error",
  "message": "Query failed: [error description]"
}
```
AI should acknowledge: "I'm having trouble pulling that data right now. Let me connect you with a specialist."

---

## Future Enhancements

Possible future database query additions:
- `get_support_tickets` - Retrieve customer's open tickets
- `get_invoices` - Get invoice history
- `get_domain_dns` - Check DNS records
- `check_ssl_status` - Verify SSL certificate status
- `get_uptime_stats` - Pull service uptime/performance metrics

Each would follow the same security model (whitelisted fields, visitor verification, audit logging).
