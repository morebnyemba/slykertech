# Live Chat System Investigation & Fixes

## Issues Found

### 1. **Duplicate Widget Components**
- **Problem**: Two conflicting `LiveChatWidget.tsx` implementations:
  - `/src/components/LiveChatWidget.tsx` - Attempted Django WebSocket connection
  - `/src/components/chat/LiveChatWidget.tsx` - Attempted local Erlang connection
- **Impact**: Frontend was loading the Django version, which wasn't implemented properly

### 2. **Frontend Not Connecting to Erlang Service**
- **Problem**: LiveChatWidget was trying to connect to Django WebSocket instead of Erlang service
- **Root Cause**: Infrastructure for Django livechat was not implemented, but Erlang service was available

### 3. **Missing Nginx Route**
- **Problem**: Frontend couldn't reach Erlang livechat service through proper nginx proxy
- **Status**: `/ws/chat/` existed but frontend needed `/livechat/ws/` endpoint

### 4. **Incomplete Erlang Handler**
- **Problem**: `chat_websocket_handler.erl` was a stub, didn't properly handle messages
- **Missing**: JSON parsing, welcome messages, proper error handling

## Fixes Applied

### 1. ✅ Consolidated Frontend Widget
- **File**: `src/components/LiveChatWidget.tsx`
- **Changes**:
  - Single, unified implementation connecting to Erlang service
  - Proper WebSocket URL resolution (works in production & development)
  - Connection status indicator (green/red dot)
  - Visitor name prompt on first open
  - Auto-reconnect after 3 seconds on disconnect
  - Better error handling and system messages
  - Smooth scrolling to latest messages
  - Disabled input when disconnected
  - Proper message formatting (user/support/system types)

### 2. ✅ Removed Duplicate Component
- **File**: `src/components/chat/LiveChatWidget.tsx` (DELETED)
- **Reason**: Redundant and conflicting implementation

### 3. ✅ Enhanced Nginx Configuration
- **File**: `nginx.conf`
- **Changes**:
  - Added `/livechat/ws/` location to both api and www server blocks
  - Routes to Erlang service at `http://livechat:4001/ws/chat/`
  - Proper WebSocket headers for Upgrade and Connection
  - 24-hour read/send timeout for long-lived connections

### 4. ✅ Improved Erlang WebSocket Handler
- **File**: `livechat_erlang/src/chat_websocket_handler.erl`
- **Changes**:
  - Proper JSON message parsing using jsx library
  - Welcome message on connection
  - Handles "message" type events
  - Extracts visitor_name and message text
  - Calls Django AI bridge API for responses
  - Fallback messages on error
  - Try-catch for malformed JSON

## How It Works Now

1. **User opens chat widget** on website
2. **Prompted for name** (or uses logged-in user)
3. **WebSocket connects** via nginx to Erlang service
4. **Erlang receives** visitor messages
5. **Calls Django API** at `/api/livechat/bridge/ai_response/`
6. **Django processes** message (could call Gemini AI or route to support staff)
7. **Response sent back** to Erlang
8. **Erlang broadcasts** to user via WebSocket
9. **Widget displays** message in chat

## Connection Flow

```
Frontend (src/components/LiveChatWidget.tsx)
    ↓
Browser WebSocket
    ↓
Nginx (port 443 or 80)
    ↓
nginx.conf: /livechat/ws/ → http://livechat:4001/ws/chat/
    ↓
Erlang Service (livechat:4001)
    ↓
chat_websocket_handler.erl
    ↓
django_http:call_api() → Backend AI Bridge
    ↓
Django Livechat API (/api/livechat/bridge/ai_response/)
```

## Docker Compose Status

The livechat service is **properly configured** in docker-compose.yml:
- **Image**: Built from `livechat_erlang/Dockerfile`
- **Port**: 4001 (internal), routed through nginx
- **Health Check**: `/health` endpoint at port 4001
- **Dependencies**: Requires backend and ticketing services

## Next Steps

1. **Verify Django API endpoint** exists:
   - Endpoint: `/api/livechat/bridge/ai_response/`
   - Expected to accept POST requests with `{message, visitor_name}`
   - Return `{type: "message", text: "response"}`

2. **Test Connection**:
   ```bash
   # Check if Erlang service is running
   docker compose ps livechat
   
   # Check if service responds to health check
   curl http://localhost:4001/health
   ```

3. **Monitor Erlang Logs**:
   ```bash
   docker compose logs -f livechat
   ```

4. **Test Widget**:
   - Open website
   - Click chat widget (bottom-right)
   - Enter name and send message
   - Check browser console for WebSocket messages

## Files Modified

- ✅ `src/components/LiveChatWidget.tsx` - Rewritten for Erlang
- ✅ `src/components/chat/LiveChatWidget.tsx` - DELETED
- ✅ `nginx.conf` - Added /livechat/ws/ routes
- ✅ `livechat_erlang/src/chat_websocket_handler.erl` - Enhanced handler

## Files Unchanged (Already Good)

- ✅ `docker-compose.yml` - Livechat service properly configured
- ✅ `livechat_erlang/Dockerfile` - Correct build setup
- ✅ `livechat_erlang/src/health_handler.erl` - Responds correctly
- ✅ `livechat_erlang/rebar.config` - Has all dependencies (jsx, cowboy, gun)
- ✅ `livechat_erlang/src/livechat_app.erl` - Correct routing
