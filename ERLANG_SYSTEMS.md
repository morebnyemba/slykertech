# Erlang Systems - Architecture & Implementation

## Overview

This directory contains the Erlang OTP-based systems for high-concurrency operations:
- **Ticketing System** (`ticketing/`) - Handle 10,000+ concurrent tickets
- **Live Chat System** (`livechat_erlang/`) - Manage 10,000+ WebSocket connections

## Why Erlang?

- **Concurrency**: Lightweight processes (2KB each)
- **Fault Tolerance**: OTP supervision trees with automatic recovery
- **Low Latency**: <10ms message routing
- **Hot Code Swapping**: Update without disconnecting users
- **Distributed**: Scale across multiple nodes

## Architecture

### Ticketing System (Port 4000)
```
ticket_server.erl          Main GenServer for ticket management
├── create_ticket/1        Create new tickets
├── get_ticket/1           Retrieve ticket details
├── update_ticket/2        Update ticket status
└── notify_django/3        Sync with Django backend
```

### Live Chat System (Port 4001)
```
chat_server.erl            Main GenServer for chat sessions
├── new_session/1          Create chat session
├── send_message/2         Handle incoming messages
├── get_ai_response/3      Call Django Gemini AI
└── close_session/1        Clean up session
```

## Integration with Django

Both systems communicate with Django via HTTP REST API:

**Ticketing → Django:**
- POST `/api/tickets/sync/` - Sync ticket data
- GET `/api/tickets/{id}/` - Fetch ticket details

**Live Chat → Django:**
- POST `/api/livechat/ai/respond/` - Get Gemini AI response
- POST `/api/livechat/tickets/create/` - Create ticket from chat
- POST `/api/livechat/services/manage/` - AI service actions

## Building & Running

### Development (Local)
```bash
cd ticketing
rebar3 compile
rebar3 shell

cd livechat_erlang
rebar3 compile
rebar3 shell
```

### Production (Docker)
```bash
docker-compose up -d ticketing livechat
```

## Current Status

**✅ Completed:**
- Basic Erlang app structure
- Dockerfiles for both systems
- rebar3 configuration
- Main GenServer skeletons

**⚠️ In Progress:**
- WebSocket handlers (Cowboy)
- Django HTTP client integration
- Message routing logic
- Session management
- Presence tracking

**❌ TODO:**
- Supervision trees
- Error handling & recovery
- Load balancing
- Monitoring & metrics
- Integration tests

## Performance Targets

- **Concurrent Sessions:** 10,000+
- **Message Latency:** <10ms p95
- **Memory per Session:** <5KB
- **Uptime:** 99.9%

## Dependencies

- Erlang/OTP 26+
- Cowboy (WebSocket server)
- JSX (JSON encoding/decoding)
- Hackney (HTTP client)
- Gun (HTTP/2 client)

## Next Steps

1. Implement WebSocket handlers with Cowboy
2. Add Django HTTP client with proper error handling
3. Create supervision trees for fault tolerance
4. Build integration tests
5. Add monitoring with telemetry
6. Performance testing with concurrent connections

## Resources

- [Erlang OTP Documentation](https://www.erlang.org/doc/)
- [Cowboy WebSocket Guide](https://ninenines.eu/docs/en/cowboy/2.10/guide/ws_handlers/)
- [Learn You Some Erlang](https://learnyousomeerlang.com/)
