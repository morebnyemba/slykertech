#!/bin/bash

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${YELLOW}=== LiveChat Service Diagnostic ===${NC}\n"

# Check if livechat container is running
echo -e "${YELLOW}1. Checking livechat container status...${NC}"
if docker ps | grep -q livechat; then
    echo -e "${GREEN}✓ LiveChat container is running${NC}"
else
    echo -e "${RED}✗ LiveChat container is NOT running${NC}"
    echo "Starting livechat container..."
    docker compose up -d livechat
fi

# Check livechat health
echo -e "\n${YELLOW}2. Checking livechat health endpoint...${NC}"
HEALTH=$(curl -s -o /dev/null -w "%{http_code}" http://localhost:4001/health)
if [ "$HEALTH" = "200" ]; then
    echo -e "${GREEN}✓ LiveChat health check passed (HTTP $HEALTH)${NC}"
    curl -s http://localhost:4001/health | jq . 2>/dev/null || curl -s http://localhost:4001/health
else
    echo -e "${RED}✗ LiveChat health check failed (HTTP $HEALTH)${NC}"
fi

# Check nginx livechat route
echo -e "\n${YELLOW}3. Checking nginx configuration...${NC}"
if docker exec nginx grep -q "livechat/ws" /etc/nginx/nginx.conf; then
    echo -e "${GREEN}✓ Nginx has livechat route configured${NC}"
else
    echo -e "${RED}✗ Nginx livechat route not found${NC}"
fi

# Check if nginx is properly proxying to livechat
echo -e "\n${YELLOW}4. Checking livechat proxy through nginx...${NC}"
# Try to make an HTTP request to the health endpoint through nginx
# Note: WebSocket upgrade will fail with HTTP but we can check if nginx is routing
PROXY_HEALTH=$(curl -s -o /dev/null -w "%{http_code}" -H "Connection: Upgrade" http://localhost:80/livechat/ws/sales/)
if [ "$PROXY_HEALTH" = "101" ] || [ "$PROXY_HEALTH" = "400" ] || [ "$PROXY_HEALTH" = "404" ]; then
    echo -e "${GREEN}✓ Nginx is routing to livechat (HTTP $PROXY_HEALTH)${NC}"
else
    echo -e "${YELLOW}⚠ Nginx response: HTTP $PROXY_HEALTH${NC}"
fi

# Check livechat logs
echo -e "\n${YELLOW}5. Recent livechat logs...${NC}"
docker compose logs --tail 20 livechat

# Check if Erlang is actually running
echo -e "\n${YELLOW}6. Checking Erlang process...${NC}"
docker exec livechat ps aux | grep -E 'beam|erl' | grep -v grep || echo -e "${RED}No Erlang process found${NC}"

echo -e "\n${YELLOW}=== Diagnostic Complete ===${NC}"
