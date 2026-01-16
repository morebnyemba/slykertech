#!/bin/bash

# CORS and WebSocket Testing Script
# This script tests CORS headers and WebSocket connections after deployment

set -e

# Configuration
API_URL="${API_URL:-https://api.slykertech.co.zw}"
FRONTEND_URL="${FRONTEND_URL:-https://slykertech.co.zw}"

echo "======================================"
echo "CORS and WebSocket Testing Script"
echo "======================================"
echo "API URL: $API_URL"
echo "Frontend Origin: $FRONTEND_URL"
echo ""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Test 1: OPTIONS Preflight Request
echo "Test 1: OPTIONS Preflight Request to /api/billing/carts/current/"
echo "--------------------------------------"
response=$(curl -s -o /dev/null -w "%{http_code}" -X OPTIONS \
  -H "Origin: $FRONTEND_URL" \
  -H "Access-Control-Request-Method: GET" \
  -H "Access-Control-Request-Headers: Authorization" \
  "$API_URL/api/billing/carts/current/")

if [ "$response" = "204" ] || [ "$response" = "200" ]; then
  echo -e "${GREEN}✓ PASS${NC} - Preflight request successful (HTTP $response)"
else
  echo -e "${RED}✗ FAIL${NC} - Preflight request failed (HTTP $response)"
fi

# Check CORS headers in OPTIONS response
echo ""
echo "Checking CORS headers in OPTIONS response:"
curl -s -I -X OPTIONS \
  -H "Origin: $FRONTEND_URL" \
  -H "Access-Control-Request-Method: GET" \
  -H "Access-Control-Request-Headers: Authorization" \
  "$API_URL/api/billing/carts/current/" | grep -i "access-control" || echo -e "${RED}No CORS headers found${NC}"

echo ""
echo ""

# Test 2: GET Request with Origin
echo "Test 2: GET Request to /api/billing/carts/current/ with Origin header"
echo "--------------------------------------"
response=$(curl -s -o /dev/null -w "%{http_code}" -X GET \
  -H "Origin: $FRONTEND_URL" \
  -H "Content-Type: application/json" \
  "$API_URL/api/billing/carts/current/")

if [ "$response" = "200" ] || [ "$response" = "201" ]; then
  echo -e "${GREEN}✓ PASS${NC} - GET request successful (HTTP $response)"
elif [ "$response" = "401" ] || [ "$response" = "403" ]; then
  echo -e "${YELLOW}⚠ INFO${NC} - Authentication required (HTTP $response) - this is expected"
else
  echo -e "${YELLOW}⚠ WARN${NC} - Unexpected response (HTTP $response)"
fi

# Check CORS headers in GET response
echo ""
echo "Checking CORS headers in GET response:"
curl -s -I -X GET \
  -H "Origin: $FRONTEND_URL" \
  "$API_URL/api/billing/carts/current/" | grep -i "access-control" || echo -e "${RED}No CORS headers found${NC}"

echo ""
echo ""

# Test 3: Health Check
echo "Test 3: Health Check Endpoint"
echo "--------------------------------------"
response=$(curl -s -o /dev/null -w "%{http_code}" "$API_URL/health/")

if [ "$response" = "200" ]; then
  echo -e "${GREEN}✓ PASS${NC} - Health check successful (HTTP $response)"
else
  echo -e "${RED}✗ FAIL${NC} - Health check failed (HTTP $response)"
fi

echo ""
echo ""

# Test 4: WebSocket Connection (requires wscat or websocat)
echo "Test 4: WebSocket Connection Test"
echo "--------------------------------------"
if command -v wscat &> /dev/null; then
  echo "Testing WebSocket connection with wscat..."
  timeout 5 wscat -c "wss://api.slykertech.co.zw/ws/analytics/" -H "Origin: $FRONTEND_URL" 2>&1 | head -5 || true
elif command -v websocat &> /dev/null; then
  echo "Testing WebSocket connection with websocat..."
  timeout 5 websocat -H="Origin: $FRONTEND_URL" "wss://api.slykertech.co.zw/ws/analytics/" 2>&1 | head -5 || true
else
  echo -e "${YELLOW}⚠ SKIP${NC} - wscat or websocat not installed"
  echo "Install with: npm install -g wscat"
  echo "Or install websocat from: https://github.com/vi/websocat"
fi

echo ""
echo ""

# Test 5: CORS Headers on Error Response
echo "Test 5: CORS Headers on 404 Error Response"
echo "--------------------------------------"
response=$(curl -s -o /dev/null -w "%{http_code}" -X GET \
  -H "Origin: $FRONTEND_URL" \
  "$API_URL/api/nonexistent-endpoint/")

echo "Response code: $response"
echo "Checking CORS headers on error response:"
curl -s -I -X GET \
  -H "Origin: $FRONTEND_URL" \
  "$API_URL/api/nonexistent-endpoint/" | grep -i "access-control" && echo -e "${GREEN}✓ PASS${NC} - CORS headers present on error response" || echo -e "${RED}✗ FAIL${NC} - No CORS headers on error response"

echo ""
echo ""

echo "======================================"
echo "Testing Complete"
echo "======================================"
echo ""
echo "Summary:"
echo "- If all tests pass with CORS headers, the fix is working correctly"
echo "- 401/403 responses are expected for protected endpoints"
echo "- CORS headers should be present on ALL responses (including errors)"
echo ""
echo "Next steps:"
echo "1. Test from the actual frontend application"
echo "2. Check browser console for any CORS errors"
echo "3. Monitor application logs for any issues"
