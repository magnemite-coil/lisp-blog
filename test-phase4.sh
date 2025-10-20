#!/bin/bash

# Phase 4 Error Handling Test Script
# Tests API endpoints and error handling functionality

set -e

BASE_URL="http://localhost:8080"
COOKIE_FILE="/tmp/lisp-blog-test-cookies.txt"

echo "================================================"
echo "Phase 4 Error Handling Test Script"
echo "================================================"
echo ""

# Clean up previous cookie file
rm -f "$COOKIE_FILE"

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Test counter
TESTS_PASSED=0
TESTS_FAILED=0

# Function to print test result
print_result() {
    if [ $1 -eq 0 ]; then
        echo -e "${GREEN}✓ PASS${NC}: $2"
        ((TESTS_PASSED++))
    else
        echo -e "${RED}✗ FAIL${NC}: $2"
        ((TESTS_FAILED++))
    fi
}

# Test 1: Login with valid credentials (should succeed and set cookie)
echo "Test 1: Login with valid credentials"
RESPONSE=$(curl -s -w "\n%{http_code}" -X POST "$BASE_URL/api/auth/login" \
    -H "Content-Type: application/json" \
    -c "$COOKIE_FILE" \
    -d '{"username":"testuser","password":"password123"}')

HTTP_CODE=$(echo "$RESPONSE" | tail -1)
BODY=$(echo "$RESPONSE" | sed '$d')

if [ "$HTTP_CODE" -eq 200 ]; then
    print_result 0 "Login successful (HTTP 200)"
    echo "   Response: $BODY"
else
    print_result 1 "Login failed (HTTP $HTTP_CODE)"
    echo "   Response: $BODY"
fi
echo ""

# Test 2: Login with invalid credentials (should return error)
echo "Test 2: Login with invalid credentials"
RESPONSE=$(curl -s -w "\n%{http_code}" -X POST "$BASE_URL/api/auth/login" \
    -H "Content-Type: application/json" \
    -d '{"username":"wronguser","password":"wrongpass"}')

HTTP_CODE=$(echo "$RESPONSE" | tail -1)
BODY=$(echo "$RESPONSE" | sed '$d')

if [ "$HTTP_CODE" -eq 401 ]; then
    print_result 0 "Invalid login rejected (HTTP 401)"
    echo "   Response: $BODY"
else
    print_result 1 "Expected HTTP 401, got $HTTP_CODE"
    echo "   Response: $BODY"
fi
echo ""

# Test 3: Get current user (authenticated)
echo "Test 3: Get current user (authenticated)"
RESPONSE=$(curl -s -w "\n%{http_code}" -X GET "$BASE_URL/api/auth/me" \
    -b "$COOKIE_FILE")

HTTP_CODE=$(echo "$RESPONSE" | tail -1)
BODY=$(echo "$RESPONSE" | sed '$d')

if [ "$HTTP_CODE" -eq 200 ]; then
    print_result 0 "Get current user successful (HTTP 200)"
    echo "   Response: $BODY"
else
    print_result 1 "Failed to get current user (HTTP $HTTP_CODE)"
    echo "   Response: $BODY"
fi
echo ""

# Test 4: Get current user (unauthenticated)
echo "Test 4: Get current user (unauthenticated)"
RESPONSE=$(curl -s -w "\n%{http_code}" -X GET "$BASE_URL/api/auth/me")

HTTP_CODE=$(echo "$RESPONSE" | tail -1)
BODY=$(echo "$RESPONSE" | sed '$d')

if [ "$HTTP_CODE" -eq 401 ]; then
    print_result 0 "Unauthenticated request rejected (HTTP 401)"
    echo "   Response: $BODY"
else
    print_result 1 "Expected HTTP 401, got $HTTP_CODE"
    echo "   Response: $BODY"
fi
echo ""

# Test 5: Create a post (authenticated)
echo "Test 5: Create a post (authenticated)"
RESPONSE=$(curl -s -w "\n%{http_code}" -X POST "$BASE_URL/api/posts" \
    -H "Content-Type: application/json" \
    -b "$COOKIE_FILE" \
    -d '{"title":"Phase 4 Test Post","content":"Testing error handling","status":"published"}')

HTTP_CODE=$(echo "$RESPONSE" | tail -1)
BODY=$(echo "$RESPONSE" | sed '$d')

if [ "$HTTP_CODE" -eq 201 ]; then
    print_result 0 "Post created successfully (HTTP 201)"
    echo "   Response: $BODY"
    # Extract post ID for later tests
    POST_ID=$(echo "$BODY" | grep -o '"id":[0-9]*' | grep -o '[0-9]*' | head -n 1)
    echo "   Created post ID: $POST_ID"
else
    print_result 1 "Failed to create post (HTTP $HTTP_CODE)"
    echo "   Response: $BODY"
    POST_ID=""
fi
echo ""

# Test 6: Get all posts
echo "Test 6: Get all posts"
RESPONSE=$(curl -s -w "\n%{http_code}" -X GET "$BASE_URL/api/posts")

HTTP_CODE=$(echo "$RESPONSE" | tail -1)
BODY=$(echo "$RESPONSE" | sed '$d')

if [ "$HTTP_CODE" -eq 200 ]; then
    print_result 0 "Get posts successful (HTTP 200)"
    POST_COUNT=$(echo "$BODY" | grep -o '"id":' | wc -l)
    echo "   Found $POST_COUNT posts"
else
    print_result 1 "Failed to get posts (HTTP $HTTP_CODE)"
    echo "   Response: $BODY"
fi
echo ""

# Test 7: Delete the created post (if exists)
if [ -n "$POST_ID" ]; then
    echo "Test 7: Delete post (authenticated)"
    RESPONSE=$(curl -s -w "\n%{http_code}" -X DELETE "$BASE_URL/api/posts/$POST_ID" \
        -b "$COOKIE_FILE")

    HTTP_CODE=$(echo "$RESPONSE" | tail -n 1)
    BODY=$(echo "$RESPONSE" | head -n -1)

    if [ "$HTTP_CODE" -eq 200 ]; then
        print_result 0 "Post deleted successfully (HTTP 200)"
        echo "   Response: $BODY"
    else
        print_result 1 "Failed to delete post (HTTP $HTTP_CODE)"
        echo "   Response: $BODY"
    fi
    echo ""
fi

# Test 8: Logout
echo "Test 8: Logout"
RESPONSE=$(curl -s -w "\n%{http_code}" -X POST "$BASE_URL/api/auth/logout" \
    -b "$COOKIE_FILE")

HTTP_CODE=$(echo "$RESPONSE" | tail -1)
BODY=$(echo "$RESPONSE" | sed '$d')

if [ "$HTTP_CODE" -eq 200 ]; then
    print_result 0 "Logout successful (HTTP 200)"
    echo "   Response: $BODY"
else
    print_result 1 "Failed to logout (HTTP $HTTP_CODE)"
    echo "   Response: $BODY"
fi
echo ""

# Clean up
rm -f "$COOKIE_FILE"

# Print summary
echo "================================================"
echo "Test Summary"
echo "================================================"
echo -e "Tests Passed: ${GREEN}$TESTS_PASSED${NC}"
echo -e "Tests Failed: ${RED}$TESTS_FAILED${NC}"
echo ""

if [ $TESTS_FAILED -eq 0 ]; then
    echo -e "${GREEN}All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}Some tests failed.${NC}"
    exit 1
fi
