/**
 * Domain Search API Route
 * Endpoint for checking domain availability using WHOIS service
 */

import { NextRequest, NextResponse } from 'next/server';
import type { DomainSearchRequest } from '@/types/domain';

// Backend API URL
const BACKEND_API_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:8000';

// Rate limiting configuration
const RATE_LIMIT_MAX_DOMAINS = 10;
const RATE_LIMIT_TIMEOUT = 30000; // 30 seconds

/**
 * POST /api/domain-search
 * Check domain availability
 */
export async function POST(request: NextRequest) {
  try {
    // Parse request body
    const body: DomainSearchRequest = await request.json();

    // Validate request
    if (!body.domains || !Array.isArray(body.domains)) {
      return NextResponse.json(
        {
          error: 'Invalid request: domains must be an array',
        },
        { status: 400 }
      );
    }

    // Check domain count limit
    if (body.domains.length === 0) {
      return NextResponse.json(
        {
          error: 'At least one domain is required',
        },
        { status: 400 }
      );
    }

    if (body.domains.length > RATE_LIMIT_MAX_DOMAINS) {
      return NextResponse.json(
        {
          error: `Maximum ${RATE_LIMIT_MAX_DOMAINS} domains per request`,
        },
        { status: 400 }
      );
    }

    // Sanitize domain inputs
    const sanitizedDomains = body.domains.map(d => 
      d.trim().toLowerCase().replace(/[^a-z0-9.-]/g, '')
    );

    // Validate each domain format
    const domainRegex = /^(?:[a-z0-9](?:[a-z0-9-]{0,61}[a-z0-9])?\.)+[a-z]{2,}$/;
    const invalidDomains = sanitizedDomains.filter(d => !domainRegex.test(d));
    
    if (invalidDomains.length > 0) {
      return NextResponse.json(
        {
          error: `Invalid domain format: ${invalidDomains.join(', ')}`,
        },
        { status: 400 }
      );
    }

    // Call Python WHOIS service with parallel processing support
    const requestBody: { domains: string[]; parallel: boolean; max_workers: number } = {
      domains: sanitizedDomains,
      parallel: true,
      max_workers: 5
    };
    
    const response = await fetch(`${BACKEND_API_URL}/api/services/whois/check/`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(requestBody),
      signal: AbortSignal.timeout(RATE_LIMIT_TIMEOUT),
    });

    if (!response.ok) {
      // If the backend endpoint doesn't exist yet, return a helpful message
      if (response.status === 404) {
        return NextResponse.json(
          {
            error: 'WHOIS service endpoint not configured. Please set up the Django REST API endpoint at /api/whois/check/',
          },
          { status: 503 }
        );
      }

      const errorData = await response.json().catch(() => ({}));
      return NextResponse.json(
        {
          error: errorData.detail || errorData.error || 'Backend service error',
        },
        { status: response.status }
      );
    }

    const data = await response.json();
    
    // Return results
    return NextResponse.json(data);

  } catch (error) {
    console.error('Domain search API error:', error);
    
    if (error instanceof Error) {
      if (error.name === 'AbortError' || error.name === 'TimeoutError') {
        return NextResponse.json(
          {
            error: 'Request timeout - WHOIS queries took too long',
          },
          { status: 504 }
        );
      }
      
      return NextResponse.json(
        {
          error: error.message || 'Internal server error',
        },
        { status: 500 }
      );
    }

    return NextResponse.json(
      {
        error: 'Internal server error',
      },
      { status: 500 }
    );
  }
}

/**
 * GET /api/domain-search
 * Get API information
 */
export async function GET() {
  return NextResponse.json({
    name: 'Domain Search API',
    version: '1.0.0',
    description: 'Check domain availability using WHOIS servers',
    endpoints: {
      POST: {
        path: '/api/domain-search',
        description: 'Check domain availability',
        requestBody: {
          domains: ['example.com', 'test.org'],
        },
        response: {
          results: [
            {
              domain: 'string',
              available: 'boolean',
              tld: 'string',
              whoisServer: 'string',
              message: 'string (optional)',
              error: 'string (optional)',
            },
          ],
        },
      },
    },
    limits: {
      maxDomainsPerRequest: RATE_LIMIT_MAX_DOMAINS,
      timeout: `${RATE_LIMIT_TIMEOUT / 1000}s`,
    },
  });
}
