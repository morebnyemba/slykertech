/**
 * WHOIS TLDs API Route
 * Returns list of supported TLDs from WHOIS configuration
 */

import { NextResponse } from 'next/server';
import whoisConfig from '../../../../dist.whois.json';

/**
 * GET /api/whois-tlds
 * Get list of supported TLDs
 */
export async function GET() {
  try {
    const tlds = Object.keys(whoisConfig.whois_servers).sort();
    
    return NextResponse.json({
      tlds,
      count: tlds.length,
    });
  } catch (error) {
    console.error('Error fetching TLDs:', error);
    return NextResponse.json(
      { error: 'Failed to fetch TLDs' },
      { status: 500 }
    );
  }
}
