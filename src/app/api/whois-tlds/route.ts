/**
 * WHOIS TLDs API Route
 * Returns list of supported TLDs from WHOIS configuration
 */

import { NextResponse } from 'next/server';
import whoisConfig from '../../../../dist.whois.json';

// Define the type for WHOIS configuration entries
interface WhoisConfigEntry {
  extensions: string;
  uri: string;
  available: string;
}

/**
 * GET /api/whois-tlds
 * Get list of supported TLDs
 */
export async function GET() {
  try {
    // Extract all TLDs from the extensions array
    // The config is an array of objects with 'extensions' property containing comma-separated TLDs
    const tlds: string[] = [];
    const configArray = whoisConfig as WhoisConfigEntry[];
    
    for (const entry of configArray) {
      if (entry.extensions) {
        // Split comma-separated extensions and clean up
        const extensions = entry.extensions.split(',').map((ext: string) => 
          ext.trim().replace(/^\./, '') // Remove leading dot
        );
        tlds.push(...extensions);
      }
    }
    
    // Remove duplicates and sort
    const uniqueTlds = [...new Set(tlds)].sort();
    
    return NextResponse.json({
      tlds: uniqueTlds,
      count: uniqueTlds.length,
    });
  } catch (error) {
    console.error('Error fetching TLDs:', error);
    return NextResponse.json(
      { error: 'Failed to fetch TLDs' },
      { status: 500 }
    );
  }
}
