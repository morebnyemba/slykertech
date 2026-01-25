'use client';

/**
 * Domain Search Component
 * Provides domain availability search with WHOIS lookup
 */

import React, { useState } from 'react';
import { Search, Loader2, CheckCircle2, XCircle, Copy, Download, Filter } from 'lucide-react';
import { Button } from '@/components/ui/button';
import {
  validateDomain,
  parseMultipleDomains,
  validateMultipleDomains,
} from '@/lib/utils/domain-utils';
import type { DomainSearchResult } from '@/types/domain';

interface DomainSearchProps {
  className?: string;
}

type FilterType = 'all' | 'available' | 'unavailable';

export default function DomainSearch({ className = '' }: DomainSearchProps) {
  const [searchInput, setSearchInput] = useState('');
  const [results, setResults] = useState<DomainSearchResult[]>([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [filter, setFilter] = useState<FilterType>('all');
  const [copiedDomain, setCopiedDomain] = useState<string | null>(null);

  const handleSearch = async () => {
    // Clear previous results and errors
    setError(null);
    setResults([]);

    // Parse and validate domains
    const domains = parseMultipleDomains(searchInput);
    
    if (domains.length === 0) {
      setError('Please enter at least one domain name');
      return;
    }

    // Validate domains
    const { valid, invalid } = validateMultipleDomains(domains);
    
    if (invalid.length > 0) {
      setError(`Invalid domains: ${invalid.map(i => i.domain).join(', ')}`);
      return;
    }

    if (valid.length > 10) {
      setError('Maximum 10 domains per search');
      return;
    }

    setLoading(true);

    try {
      // Call API endpoint
      const response = await fetch('/api/domain-search', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({ domains: valid }),
      });

      if (!response.ok) {
        const errorData = await response.json();
        throw new Error(errorData.error || 'Search failed');
      }

      const data = await response.json();
      setResults(data.results || []);
    } catch (err) {
      setError(err instanceof Error ? err.message : 'An error occurred');
    } finally {
      setLoading(false);
    }
  };

  const handleCopy = async (domain: string) => {
    try {
      await navigator.clipboard.writeText(domain);
      setCopiedDomain(domain);
      setTimeout(() => setCopiedDomain(null), 2000);
    } catch (err) {
      console.error('Failed to copy:', err);
    }
  };

  const handleExportCSV = () => {
    if (results.length === 0) return;

    const headers = ['Domain', 'TLD', 'Available', 'WHOIS Server', 'Message'];
    const rows = filteredResults.map(r => [
      r.domain,
      r.tld,
      r.available ? 'Yes' : 'No',
      r.whoisServer,
      r.message || r.error || '',
    ]);

    const csv = [
      headers.join(','),
      ...rows.map(row => row.map(cell => `"${cell}"`).join(',')),
    ].join('\n');

    const blob = new Blob([csv], { type: 'text/csv' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = `domain-search-${Date.now()}.csv`;
    a.click();
    URL.revokeObjectURL(url);
  };

  const filteredResults = results.filter(result => {
    if (filter === 'all') return true;
    if (filter === 'available') return result.available;
    if (filter === 'unavailable') return !result.available;
    return true;
  });

  const availableCount = results.filter(r => r.available).length;
  const unavailableCount = results.filter(r => !r.available).length;

  return (
    <div className={`w-full max-w-6xl mx-auto p-6 ${className}`}>
      {/* Header */}
      <div className="mb-8">
        <h1 className="text-3xl font-bold mb-2">Domain Availability Search</h1>
        <p className="text-muted-foreground">
          Check domain availability across multiple TLDs using WHOIS servers
        </p>
      </div>

      {/* Search Input */}
      <div className="mb-6">
        <div className="flex flex-col gap-4">
          <div className="relative">
            <textarea
              className="w-full min-h-[120px] p-4 pr-12 border rounded-lg resize-y focus:outline-none focus:ring-2 focus:ring-primary"
              placeholder="Enter domain names (one per line or comma-separated)&#10;Example: example.com, test.org, mysite.net"
              value={searchInput}
              onChange={(e) => setSearchInput(e.target.value)}
              onKeyDown={(e) => {
                if (e.key === 'Enter' && e.ctrlKey) {
                  handleSearch();
                }
              }}
            />
            <Search className="absolute top-4 right-4 text-muted-foreground" size={20} />
          </div>

          <div className="flex items-center justify-between">
            <p className="text-sm text-muted-foreground">
              Tip: Press Ctrl+Enter to search • Max 10 domains per search
            </p>
            <Button
              onClick={handleSearch}
              disabled={loading || !searchInput.trim()}
            >
              {loading ? (
                <>
                  <Loader2 className="animate-spin mr-2" size={16} />
                  Searching...
                </>
              ) : (
                <>
                  <Search className="mr-2" size={16} />
                  Search Domains
                </>
              )}
            </Button>
          </div>
        </div>

        {/* Error Message */}
        {error && (
          <div className="mt-4 p-4 bg-destructive/10 border border-destructive rounded-lg">
            <p className="text-destructive text-sm">{error}</p>
          </div>
        )}
      </div>

      {/* Results */}
      {results.length > 0 && (
        <div className="space-y-4">
          {/* Results Header */}
          <div className="flex items-center justify-between">
            <div className="flex items-center gap-4">
              <h2 className="text-xl font-semibold">
                Results ({filteredResults.length})
              </h2>
              
              {/* Filter Buttons */}
              <div className="flex items-center gap-2">
                <Button
                  variant={filter === 'all' ? 'default' : 'outline'}
                  size="sm"
                  onClick={() => setFilter('all')}
                >
                  All ({results.length})
                </Button>
                <Button
                  variant={filter === 'available' ? 'default' : 'outline'}
                  size="sm"
                  onClick={() => setFilter('available')}
                >
                  Available ({availableCount})
                </Button>
                <Button
                  variant={filter === 'unavailable' ? 'default' : 'outline'}
                  size="sm"
                  onClick={() => setFilter('unavailable')}
                >
                  Registered ({unavailableCount})
                </Button>
              </div>
            </div>

            {/* Export Button */}
            <Button
              variant="outline"
              size="sm"
              onClick={handleExportCSV}
            >
              <Download className="mr-2" size={16} />
              Export CSV
            </Button>
          </div>

          {/* Results Table */}
          <div className="border rounded-lg overflow-hidden">
            <div className="overflow-x-auto">
              <table className="w-full">
                <thead className="bg-muted">
                  <tr>
                    <th className="text-left p-4 font-semibold">Domain</th>
                    <th className="text-left p-4 font-semibold">TLD</th>
                    <th className="text-left p-4 font-semibold">Status</th>
                    <th className="text-left p-4 font-semibold">WHOIS Server</th>
                    <th className="text-left p-4 font-semibold">Actions</th>
                  </tr>
                </thead>
                <tbody>
                  {filteredResults.map((result, index) => (
                    <tr
                      key={`${result.domain}-${index}`}
                      className="border-t hover:bg-muted/50 transition-colors"
                    >
                      <td className="p-4">
                        <span className="font-mono text-sm">{result.domain}</span>
                      </td>
                      <td className="p-4">
                        <span className="px-2 py-1 bg-muted rounded text-xs font-medium">
                          .{result.tld}
                        </span>
                      </td>
                      <td className="p-4">
                        <div className="flex items-center gap-2">
                          {result.error ? (
                            <>
                              <XCircle className="text-destructive" size={18} />
                              <span className="text-sm text-destructive">Error</span>
                            </>
                          ) : result.available ? (
                            <>
                              <CheckCircle2 className="text-green-600" size={18} />
                              <span className="text-sm text-green-600 font-medium">
                                Available
                              </span>
                            </>
                          ) : (
                            <>
                              <XCircle className="text-muted-foreground" size={18} />
                              <span className="text-sm text-muted-foreground">
                                Registered
                              </span>
                            </>
                          )}
                        </div>
                        {(result.message || result.error) && (
                          <p className="text-xs text-muted-foreground mt-1">
                            {result.message || result.error}
                          </p>
                        )}
                      </td>
                      <td className="p-4">
                        <span className="text-sm text-muted-foreground">
                          {result.whoisServer}
                        </span>
                      </td>
                      <td className="p-4">
                        <Button
                          variant="ghost"
                          size="sm"
                          onClick={() => handleCopy(result.domain)}
                          title="Copy domain name"
                        >
                          {copiedDomain === result.domain ? (
                            <>
                              <CheckCircle2 className="text-green-600" size={16} />
                            </>
                          ) : (
                            <>
                              <Copy size={16} />
                            </>
                          )}
                        </Button>
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          </div>

          {filteredResults.length === 0 && (
            <div className="text-center py-8 text-muted-foreground">
              No results match the selected filter
            </div>
          )}
        </div>
      )}

      {/* No Results Message */}
      {!loading && results.length === 0 && !error && searchInput && (
        <div className="text-center py-12 text-muted-foreground">
          <Search className="mx-auto mb-4" size={48} />
          <p>Enter domain names above and click Search to check availability</p>
        </div>
      )}
    </div>
  );
}
