'use client';

import React, { useState } from 'react';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Badge } from '@/components/ui/badge';
import { Copy, Eye, EyeOff, RefreshCw } from 'lucide-react';

export default function APIManagementConsole() {
  const [apiKey] = useState('rsk_live_1234567890abcdef');
  const [apiSecret] = useState('rss_secretkey1234567890');
  const [showSecret, setShowSecret] = useState(false);
  const [copied, setCopied] = useState(false);

  const copyToClipboard = (text: string) => {
    navigator.clipboard.writeText(text);
    setCopied(true);
    setTimeout(() => setCopied(false), 2000);
  };

  const stats = [
    { label: 'API Calls Today', value: '1,234' },
    { label: 'Rate Limit', value: '5,000/hr' },
    { label: 'Success Rate', value: '99.8%' },
  ];

  return (
    <div className="space-y-6">
      <Card>
        <CardHeader>
          <CardTitle>API Credentials</CardTitle>
          <CardDescription>Use these credentials for external integrations</CardDescription>
        </CardHeader>
        <CardContent className="space-y-4">
          <div className="space-y-2">
            <label className="text-sm font-medium">API Key</label>
            <div className="flex gap-2">
              <Input value={apiKey} readOnly />
              <Button 
                variant="outline" 
                size="icon"
                onClick={() => copyToClipboard(apiKey)}
              >
                <Copy className="h-4 w-4" />
              </Button>
            </div>
          </div>

          <div className="space-y-2">
            <label className="text-sm font-medium">API Secret</label>
            <div className="flex gap-2">
              <Input 
                type={showSecret ? 'text' : 'password'}
                value={apiSecret} 
                readOnly 
              />
              <Button 
                variant="outline" 
                size="icon"
                onClick={() => setShowSecret(!showSecret)}
              >
                {showSecret ? <EyeOff className="h-4 w-4" /> : <Eye className="h-4 w-4" />}
              </Button>
              <Button 
                variant="outline" 
                size="icon"
                onClick={() => copyToClipboard(apiSecret)}
              >
                <Copy className="h-4 w-4" />
              </Button>
            </div>
          </div>

          {copied && (
            <p className="text-sm text-green-600">Copied to clipboard!</p>
          )}

          <Button variant="destructive" className="w-full">
            <RefreshCw className="mr-2 h-4 w-4" />
            Regenerate API Keys
          </Button>
        </CardContent>
      </Card>

      <Card>
        <CardHeader>
          <CardTitle>API Usage Statistics</CardTitle>
          <CardDescription>Monitor your API performance</CardDescription>
        </CardHeader>
        <CardContent>
          <div className="grid grid-cols-3 gap-4">
            {stats.map(stat => (
              <div key={stat.label} className="text-center p-4 border rounded-lg">
                <p className="text-2xl font-bold">{stat.value}</p>
                <p className="text-sm text-muted-foreground">{stat.label}</p>
              </div>
            ))}
          </div>
        </CardContent>
      </Card>

      <Card>
        <CardHeader>
          <CardTitle>API Documentation</CardTitle>
          <CardDescription>Quick reference for endpoints</CardDescription>
        </CardHeader>
        <CardContent>
          <div className="space-y-2">
            <div className="flex items-center justify-between p-3 border rounded-lg">
              <div>
                <Badge className="mb-1">POST</Badge>
                <p className="text-sm">/api/reseller/provision_service</p>
              </div>
              <Button variant="outline" size="sm">View Docs</Button>
            </div>
            <div className="flex items-center justify-between p-3 border rounded-lg">
              <div>
                <Badge variant="secondary" className="mb-1">GET</Badge>
                <p className="text-sm">/api/reseller/clients</p>
              </div>
              <Button variant="outline" size="sm">View Docs</Button>
            </div>
          </div>
        </CardContent>
      </Card>
    </div>
  );
}
