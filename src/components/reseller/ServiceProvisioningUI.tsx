'use client';

import React, { useState } from 'react';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import { Alert, AlertDescription } from '@/components/ui/alert';
import { CheckCircle2, Loader2 } from 'lucide-react';

export default function ServiceProvisioningUI() {
  const [loading, setLoading] = useState(false);
  const [success, setSuccess] = useState(false);
  const [selectedService, setSelectedService] = useState('');
  const [selectedClient, setSelectedClient] = useState('');

  const services = [
    { id: 'hosting', name: 'Web Hosting', price: '$9.99/mo' },
    { id: 'domain', name: 'Domain Registration', price: '$14.99/yr' },
    { id: 'webdev', name: 'Web Development', price: '$2,999' },
  ];

  const handleProvision = async () => {
    setLoading(true);
    // Simulate API call
    await new Promise(resolve => setTimeout(resolve, 2000));
    setLoading(false);
    setSuccess(true);
    setTimeout(() => setSuccess(false), 3000);
  };

  return (
    <Card>
      <CardHeader>
        <CardTitle>Provision Service</CardTitle>
        <CardDescription>Create new service for client instantly</CardDescription>
      </CardHeader>
      <CardContent className="space-y-4">
        {success && (
          <Alert>
            <CheckCircle2 className="h-4 w-4" />
            <AlertDescription>Service provisioned successfully!</AlertDescription>
          </Alert>
        )}
        
        <div className="space-y-2">
          <Label>Select Client</Label>
          <Select value={selectedClient} onValueChange={setSelectedClient}>
            <SelectTrigger>
              <SelectValue placeholder="Choose client" />
            </SelectTrigger>
            <SelectContent>
              <SelectItem value="client1">Client 1</SelectItem>
              <SelectItem value="client2">Client 2</SelectItem>
            </SelectContent>
          </Select>
        </div>

        <div className="space-y-2">
          <Label>Select Service</Label>
          <Select value={selectedService} onValueChange={setSelectedService}>
            <SelectTrigger>
              <SelectValue placeholder="Choose service" />
            </SelectTrigger>
            <SelectContent>
              {services.map(service => (
                <SelectItem key={service.id} value={service.id}>
                  {service.name} - {service.price}
                </SelectItem>
              ))}
            </SelectContent>
          </Select>
        </div>

        <div className="space-y-2">
          <Label>Domain (for hosting)</Label>
          <Input placeholder="example.com" />
        </div>

        <Button 
          onClick={handleProvision} 
          disabled={loading || !selectedService || !selectedClient}
          className="w-full"
        >
          {loading ? (
            <>
              <Loader2 className="mr-2 h-4 w-4 animate-spin" />
              Provisioning...
            </>
          ) : (
            'Provision Service'
          )}
        </Button>
      </CardContent>
    </Card>
  );
}
