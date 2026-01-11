'use client';

import React, { useState } from 'react';

interface Ticket {
  id: number;
  subject: string;
  status: 'open' | 'in_progress' | 'resolved' | 'closed';
  priority: 'low' | 'medium' | 'high' | 'critical';
  created_at: string;
}

export default function TicketingInterface() {
  const [tickets] = useState<Ticket[]>([
    {
      id: 1,
      subject: 'Cannot access cPanel',
      status: 'open',
      priority: 'high',
      created_at: '2024-01-11T10:30:00Z'
    }
  ]);

  return (
    <div className="space-y-6">
      <div className="p-6 bg-white dark:bg-gray-800 rounded-lg shadow">
        <h2 className="text-2xl font-bold mb-4">Support Tickets</h2>
        <div className="space-y-4">
          {tickets.map(ticket => (
            <div key={ticket.id} className="flex items-center justify-between p-4 border rounded-lg">
              <div>
                <p className="font-medium">{ticket.subject}</p>
                <p className="text-sm text-gray-600">Ticket #{ticket.id}</p>
              </div>
              <div className="flex gap-2">
                <span className="px-2 py-1 text-xs rounded bg-red-100 text-red-800">{ticket.priority}</span>
                <span className="px-2 py-1 text-xs rounded bg-blue-100 text-blue-800">{ticket.status}</span>
              </div>
            </div>
          ))}
        </div>
      </div>
    </div>
  );
}
