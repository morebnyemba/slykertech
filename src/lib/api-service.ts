/**
 * API Service for Backend Integration
 * Centralized API communication for all backend endpoints
 */

const API_BASE_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:8000/api';

interface ApiResponse<T = unknown> {
  data?: T;
  error?: string;
  status: number;
}

class ApiService {
  private baseUrl: string;
  private token: string | null = null;

  constructor(baseUrl: string = API_BASE_URL) {
    this.baseUrl = baseUrl;
    if (typeof window !== 'undefined') {
      this.token = localStorage.getItem('access_token');
    }
  }

  private getHeaders(): HeadersInit {
    const headers: HeadersInit = {
      'Content-Type': 'application/json',
    };
    
    if (this.token) {
      headers['Authorization'] = `Bearer ${this.token}`;
    }
    
    return headers;
  }

  private async request<T>(
    endpoint: string,
    options: RequestInit = {}
  ): Promise<ApiResponse<T>> {
    try {
      const response = await fetch(`${this.baseUrl}${endpoint}`, {
        ...options,
        credentials: 'include', // Include cookies for CORS
        headers: {
          ...this.getHeaders(),
          ...options.headers,
        },
      });

      const data = await response.json().catch(() => null);

      if (!response.ok) {
        return {
          error: data?.detail || data?.error || 'An error occurred',
          status: response.status,
        };
      }

      return {
        data,
        status: response.status,
      };
    } catch (error) {
      return {
        error: error instanceof Error ? error.message : 'Network error',
        status: 500,
      };
    }
  }

  // Authentication
  async login(email: string, password: string) {
    return this.request('/token/', {
      method: 'POST',
      body: JSON.stringify({ email, password }),
    });
  }

  async register(userData: {
    email: string;
    password: string;
    first_name: string;
    last_name: string;
    mobile_number: string;
  }) {
    return this.request('/accounts/register/', {
      method: 'POST',
      body: JSON.stringify(userData),
    });
  }

  setToken(token: string) {
    this.token = token;
    if (typeof window !== 'undefined') {
      localStorage.setItem('access_token', token);
    }
  }

  clearToken() {
    this.token = null;
    if (typeof window !== 'undefined') {
      localStorage.removeItem('access_token');
    }
  }

  // Services
  async getServices() {
    return this.request('/services/services/');
  }

  async getService(id: number) {
    return this.request(`/services/services/${id}/`);
  }

  // Hosting Products
  async getHostingProducts() {
    return this.request('/services/hosting-products/');
  }

  async getHostingProduct(id: number) {
    return this.request(`/services/hosting-products/${id}/`);
  }

  // Domain Products
  async getDomainProducts() {
    return this.request('/services/domain-products/');
  }

  async getDomainProduct(id: number) {
    return this.request(`/services/domain-products/${id}/`);
  }

  // Service Addons
  async getServiceAddons() {
    return this.request('/services/service-addons/');
  }

  async getServiceAddon(id: number) {
    return this.request(`/services/service-addons/${id}/`);
  }

  // Domain Registrations
  async getDomainRegistrations() {
    return this.request('/services/domain-registrations/');
  }

  async getDomainRegistration(id: number) {
    return this.request(`/services/domain-registrations/${id}/`);
  }

  // Subscriptions
  async getSubscriptions() {
    return this.request('/services/subscriptions/');
  }

  async createSubscription(data: Record<string, unknown>) {
    return this.request('/services/subscriptions/', {
      method: 'POST',
      body: JSON.stringify(data),
    });
  }

  // Invoices
  async getInvoices() {
    return this.request('/billing/invoices/');
  }

  async getInvoice(id: number) {
    return this.request(`/billing/invoices/${id}/`);
  }

  async payInvoiceWithPaynow(invoiceId: number, paymentData: Record<string, unknown>) {
    return this.request(`/billing/invoices/${invoiceId}/paynow_payment/`, {
      method: 'POST',
      body: JSON.stringify(paymentData),
    });
  }

  async expressCheckout(invoiceId: number, checkoutData: Record<string, unknown>) {
    return this.request(`/billing/invoices/${invoiceId}/express_checkout/`, {
      method: 'POST',
      body: JSON.stringify(checkoutData),
    });
  }

  // Projects
  async getProjects() {
    return this.request('/services/projects/');
  }

  async getProject(id: number) {
    return this.request(`/services/projects/${id}/`);
  }

  async updateProjectProgress(id: number, progress: number) {
    return this.request(`/services/projects/${id}/update_progress/`, {
      method: 'POST',
      body: JSON.stringify({ progress }),
    });
  }

  // Notifications
  async getNotifications() {
    return this.request('/notifications/notifications/');
  }

  async getUnreadCount() {
    return this.request('/notifications/notifications/unread_count/');
  }

  async markNotificationRead(id: number) {
    return this.request(`/notifications/notifications/${id}/mark_read/`, {
      method: 'POST',
    });
  }

  async markAllNotificationsRead() {
    return this.request('/notifications/notifications/mark_all_read/', {
      method: 'POST',
    });
  }

  // DNS Records
  async getDNSRecords() {
    return this.request('/services/dns-records/');
  }

  async createDNSRecord(data: Record<string, unknown>) {
    return this.request('/services/dns-records/', {
      method: 'POST',
      body: JSON.stringify(data),
    });
  }

  async updateDNSRecord(id: number, data: Record<string, unknown>) {
    return this.request(`/services/dns-records/${id}/`, {
      method: 'PUT',
      body: JSON.stringify(data),
    });
  }

  async deleteDNSRecord(id: number) {
    return this.request(`/services/dns-records/${id}/`, {
      method: 'DELETE',
    });
  }

  // Integrations
  async getCPanelAccounts() {
    return this.request('/integrations/cpanel/');
  }

  async getDirectAdminAccounts() {
    return this.request('/integrations/directadmin/');
  }

  async getCloudflareIntegrations() {
    return this.request('/integrations/cloudflare/');
  }

  async getNamecheapDomains() {
    return this.request('/integrations/namecheap/');
  }

  // Wallet
  async getWallets() {
    return this.request('/wallet/wallets/');
  }

  async getWallet(id: number) {
    return this.request(`/wallet/wallets/${id}/`);
  }

  async getWalletTransactions(walletId?: number) {
    const endpoint = walletId 
      ? `/wallet/transactions/?wallet=${walletId}`
      : '/wallet/transactions/';
    return this.request(endpoint);
  }

  async createWalletTransaction(data: Record<string, unknown>) {
    return this.request('/wallet/transactions/', {
      method: 'POST',
      body: JSON.stringify(data),
    });
  }

  // Payments
  async getPayments() {
    return this.request('/billing/payments/');
  }

  async getPayment(id: number) {
    return this.request(`/billing/payments/${id}/`);
  }

  async createPayment(data: Record<string, unknown>) {
    return this.request('/billing/payments/', {
      method: 'POST',
      body: JSON.stringify(data),
    });
  }

  // Billing Profiles
  async getBillingProfiles() {
    return this.request('/billing/billing-profiles/');
  }

  async getBillingProfile(id: number) {
    return this.request(`/billing/billing-profiles/${id}/`);
  }

  async updateBillingProfile(id: number, data: Record<string, unknown>) {
    return this.request(`/billing/billing-profiles/${id}/`, {
      method: 'PUT',
      body: JSON.stringify(data),
    });
  }

  // Project Milestones
  async getProjectMilestones(projectId?: number) {
    const endpoint = projectId 
      ? `/services/milestones/?project=${projectId}`
      : '/services/milestones/';
    return this.request(endpoint);
  }

  async createProjectMilestone(data: Record<string, unknown>) {
    return this.request('/services/milestones/', {
      method: 'POST',
      body: JSON.stringify(data),
    });
  }

  // Project Tasks
  async getProjectTasks(projectId?: number) {
    const endpoint = projectId 
      ? `/services/tasks/?project=${projectId}`
      : '/services/tasks/';
    return this.request(endpoint);
  }

  async updateProjectTask(id: number, data: Record<string, unknown>) {
    return this.request(`/services/tasks/${id}/`, {
      method: 'PATCH',
      body: JSON.stringify(data),
    });
  }

  // User Profile
  async getUserProfile() {
    return this.request('/accounts/users/me/');
  }

  async updateUserProfile(data: Record<string, unknown>) {
    return this.request('/accounts/users/me/', {
      method: 'PATCH',
      body: JSON.stringify(data),
    });
  }

  // Notification Preferences
  async getNotificationPreferences() {
    return this.request('/notifications/preferences/');
  }

  async updateNotificationPreferences(id: number, data: Record<string, unknown>) {
    return this.request(`/notifications/preferences/${id}/`, {
      method: 'PATCH',
      body: JSON.stringify(data),
    });
  }

  // Public endpoints (no auth required)
  async getPublicServices() {
    // Temporarily clear token for public request
    const tempToken = this.token;
    this.token = null;
    const result = await this.request('/services/services/');
    this.token = tempToken;
    return result;
  }

  async getPublicStats() {
    // Mock endpoint - would need to be implemented in backend
    return {
      data: {
        activeClients: 5,
        uptime: 99.99,
        engineers: 1,
        yearsExperience: 4
      },
      status: 200
    };
  }

  // ============ Admin Endpoints ============

  // Provisioning Failures
  async getProvisioningFailures(status?: string) {
    const params = status ? `?status=${status}` : '';
    return this.request(`/services/provisioning-failures/${params}`);
  }

  async getProvisioningFailure(id: number) {
    return this.request(`/services/provisioning-failures/${id}/`);
  }

  async getPendingFailuresCount() {
    return this.request('/services/provisioning-failures/pending_count/');
  }

  async updateProvisioningFailure(id: number, data: Record<string, unknown>) {
    return this.request(`/services/provisioning-failures/${id}/`, {
      method: 'PATCH',
      body: JSON.stringify(data),
    });
  }

  async markFailureResolved(id: number, notes?: string) {
    return this.request(`/services/provisioning-failures/${id}/mark_resolved/`, {
      method: 'POST',
      body: JSON.stringify({ notes: notes || '' }),
    });
  }

  async dismissFailure(id: number, notes?: string) {
    return this.request(`/services/provisioning-failures/${id}/dismiss/`, {
      method: 'POST',
      body: JSON.stringify({ notes: notes || '' }),
    });
  }

  async retryProvisioning(id: number) {
    return this.request(`/services/provisioning-failures/${id}/retry_provisioning/`, {
      method: 'POST',
    });
  }

  async completeManualProvisioning(id: number, data: {
    notes?: string;
    provisioned_username?: string;
    provisioned_domain?: string;
    provisioned_password?: string;
    additional_data?: Record<string, unknown>;
  }) {
    return this.request(`/services/provisioning-failures/${id}/complete_manual_provisioning/`, {
      method: 'POST',
      body: JSON.stringify(data),
    });
  }

  // All Subscriptions (admin)
  async getAllSubscriptions() {
    return this.request('/services/subscriptions/');
  }

  // All Clients (admin)
  async getAllClients() {
    return this.request('/clients/');
  }

  // ============ Tickets Endpoints ============

  async getTickets(params?: { status?: string; priority?: string; assigned_to_me?: boolean }) {
    let url = '/tickets/tickets/';
    const queryParams = new URLSearchParams();
    if (params?.status) queryParams.append('status', params.status);
    if (params?.priority) queryParams.append('priority', params.priority);
    if (params?.assigned_to_me) queryParams.append('assigned_to_me', 'true');
    if (queryParams.toString()) url += `?${queryParams.toString()}`;
    return this.request(url);
  }

  async getTicket(id: number) {
    return this.request(`/tickets/tickets/${id}/`);
  }

  async createTicket(data: { subject: string; description: string; priority?: string; department?: string }) {
    return this.request('/tickets/tickets/', {
      method: 'POST',
      body: JSON.stringify(data),
    });
  }

  async updateTicket(id: number, data: { status?: string; priority?: string; assigned_to_id?: number }) {
    return this.request(`/tickets/tickets/${id}/`, {
      method: 'PATCH',
      body: JSON.stringify(data),
    });
  }

  async getTicketStats() {
    return this.request('/tickets/tickets/stats/');
  }

  async assignTicketToMe(id: number) {
    return this.request(`/tickets/tickets/${id}/assign_to_me/`, {
      method: 'POST',
    });
  }

  async closeTicket(id: number) {
    return this.request(`/tickets/tickets/${id}/close/`, {
      method: 'POST',
    });
  }

  async reopenTicket(id: number) {
    return this.request(`/tickets/tickets/${id}/reopen/`, {
      method: 'POST',
    });
  }

  async replyToTicket(ticketId: number, message: string, isInternal?: boolean) {
    return this.request('/tickets/replies/', {
      method: 'POST',
      body: JSON.stringify({ ticket: ticketId, message, is_internal: isInternal || false }),
    });
  }

  // ============ Live Chat Endpoints ============

  async getChatSessions(status?: string) {
    const url = status ? `/livechat/sessions/?status=${status}` : '/livechat/sessions/';
    return this.request(url);
  }

  async getActiveChatSessions() {
    return this.request('/livechat/sessions/active/');
  }

  async getChatSession(id: number) {
    return this.request(`/livechat/sessions/${id}/`);
  }

  async getChatStats() {
    return this.request('/livechat/sessions/stats/');
  }

  async closeChatSession(id: number) {
    return this.request(`/livechat/sessions/${id}/close_session/`, {
      method: 'POST',
    });
  }

  async sendChatMessage(sessionId: number, message: string) {
    return this.request(`/livechat/sessions/${sessionId}/send_message/`, {
      method: 'POST',
      body: JSON.stringify({ message }),
    });
  }

  async getChatMessages(sessionId: number) {
    return this.request(`/livechat/messages/?session=${sessionId}`);
  }

  // ============ Analytics Endpoints ============

  async getAdminAnalytics() {
    // Aggregate data from various endpoints
    return {
      async fetch(apiService: ApiService) {
        const [ticketStats, chatStats, subscriptions, clients, failures] = await Promise.all([
          apiService.getTicketStats(),
          apiService.getChatStats(),
          apiService.getAllSubscriptions(),
          apiService.getAllClients(),
          apiService.getPendingFailuresCount(),
        ]);
        return {
          tickets: ticketStats.data,
          chat: chatStats.data,
          subscriptions: Array.isArray(subscriptions.data) ? subscriptions.data.length : 0,
          clients: Array.isArray(clients.data) ? clients.data.length : 0,
          pendingFailures: (failures.data as { pending_count: number })?.pending_count || 0,
        };
      }
    };
  }
}

// Export singleton instance
export const apiService = new ApiService();

// Export class for creating new instances if needed
export default ApiService;
