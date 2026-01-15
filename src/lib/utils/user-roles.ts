/**
 * Utility functions for user roles and permissions
 */

interface User {
  id: number;
  email: string;
  first_name: string;
  last_name: string;
  mobile_number?: string;
  role?: string;
  is_staff?: boolean;
  is_superuser?: boolean;
}

/**
 * Check if user is a staff member (admin or staff role)
 */
export const isUserStaff = (user: User | null): boolean => {
  if (!user) return false;
  
  // Check multiple role patterns for compatibility
  return (
    user.role === 'admin' ||
    user.role === 'staff' ||
    user.is_staff === true ||
    user.is_superuser === true
  );
};

/**
 * Check if user has admin privileges
 */
export const isUserAdmin = (user: User | null): boolean => {
  if (!user) return false;
  
  return (
    user.role === 'admin' ||
    user.is_superuser === true
  );
};
