'use client';

import { useState, useEffect } from 'react';
import { useRouter, useSearchParams } from 'next/navigation';
import Link from 'next/link';
import { useAuthStore } from '@/lib/stores/auth-store';
import { 
  FaEnvelope, FaLock, FaSpinner, FaEye, FaEyeSlash, FaUser, FaPhone, 
  FaCheck, FaBuilding, FaBriefcase, FaGift 
} from 'react-icons/fa';

interface FormData {
  email: string;
  password: string;
  confirmPassword: string;
  first_name: string;
  last_name: string;
  mobile_number: string;
  company_name: string;
  user_type: 'client' | 'staff';
  referral_code: string;
}

interface ValidationErrors {
  [key: string]: string;
}

const STEPS = [
  { id: 1, title: 'Personal Info', description: 'Your basic details' },
  { id: 2, title: 'Contact', description: 'How to reach you' },
  { id: 3, title: 'Security', description: 'Secure your account' },
  { id: 4, title: 'Additional', description: 'Optional information' },
];

export default function MultiStepSignupForm() {
  const router = useRouter();
  const searchParams = useSearchParams();
  const { register, isLoading } = useAuthStore();
  const [currentStep, setCurrentStep] = useState(1);
  const [referrerName, setReferrerName] = useState<string | null>(null);
  const [formData, setFormData] = useState<FormData>({
    email: '',
    password: '',
    confirmPassword: '',
    first_name: '',
    last_name: '',
    mobile_number: '',
    company_name: '',
    user_type: 'client',
    referral_code: '',
  });
  const [errors, setErrors] = useState<ValidationErrors>({});
  const [showPassword, setShowPassword] = useState(false);
  const [showConfirmPassword, setShowConfirmPassword] = useState(false);
  const [agreedToTerms, setAgreedToTerms] = useState(false);

  const validateReferralCode = async (code: string) => {
    try {
      const API_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:8000/api';
      const response = await fetch(`${API_URL}/referrals/referrals/validate_code/`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ referral_code: code })
      });
      
      if (response.ok) {
        const data = await response.json();
        setReferrerName(data.referrer_name);
      } else {
        setReferrerName(null);
      }
    } catch {
      setReferrerName(null);
    }
  };

  // Check for referral code in URL
  useEffect(() => {
    const refCode = searchParams.get('ref');
    if (refCode) {
      setFormData(prev => ({ ...prev, referral_code: refCode }));
      validateReferralCode(refCode);
    }
  }, [searchParams]);

  // Validation functions
  const validateEmail = (email: string): string => {
    if (!email) return 'Email is required';
    const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
    if (!emailRegex.test(email)) return 'Please enter a valid email address';
    return '';
  };

  const validatePassword = (password: string): string => {
    if (!password) return 'Password is required';
    if (password.length < 8) return 'Password must be at least 8 characters';
    if (!/[A-Z]/.test(password)) return 'Password must contain at least one uppercase letter';
    if (!/[a-z]/.test(password)) return 'Password must contain at least one lowercase letter';
    if (!/[0-9]/.test(password)) return 'Password must contain at least one number';
    if (!/[!@#$%^&*]/.test(password)) return 'Password must contain at least one special character (!@#$%^&*)';
    return '';
  };

  const validatePhone = (phone: string): string => {
    if (!phone) return 'Mobile number is required';
    const phoneRegex = /^\+?[0-9]{10,15}$/;
    if (!phoneRegex.test(phone.replace(/\s/g, ''))) return 'Please enter a valid phone number';
    return '';
  };

  const validateName = (name: string, fieldName: string): string => {
    if (!name) return `${fieldName} is required`;
    if (name.length < 2) return `${fieldName} must be at least 2 characters`;
    if (!/^[a-zA-Z\s'-]+$/.test(name)) return `${fieldName} can only contain letters, spaces, hyphens, and apostrophes`;
    return '';
  };

  // Validate current step
  const validateStep = (step: number): boolean => {
    const newErrors: ValidationErrors = {};

    if (step === 1) {
      const firstNameError = validateName(formData.first_name, 'First name');
      const lastNameError = validateName(formData.last_name, 'Last name');
      if (firstNameError) newErrors.first_name = firstNameError;
      if (lastNameError) newErrors.last_name = lastNameError;
    }

    if (step === 2) {
      const emailError = validateEmail(formData.email);
      const phoneError = validatePhone(formData.mobile_number);
      if (emailError) newErrors.email = emailError;
      if (phoneError) newErrors.mobile_number = phoneError;
    }

    if (step === 3) {
      const passwordError = validatePassword(formData.password);
      if (passwordError) newErrors.password = passwordError;
      if (formData.password !== formData.confirmPassword) {
        newErrors.confirmPassword = 'Passwords do not match';
      }
    }

    setErrors(newErrors);
    return Object.keys(newErrors).length === 0;
  };

  const handleChange = (e: React.ChangeEvent<HTMLInputElement | HTMLSelectElement>) => {
    const { name, value } = e.target;
    setFormData(prev => ({ ...prev, [name]: value }));
    
    // Clear error for this field
    if (errors[name]) {
      setErrors(prev => {
        const newErrors = { ...prev };
        delete newErrors[name];
        return newErrors;
      });
    }
  };

  const nextStep = () => {
    if (validateStep(currentStep)) {
      setCurrentStep(prev => Math.min(prev + 1, STEPS.length));
    }
  };

  const prevStep = () => {
    setCurrentStep(prev => Math.max(prev - 1, 1));
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();

    // Validate all steps
    for (let step = 1; step <= 4; step++) {
      if (!validateStep(step) && step !== 4) { // Step 4 is optional
        setCurrentStep(step);
        return;
      }
    }

    if (!agreedToTerms) {
      setErrors({ terms: 'You must agree to the terms and conditions' });
      return;
    }

    const result = await register({
      email: formData.email,
      password: formData.password,
      first_name: formData.first_name,
      last_name: formData.last_name,
      mobile_number: formData.mobile_number,
      company_name: formData.company_name,
      user_type: formData.user_type,
      referral_code: formData.referral_code || undefined,
    });

    if (result.success) {
      router.push('/dashboard');
    } else {
      setErrors({ submit: result.error || 'Registration failed. Please try again.' });
      setCurrentStep(1);
    }
  };

  const getPasswordStrength = (password: string): { strength: number; label: string; color: string } => {
    if (!password) return { strength: 0, label: '', color: '' };
    let strength = 0;
    if (password.length >= 8) strength++;
    if (/[A-Z]/.test(password)) strength++;
    if (/[a-z]/.test(password)) strength++;
    if (/[0-9]/.test(password)) strength++;
    if (/[!@#$%^&*]/.test(password)) strength++;

    if (strength <= 2) return { strength: strength * 20, label: 'Weak', color: 'bg-red-500' };
    if (strength <= 3) return { strength: strength * 20, label: 'Fair', color: 'bg-yellow-500' };
    if (strength <= 4) return { strength: strength * 20, label: 'Good', color: 'bg-blue-500' };
    return { strength: 100, label: 'Strong', color: 'bg-green-500' };
  };

  const passwordStrength = getPasswordStrength(formData.password);

  return (
    <div className="min-h-screen flex items-center justify-center bg-gradient-to-br from-blue-50 via-white to-blue-50 dark:from-gray-900 dark:via-gray-800 dark:to-gray-900 px-4 py-12">
      <div className="max-w-2xl w-full">
        {/* Referral Banner */}
        {referrerName && (
          <div className="mb-6 p-4 bg-gradient-to-r from-green-50 to-blue-50 dark:from-green-900/30 dark:to-blue-900/30 border border-green-200 dark:border-green-800 rounded-xl">
            <div className="flex items-center gap-3">
              <div className="p-2 bg-green-100 dark:bg-green-800 rounded-full">
                <FaGift className="text-green-600 dark:text-green-300 w-5 h-5" />
              </div>
              <div>
                <p className="font-semibold text-green-800 dark:text-green-200">
                  You were referred by {referrerName}!
                </p>
                <p className="text-sm text-green-700 dark:text-green-300">
                  Sign up to receive your welcome bonus
                </p>
              </div>
            </div>
          </div>
        )}

        {/* Progress Steps */}
        <div className="mb-8">
          <div className="flex justify-between items-center">
            {STEPS.map((step, index) => (
              <div key={step.id} className="flex-1 relative">
                {/* Step Circle */}
                <div className="flex flex-col items-center">
                  <div
                    className={`w-10 h-10 rounded-full flex items-center justify-center border-2 transition-all ${
                      currentStep > step.id
                        ? 'bg-green-500 border-green-500 text-white'
                        : currentStep === step.id
                        ? 'bg-blue-600 border-blue-600 text-white'
                        : 'bg-white dark:bg-gray-800 border-gray-300 dark:border-gray-600 text-gray-400'
                    }`}
                  >
                    {currentStep > step.id ? <FaCheck /> : step.id}
                  </div>
                  <div className="text-xs mt-2 text-center">
                    <div className={`font-medium ${currentStep >= step.id ? 'text-blue-600 dark:text-blue-400' : 'text-gray-400'}`}>
                      {step.title}
                    </div>
                  </div>
                </div>
                {/* Connecting Line */}
                {index < STEPS.length - 1 && (
                  <div
                    className={`absolute top-5 left-1/2 w-full h-0.5 -z-10 transition-all ${
                      currentStep > step.id ? 'bg-green-500' : 'bg-gray-300 dark:bg-gray-600'
                    }`}
                  />
                )}
              </div>
            ))}
          </div>
        </div>

        {/* Card */}
        <div className="bg-white dark:bg-gray-800 rounded-2xl shadow-xl p-8">
          {/* Header */}
          <div className="text-center mb-8">
            <h1 className="text-3xl font-bold text-gray-900 dark:text-white mb-2">
              {STEPS[currentStep - 1].title}
            </h1>
            <p className="text-gray-600 dark:text-gray-400">
              {STEPS[currentStep - 1].description}
            </p>
          </div>

          {/* Error Messages */}
          {Object.keys(errors).length > 0 && (
            <div className="mb-6 p-4 bg-red-50 dark:bg-red-900/20 border border-red-200 dark:border-red-800 rounded-lg">
              {Object.values(errors).map((error, index) => (
                <p key={index} className="text-red-800 dark:text-red-200 text-sm">{error}</p>
              ))}
            </div>
          )}

          {/* Form */}
          <form onSubmit={handleSubmit} className="space-y-6">
            {/* Step 1: Personal Info */}
            {currentStep === 1 && (
              <div className="space-y-4">
                <div className="grid grid-cols-2 gap-4">
                  <div>
                    <label htmlFor="first_name" className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                      First Name *
                    </label>
                    <div className="relative">
                      <div className="absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none">
                        <FaUser className="text-gray-400" />
                      </div>
                      <input
                        id="first_name"
                        name="first_name"
                        type="text"
                        value={formData.first_name}
                        onChange={handleChange}
                        className={`block w-full pl-10 pr-3 py-3 border rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent dark:bg-gray-700 dark:text-white ${
                          errors.first_name ? 'border-red-500' : 'border-gray-300 dark:border-gray-600'
                        }`}
                        placeholder="John"
                      />
                    </div>
                  </div>

                  <div>
                    <label htmlFor="last_name" className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                      Last Name *
                    </label>
                    <input
                      id="last_name"
                      name="last_name"
                      type="text"
                      value={formData.last_name}
                      onChange={handleChange}
                      className={`block w-full px-3 py-3 border rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent dark:bg-gray-700 dark:text-white ${
                        errors.last_name ? 'border-red-500' : 'border-gray-300 dark:border-gray-600'
                      }`}
                      placeholder="Doe"
                    />
                  </div>
                </div>
              </div>
            )}

            {/* Step 2: Contact */}
            {currentStep === 2 && (
              <div className="space-y-4">
                <div>
                  <label htmlFor="email" className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                    Email Address *
                  </label>
                  <div className="relative">
                    <div className="absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none">
                      <FaEnvelope className="text-gray-400" />
                    </div>
                    <input
                      id="email"
                      name="email"
                      type="email"
                      value={formData.email}
                      onChange={handleChange}
                      className={`block w-full pl-10 pr-3 py-3 border rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent dark:bg-gray-700 dark:text-white ${
                        errors.email ? 'border-red-500' : 'border-gray-300 dark:border-gray-600'
                      }`}
                      placeholder="you@example.com"
                    />
                  </div>
                </div>

                <div>
                  <label htmlFor="mobile_number" className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                    Mobile Number *
                  </label>
                  <div className="relative">
                    <div className="absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none">
                      <FaPhone className="text-gray-400" />
                    </div>
                    <input
                      id="mobile_number"
                      name="mobile_number"
                      type="tel"
                      value={formData.mobile_number}
                      onChange={handleChange}
                      className={`block w-full pl-10 pr-3 py-3 border rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent dark:bg-gray-700 dark:text-white ${
                        errors.mobile_number ? 'border-red-500' : 'border-gray-300 dark:border-gray-600'
                      }`}
                      placeholder="+263 78 721 1325"
                    />
                  </div>
                  <p className="mt-1 text-xs text-gray-500 dark:text-gray-400">
                    Used for WhatsApp notifications and support
                  </p>
                </div>
              </div>
            )}

            {/* Step 3: Security */}
            {currentStep === 3 && (
              <div className="space-y-4">
                <div>
                  <label htmlFor="password" className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                    Password *
                  </label>
                  <div className="relative">
                    <div className="absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none">
                      <FaLock className="text-gray-400" />
                    </div>
                    <input
                      id="password"
                      name="password"
                      type={showPassword ? 'text' : 'password'}
                      value={formData.password}
                      onChange={handleChange}
                      className={`block w-full pl-10 pr-10 py-3 border rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent dark:bg-gray-700 dark:text-white ${
                        errors.password ? 'border-red-500' : 'border-gray-300 dark:border-gray-600'
                      }`}
                      placeholder="••••••••"
                    />
                    <button
                      type="button"
                      onClick={() => setShowPassword(!showPassword)}
                      className="absolute inset-y-0 right-0 pr-3 flex items-center"
                    >
                      {showPassword ? (
                        <FaEyeSlash className="text-gray-400 hover:text-gray-600" />
                      ) : (
                        <FaEye className="text-gray-400 hover:text-gray-600" />
                      )}
                    </button>
                  </div>
                  {formData.password && (
                    <div className="mt-2">
                      <div className="flex justify-between items-center mb-1">
                        <span className="text-xs text-gray-600 dark:text-gray-400">Password Strength:</span>
                        <span className={`text-xs font-medium ${passwordStrength.strength >= 80 ? 'text-green-600' : passwordStrength.strength >= 60 ? 'text-blue-600' : passwordStrength.strength >= 40 ? 'text-yellow-600' : 'text-red-600'}`}>
                          {passwordStrength.label}
                        </span>
                      </div>
                      <div className="w-full bg-gray-200 dark:bg-gray-700 rounded-full h-2">
                        <div
                          className={`h-2 rounded-full transition-all ${passwordStrength.color}`}
                          style={{ width: `${passwordStrength.strength}%` }}
                        />
                      </div>
                      <ul className="mt-2 text-xs text-gray-600 dark:text-gray-400 space-y-1">
                        <li className="flex items-center gap-1">
                          <span className={formData.password.length >= 8 ? 'text-green-500' : 'text-gray-400'}>✓</span>
                          At least 8 characters
                        </li>
                        <li className="flex items-center gap-1">
                          <span className={/[A-Z]/.test(formData.password) ? 'text-green-500' : 'text-gray-400'}>✓</span>
                          One uppercase letter
                        </li>
                        <li className="flex items-center gap-1">
                          <span className={/[a-z]/.test(formData.password) ? 'text-green-500' : 'text-gray-400'}>✓</span>
                          One lowercase letter
                        </li>
                        <li className="flex items-center gap-1">
                          <span className={/[0-9]/.test(formData.password) ? 'text-green-500' : 'text-gray-400'}>✓</span>
                          One number
                        </li>
                        <li className="flex items-center gap-1">
                          <span className={/[!@#$%^&*]/.test(formData.password) ? 'text-green-500' : 'text-gray-400'}>✓</span>
                          One special character (!@#$%^&*)
                        </li>
                      </ul>
                    </div>
                  )}
                </div>

                <div>
                  <label htmlFor="confirmPassword" className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                    Confirm Password *
                  </label>
                  <div className="relative">
                    <div className="absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none">
                      <FaLock className="text-gray-400" />
                    </div>
                    <input
                      id="confirmPassword"
                      name="confirmPassword"
                      type={showConfirmPassword ? 'text' : 'password'}
                      value={formData.confirmPassword}
                      onChange={handleChange}
                      className={`block w-full pl-10 pr-10 py-3 border rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent dark:bg-gray-700 dark:text-white ${
                        errors.confirmPassword ? 'border-red-500' : 'border-gray-300 dark:border-gray-600'
                      }`}
                      placeholder="••••••••"
                    />
                    <button
                      type="button"
                      onClick={() => setShowConfirmPassword(!showConfirmPassword)}
                      className="absolute inset-y-0 right-0 pr-3 flex items-center"
                    >
                      {showConfirmPassword ? (
                        <FaEyeSlash className="text-gray-400 hover:text-gray-600" />
                      ) : (
                        <FaEye className="text-gray-400 hover:text-gray-600" />
                      )}
                    </button>
                  </div>
                </div>
              </div>
            )}

            {/* Step 4: Additional Info (Optional) */}
            {currentStep === 4 && (
              <div className="space-y-4">
                <div>
                  <label htmlFor="company_name" className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                    Company Name (Optional)
                  </label>
                  <div className="relative">
                    <div className="absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none">
                      <FaBuilding className="text-gray-400" />
                    </div>
                    <input
                      id="company_name"
                      name="company_name"
                      type="text"
                      value={formData.company_name}
                      onChange={handleChange}
                      className="block w-full pl-10 pr-3 py-3 border border-gray-300 dark:border-gray-600 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent dark:bg-gray-700 dark:text-white"
                      placeholder="Your Company Ltd"
                    />
                  </div>
                </div>

                <div>
                  <label htmlFor="user_type" className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                    Account Type
                  </label>
                  <div className="relative">
                    <div className="absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none">
                      <FaBriefcase className="text-gray-400" />
                    </div>
                    <select
                      id="user_type"
                      name="user_type"
                      value={formData.user_type}
                      onChange={handleChange}
                      className="block w-full pl-10 pr-3 py-3 border border-gray-300 dark:border-gray-600 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent dark:bg-gray-700 dark:text-white"
                    >
                      <option value="client">Client</option>
                      <option value="staff">Staff</option>
                    </select>
                  </div>
                </div>

                {/* Referral Code */}
                <div>
                  <label htmlFor="referral_code" className="block text-sm font-medium text-gray-700 dark:text-gray-300 mb-2">
                    Referral Code (Optional)
                  </label>
                  <div className="relative">
                    <div className="absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none">
                      <FaGift className="text-gray-400" />
                    </div>
                    <input
                      id="referral_code"
                      name="referral_code"
                      type="text"
                      value={formData.referral_code}
                      onChange={(e) => {
                        handleChange(e);
                        if (e.target.value.length === 8) {
                          validateReferralCode(e.target.value);
                        } else {
                          setReferrerName(null);
                        }
                      }}
                      className="block w-full pl-10 pr-3 py-3 border border-gray-300 dark:border-gray-600 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent dark:bg-gray-700 dark:text-white"
                      placeholder="ABCD1234"
                    />
                  </div>
                  {referrerName && (
                    <p className="mt-1 text-sm text-green-600 dark:text-green-400">
                      ✓ Referred by {referrerName}
                    </p>
                  )}
                  {formData.referral_code && !referrerName && formData.referral_code.length === 8 && (
                    <p className="mt-1 text-sm text-red-600 dark:text-red-400">
                      Invalid referral code
                    </p>
                  )}
                </div>

                {/* Terms Agreement */}
                <div className="flex items-start pt-4">
                  <input
                    id="terms"
                    type="checkbox"
                    checked={agreedToTerms}
                    onChange={(e) => setAgreedToTerms(e.target.checked)}
                    className="h-4 w-4 text-blue-600 focus:ring-blue-500 border-gray-300 rounded mt-1"
                  />
                  <label htmlFor="terms" className="ml-2 block text-sm text-gray-700 dark:text-gray-300">
                    I agree to the{' '}
                    <Link href="/terms-and-conditions" className="text-blue-600 hover:text-blue-700 dark:text-blue-400" target="_blank">
                      Terms and Conditions
                    </Link>
                    {' '}and{' '}
                    <Link href="/privacy-policy" className="text-blue-600 hover:text-blue-700 dark:text-blue-400" target="_blank">
                      Privacy Policy
                    </Link>
                  </label>
                </div>
              </div>
            )}

            {/* Navigation Buttons */}
            <div className="flex gap-4 pt-4">
              {currentStep > 1 && (
                <button
                  type="button"
                  onClick={prevStep}
                  className="flex-1 py-3 px-4 border border-gray-300 dark:border-gray-600 rounded-lg text-gray-700 dark:text-gray-300 hover:bg-gray-50 dark:hover:bg-gray-700 transition-all"
                >
                  Previous
                </button>
              )}
              
              {currentStep < STEPS.length ? (
                <button
                  type="button"
                  onClick={nextStep}
                  className="flex-1 py-3 px-4 bg-gradient-to-r from-blue-600 to-blue-700 hover:from-blue-700 hover:to-blue-800 text-white rounded-lg transition-all"
                >
                  Next
                </button>
              ) : (
                <button
                  type="submit"
                  disabled={isLoading || !agreedToTerms}
                  className="flex-1 flex items-center justify-center py-3 px-4 bg-gradient-to-r from-blue-600 to-blue-700 hover:from-blue-700 hover:to-blue-800 text-white rounded-lg disabled:opacity-50 disabled:cursor-not-allowed transition-all"
                >
                  {isLoading ? (
                    <>
                      <FaSpinner className="animate-spin mr-2" />
                      Creating account...
                    </>
                  ) : (
                    'Create Account'
                  )}
                </button>
              )}
            </div>
          </form>

          {/* Login Link */}
          <div className="mt-6 text-center">
            <p className="text-sm text-gray-600 dark:text-gray-400">
              Already have an account?{' '}
              <Link href="/login" className="font-medium text-blue-600 hover:text-blue-700 dark:text-blue-400">
                Sign in
              </Link>
            </p>
          </div>
        </div>

        {/* Back to Home */}
        <div className="mt-6 text-center">
          <Link href="/" className="text-sm text-gray-600 dark:text-gray-400 hover:text-gray-900 dark:hover:text-white">
            ← Back to home
          </Link>
        </div>
      </div>
    </div>
  );
}
