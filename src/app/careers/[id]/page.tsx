'use client';

import { useState, useEffect } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { FaArrowLeft, FaBriefcase, FaMapMarkerAlt, FaClock, FaDollarSign } from 'react-icons/fa';

interface JobPosting {
  id: number;
  title: string;
  description: string;
  employment_type: string;
  location: string;
  salary_range?: string;
  requirements: string;
  responsibilities: string;
  posted_date: string;
  deadline: string;
  is_active: boolean;
  application_count: number;
}

export default function JobDetailPage() {
  const params = useParams();
  const router = useRouter();
  const [job, setJob] = useState<JobPosting | null>(null);
  const [loading, setLoading] = useState(true);
  const [applying, setApplying] = useState(false);
  const [resumeUrl, setResumeUrl] = useState('');
  const [coverLetter, setCoverLetter] = useState('');
  const [message, setMessage] = useState('');
  const [isAuthenticated, setIsAuthenticated] = useState(false);

  useEffect(() => {
    checkAuth();
    fetchJob();
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [params.id]);

  const checkAuth = () => {
    if (typeof window !== 'undefined') {
      const token = localStorage.getItem('access_token');
      setIsAuthenticated(!!token);
    }
  };

  const fetchJob = async () => {
    try {
      const API_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:8000/api';
      const response = await fetch(`${API_URL}/jobs/postings/${params.id}/`);
      const data = await response.json();
      setJob(data);
    } catch {
      // Error fetching job
    } finally {
      setLoading(false);
    }
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    
    if (!isAuthenticated) {
      setMessage('Please login to apply for this position');
      router.push(`/login?redirect=/careers/${params.id}`);
      return;
    }

    setApplying(true);
    setMessage('');

    try {
      const API_URL = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:8000/api';
      const token = localStorage.getItem('access_token');
      
      const response = await fetch(`${API_URL}/jobs/applications/`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'Authorization': `Bearer ${token}`
        },
        body: JSON.stringify({
          job: params.id,
          resume_url: resumeUrl,
          cover_letter: coverLetter
        })
      });

      if (response.ok) {
        setMessage('Application submitted successfully! We will review your application and get back to you soon.');
        setResumeUrl('');
        setCoverLetter('');
      } else {
        const error = await response.json();
        setMessage(error.detail || 'Failed to submit application. Please try again.');
      }
    } catch {
      setMessage('An error occurred. Please try again later.');
    } finally {
      setApplying(false);
    }
  };

  if (loading) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <p className="text-gray-600 dark:text-gray-400">Loading job details...</p>
      </div>
    );
  }

  if (!job) {
    return (
      <div className="min-h-screen flex flex-col items-center justify-center">
        <p className="text-gray-600 dark:text-gray-400 mb-4">Job not found</p>
        <button
          onClick={() => router.push('/careers')}
          className="px-6 py-3 bg-blue-600 text-white rounded-lg"
        >
          Back to Careers
        </button>
      </div>
    );
  }

  return (
    <div className="min-h-screen py-28 px-4 sm:px-8 md:px-16 lg:px-24">
      <div className="max-w-4xl mx-auto">
        {/* Back Button */}
        <button
          onClick={() => router.push('/careers')}
          className="flex items-center gap-2 text-blue-600 dark:text-blue-400 hover:underline mb-8"
        >
          <FaArrowLeft /> Back to All Positions
        </button>

        {/* Job Header */}
        <div className="bg-white dark:bg-gray-900 rounded-2xl p-8 shadow-lg mb-8">
          <div className="flex flex-wrap items-center gap-4 mb-4">
            <h1 className="text-4xl font-bold text-blue-900 dark:text-blue-300">
              {job.title}
            </h1>
            <span className="px-4 py-2 bg-blue-100 dark:bg-blue-900 text-blue-900 dark:text-blue-300 rounded-full text-sm font-medium">
              {job.employment_type}
            </span>
          </div>

          <div className="flex flex-wrap gap-6 text-gray-600 dark:text-gray-400">
            <div className="flex items-center gap-2">
              <FaMapMarkerAlt className="text-darkgoldenrod dark:text-yellow-400" />
              {job.location}
            </div>
            {job.salary_range && (
              <div className="flex items-center gap-2">
                <FaDollarSign className="text-darkgoldenrod dark:text-yellow-400" />
                {job.salary_range}
              </div>
            )}
            <div className="flex items-center gap-2">
              <FaClock className="text-darkgoldenrod dark:text-yellow-400" />
              Deadline: {new Date(job.deadline).toLocaleDateString()}
            </div>
            <div className="flex items-center gap-2">
              <FaBriefcase className="text-darkgoldenrod dark:text-yellow-400" />
              {job.application_count} applicants
            </div>
          </div>
        </div>

        {/* Job Details */}
        <div className="bg-white dark:bg-gray-900 rounded-2xl p-8 shadow-lg mb-8">
          <h2 className="text-2xl font-bold text-blue-900 dark:text-blue-300 mb-4">
            Job Description
          </h2>
          <p className="text-gray-700 dark:text-gray-300 whitespace-pre-line mb-8">
            {job.description}
          </p>

          <h2 className="text-2xl font-bold text-blue-900 dark:text-blue-300 mb-4">
            Responsibilities
          </h2>
          <p className="text-gray-700 dark:text-gray-300 whitespace-pre-line mb-8">
            {job.responsibilities}
          </p>

          <h2 className="text-2xl font-bold text-blue-900 dark:text-blue-300 mb-4">
            Requirements
          </h2>
          <p className="text-gray-700 dark:text-gray-300 whitespace-pre-line">
            {job.requirements}
          </p>
        </div>

        {/* Application Form */}
        <div className="bg-white dark:bg-gray-900 rounded-2xl p-8 shadow-lg">
          <h2 className="text-2xl font-bold text-blue-900 dark:text-blue-300 mb-6">
            Apply for this Position
          </h2>

          {message && (
            <div className={`p-4 rounded-lg mb-6 ${
              message.includes('success') 
                ? 'bg-green-100 dark:bg-green-900 text-green-900 dark:text-green-100'
                : 'bg-red-100 dark:bg-red-900 text-red-900 dark:text-red-100'
            }`}>
              {message}
            </div>
          )}

          <form onSubmit={handleSubmit} className="space-y-6">
            <div>
              <label className="block text-gray-700 dark:text-gray-300 mb-2 font-medium">
                Resume URL *
              </label>
              <input
                type="url"
                value={resumeUrl}
                onChange={(e) => setResumeUrl(e.target.value)}
                required
                placeholder="https://example.com/your-resume.pdf"
                className="w-full px-4 py-3 rounded-lg border border-gray-300 dark:border-gray-700 bg-white dark:bg-gray-800 text-gray-900 dark:text-gray-100"
              />
              <p className="text-sm text-gray-500 dark:text-gray-500 mt-1">
                Upload your resume to Google Drive, Dropbox, or similar and paste the public link
              </p>
            </div>

            <div>
              <label className="block text-gray-700 dark:text-gray-300 mb-2 font-medium">
                Cover Letter *
              </label>
              <textarea
                value={coverLetter}
                onChange={(e) => setCoverLetter(e.target.value)}
                required
                rows={8}
                placeholder="Tell us why you're interested in this position and what makes you a great fit..."
                className="w-full px-4 py-3 rounded-lg border border-gray-300 dark:border-gray-700 bg-white dark:bg-gray-800 text-gray-900 dark:text-gray-100"
              />
            </div>

            <button
              type="submit"
              disabled={applying}
              className="w-full px-6 py-4 bg-blue-600 hover:bg-blue-700 dark:bg-blue-700 dark:hover:bg-blue-600 text-white rounded-lg font-semibold transition-colors disabled:opacity-50 disabled:cursor-not-allowed"
            >
              {applying ? 'Submitting...' : 'Submit Application'}
            </button>

            {!isAuthenticated && (
              <p className="text-center text-gray-600 dark:text-gray-400">
                You need to <a href="/login" className="text-blue-600 dark:text-blue-400 hover:underline">login</a> or <a href="/signup" className="text-blue-600 dark:text-blue-400 hover:underline">create an account</a> to apply
              </p>
            )}
          </form>
        </div>
      </div>
    </div>
  );
}
