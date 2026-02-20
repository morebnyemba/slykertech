'use client';

import React from 'react';

type BackgroundVariant = 'dots' | 'grid' | 'waves' | 'gradient-accent' | 'radial-glow';

interface SectionBackgroundProps {
    variant: BackgroundVariant;
    className?: string;
}

export default function SectionBackground({ variant, className = '' }: SectionBackgroundProps) {
    switch (variant) {
        case 'dots':
            return (
                <div className={`absolute inset-0 overflow-hidden pointer-events-none ${className}`}>
                    <div className="absolute inset-0 opacity-30 dark:opacity-15">
                        <svg width="100%" height="100%">
                            <pattern id="bg-dots" x="0" y="0" width="40" height="40" patternUnits="userSpaceOnUse">
                                <circle cx="20" cy="20" r="1" fill="currentColor" className="text-blue-400 dark:text-blue-500" />
                            </pattern>
                            <rect width="100%" height="100%" fill="url(#bg-dots)" />
                        </svg>
                    </div>
                </div>
            );

        case 'grid':
            return (
                <div className={`absolute inset-0 overflow-hidden pointer-events-none ${className}`}>
                    <div className="absolute inset-0 opacity-10 dark:opacity-5">
                        <svg width="100%" height="100%">
                            <pattern id="bg-grid" x="0" y="0" width="80" height="80" patternUnits="userSpaceOnUse">
                                <path d="M80 0L0 0L0 80" fill="none" stroke="currentColor" strokeWidth="0.5" className="text-gray-400 dark:text-gray-600" />
                            </pattern>
                            <rect width="100%" height="100%" fill="url(#bg-grid)" />
                        </svg>
                    </div>
                    <div className="absolute inset-0 bg-gradient-to-tr from-blue-500/5 via-transparent to-purple-500/5" />
                </div>
            );

        case 'waves':
            return (
                <div className={`absolute inset-0 overflow-hidden pointer-events-none ${className}`}>
                    <div className="absolute inset-0 opacity-20 dark:opacity-10">
                        <svg width="100%" height="100%">
                            <pattern id="bg-waves" x="0" y="0" width="200" height="200" patternUnits="userSpaceOnUse">
                                <path d="M0 100 Q50 50, 100 100 T200 100" fill="none" stroke="currentColor" strokeWidth="1" className="text-blue-400 dark:text-blue-600" />
                                <path d="M0 120 Q50 70, 100 120 T200 120" fill="none" stroke="currentColor" strokeWidth="1" className="text-indigo-400 dark:text-indigo-600" />
                                <path d="M0 80 Q50 30, 100 80 T200 80" fill="none" stroke="currentColor" strokeWidth="1" className="text-purple-400 dark:text-purple-600" />
                            </pattern>
                            <rect width="100%" height="100%" fill="url(#bg-waves)" />
                        </svg>
                    </div>
                </div>
            );

        case 'gradient-accent':
            return (
                <div className={`absolute inset-0 overflow-hidden pointer-events-none ${className}`}>
                    <div className="absolute left-0 top-0 h-1 w-full bg-gradient-to-r from-blue-400 via-indigo-500 to-purple-600 dark:from-blue-500 dark:via-indigo-600 dark:to-purple-700" />
                    <div className="absolute inset-0 bg-gradient-to-tr from-blue-500/5 via-transparent to-purple-500/5" />
                </div>
            );

        case 'radial-glow':
            return (
                <div className={`absolute inset-0 overflow-hidden pointer-events-none ${className}`}>
                    <div className="absolute inset-0 bg-[radial-gradient(circle_at_30%_20%,rgba(59,130,246,0.08),transparent_50%)] dark:bg-[radial-gradient(circle_at_30%_20%,rgba(59,130,246,0.12),transparent_50%)]" />
                    <div className="absolute inset-0 bg-[radial-gradient(circle_at_70%_80%,rgba(124,58,237,0.06),transparent_50%)] dark:bg-[radial-gradient(circle_at_70%_80%,rgba(124,58,237,0.1),transparent_50%)]" />
                </div>
            );

        default:
            return null;
    }
}
