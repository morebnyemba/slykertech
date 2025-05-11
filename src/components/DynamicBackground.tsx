'use client';

import { useEffect, useState } from 'react';

export default function DynamicBackground() {
  const [isMounted, setIsMounted] = useState(false);
  const [circles, setCircles] = useState([]);

  useEffect(() => {
    setIsMounted(true);
    // Generate the circle data only on the client
    setCircles(Array.from({ length: 12 }).map((_, i) => ({
      id: i,
      size: Math.random() * 10 + 5,
      left: Math.random() * 100,
      top: Math.random() * 100,
      duration: Math.random() * 6 + 4,
      delay: Math.random() * 5,
    })));
  }, []);

  if (!isMounted) {
    // Render a consistent placeholder on the server
    return <div className="fixed inset-0 -z-40 overflow-hidden pointer-events-none" />;
  }

  return (
    <div className="fixed inset-0 -z-40 overflow-hidden pointer-events-none">
      {circles.map((circle) => (
        <div
          key={circle.id}
          className="absolute rounded-full bg-blue-400 dark:bg-blue-500 opacity-20 animate-pulse"
          style={{
            width: `${circle.size}px`,
            height: `${circle.size}px`,
            left: `${circle.left}%`,
            top: `${circle.top}%`,
            animationDuration: `${circle.duration}s`,
            animationDelay: `${circle.delay}s`,
          }}
        />
      ))}
    </div>
  );
}