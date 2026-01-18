// app/contact/page.tsx
'use client';
import { FaWhatsapp, FaEnvelope, FaPhone, FaMapMarker } from 'react-icons/fa';
import { MdAccessTime } from 'react-icons/md';

// Metadata is defined in route segment layout.tsx to avoid exporting from a Client Component

export default function ContactPage() {
  const whatsappMessage = encodeURIComponent(
    "Hi Slyker Tech Web Services! I'd like to get in touch about..."
  );

  return (
    <div className="relative z-10">
      {/* Hero Section */}
      <section className="py-28 px-4 sm:px-8 md:px-16 lg:px-24 text-center bg-gradient-to-b from-blue-50 to-transparent dark:from-blue-950/50">
        <div className="max-w-5xl mx-auto">
          <h1 className="text-5xl sm:text-6xl md:text-7xl font-extrabold tracking-tight text-blue-900 dark:text-blue-300 leading-tight">
            Get in <span className="text-darkgoldenrod dark:text-yellow-400">Touch</span>
          </h1>
          <p className="mt-8 text-lg sm:text-xl text-gray-700 dark:text-gray-300 max-w-3xl mx-auto">
            Reach out to us directly via WhatsApp for instant support or use alternative methods below
          </p>
        </div>
      </section>

      {/* Contact Methods */}
      <section className="py-24 px-4 sm:px-8 bg-white dark:bg-gray-950">
        <div className="max-w-6xl mx-auto grid grid-cols-1 md:grid-cols-2 gap-12">
          {/* Primary WhatsApp Card */}
          <div className="p-8 border-2 border-green-100 dark:border-green-900 rounded-2xl bg-green-50 dark:bg-green-900/20">
            <div className="text-center">
              <div className="inline-flex items-center justify-center w-16 h-16 bg-green-100 dark:bg-green-900 rounded-full mb-6">
                <FaWhatsapp className="text-green-600 dark:text-green-400 w-8 h-8" />
              </div>
              <h2 className="text-2xl font-semibold text-blue-900 dark:text-blue-300 mb-4">
                Instant WhatsApp Support
              </h2>
              <p className="text-gray-600 dark:text-gray-400 mb-8">
                Get real-time assistance from our technical team
              </p>
              <a
                href={`https://wa.me/263787211325?text=${whatsappMessage}`}
                target="_blank"
                rel="noopener noreferrer"
                className="inline-flex items-center gap-3 px-8 py-4 bg-green-600 hover:bg-green-700 text-white rounded-lg font-semibold transition-colors"
              >
                <FaWhatsapp className="w-5 h-5" />
                Start WhatsApp Chat
              </a>
            </div>
          </div>

          {/* Alternative Methods */}
          <div className="space-y-8">
            {/* Email */}
            <div className="p-8 border border-gray-200 dark:border-gray-700 rounded-2xl">
              <div className="flex items-start gap-6">
                <div className="text-darkgoldenrod dark:text-yellow-400 mt-1">
                  <FaEnvelope className="w-6 h-6" />
                </div>
                <div>
                  <h3 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-2">
                    Email Support
                  </h3>
                  <a
                    href="mailto:support@slykertech.co.zw"
                    className="text-gray-600 dark:text-gray-400 hover:text-darkgoldenrod dark:hover:text-yellow-400"
                  >
                    support@slykertech.co.zw
                  </a>
                </div>
              </div>
            </div>

            {/* Harare Office */}
            <div className="p-8 border border-gray-200 dark:border-gray-700 rounded-2xl">
              <div className="flex items-start gap-6">
                <div className="text-darkgoldenrod dark:text-yellow-400 mt-1">
                  <FaMapMarker className="w-6 h-6" />
                </div>
                <div>
                  <h3 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-2">
                    Harare Headquarters
                  </h3>
                  <p className="text-gray-600 dark:text-gray-400">
                    2789 Dada Crescent<br />
                    Budiriro 2<br />
                    Harare, Zimbabwe
                  </p>
                  <a 
                    href="tel:+263787211325"
                    className="mt-2 inline-block text-gray-600 dark:text-gray-400 hover:text-darkgoldenrod dark:hover:text-yellow-400"
                  >
                    <FaPhone className="inline mr-2" />
                    +263 78 721 1325
                  </a>
                </div>
              </div>
            </div>
          </div>
        </div>
      </section>

      {/* Support Hours */}
      <section className="py-16 bg-gray-50 dark:bg-gray-900">
        <div className="max-w-6xl mx-auto px-4 sm:px-8">
          <div className="p-8 bg-white dark:bg-gray-800 rounded-2xl shadow-sm">
            <div className="flex items-center gap-6">
              <MdAccessTime className="text-4xl text-darkgoldenrod dark:text-yellow-400" />
              <div>
                <h3 className="text-xl font-semibold text-blue-900 dark:text-blue-300 mb-2">
                  Support Hours (CAT)
                </h3>
                <p className="text-gray-600 dark:text-gray-400">
                  Monday - Friday: 7:30AM - 5:30PM<br/>
                  Saturday: 8:00AM - 12:30PM<br/>
                  Sunday: Closed<br/>
                  Public Holidays: Closed
                </p>
              </div>
            </div>
          </div>
        </div>
      </section>

      {/* Map Section */}
      <section className="bg-white dark:bg-gray-950 py-24">
        <div className="max-w-6xl mx-auto px-4 sm:px-8">
          <div className="rounded-2xl overflow-hidden shadow-xl">
            <iframe
              title="Slyker Tech Web Services Location"
              src="https://www.google.com/maps/embed?pb=!1m18!1m12!1m3!1d192122.28528871023!2d31.044403200000005!3d-17.832345599999996!2m3!1f0!2f0!3f0!3m2!1i1024!2i768!4f13.1!3m3!1m2!1s0x1931a17094880b63%3A0x1f01f408f5ce677b!2sSlyker%20Tech%20Web%20Services(STWS)!5e1!3m2!1sen!2szw!4v1746835401341!5m2!1sen!2szw"
              width="100%"
              height="400"
              className="border-0"
              allowFullScreen
              loading="lazy"
            ></iframe>
          </div>
        </div>
      </section>
    </div>
  );
}