#!/usr/bin/env python3
"""
Generate a secure encryption key for APIConfiguration model
"""
import secrets

# Generate a secure 32-byte key
key = secrets.token_urlsafe(32)

print("=" * 60)
print("ENCRYPTION KEY GENERATED")
print("=" * 60)
print("")
print("Add this line to your .env file:")
print("")
print(f"ENCRYPTION_KEY={key}")
print("")
print("=" * 60)
print("⚠️  IMPORTANT: Keep this key secure and never commit to git!")
print("=" * 60)
