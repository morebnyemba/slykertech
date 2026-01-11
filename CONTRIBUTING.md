# Contributing to Slyker Tech

Thank you for considering contributing to Slyker Tech! This document provides guidelines and instructions for contributing to the project.

## Code of Conduct

By participating in this project, you agree to maintain a respectful and collaborative environment.

## How to Contribute

### Reporting Bugs

Before creating a bug report, please check existing issues to avoid duplicates.

When reporting a bug, include:
- Clear description of the issue
- Steps to reproduce
- Expected behavior
- Actual behavior
- Screenshots (if applicable)
- Environment details (OS, browser, Python/Node version)

### Suggesting Features

Feature suggestions are welcome! Please provide:
- Clear description of the feature
- Use case and benefits
- Possible implementation approach
- Any relevant examples or mockups

### Pull Requests

1. **Fork the repository** and create a new branch from `main`
2. **Follow the coding standards** outlined below
3. **Write clear commit messages** following conventional commits format
4. **Test your changes** thoroughly
5. **Update documentation** if needed
6. **Submit a pull request** with a clear description

## Development Setup

### Prerequisites
- Python 3.8+
- Node.js 18+
- Git

### Quick Setup
```bash
# Clone the repository
git clone https://github.com/morebnyemba/slykertech.git
cd slykertech

# Run the setup script
./setup.sh
```

Or follow the manual setup in [README.md](README.md)

## Coding Standards

### Backend (Python/Django)

- Follow [PEP 8](https://pep8.org/) style guide
- Use meaningful variable and function names
- Write docstrings for all functions and classes
- Keep functions focused and small
- Use type hints where appropriate
- Write unit tests for new features

Example:
```python
def get_user_subscriptions(user_id: int) -> List[Subscription]:
    """
    Retrieve all active subscriptions for a user.
    
    Args:
        user_id: The ID of the user
        
    Returns:
        List of Subscription objects
    """
    return Subscription.objects.filter(
        client__user_id=user_id,
        status='active'
    )
```

### Frontend (TypeScript/React)

- Use TypeScript for type safety
- Follow React best practices
- Use functional components with hooks
- Keep components small and reusable
- Write descriptive prop types
- Use meaningful component and file names

Example:
```typescript
interface ServiceCardProps {
  title: string;
  description: string;
  price: number;
  isActive: boolean;
}

export function ServiceCard({ title, description, price, isActive }: ServiceCardProps) {
  // Component logic
}
```

### Commit Messages

Follow [Conventional Commits](https://www.conventionalcommits.org/) format:

- `feat:` New feature
- `fix:` Bug fix
- `docs:` Documentation changes
- `style:` Code style changes (formatting, etc.)
- `refactor:` Code refactoring
- `test:` Adding or updating tests
- `chore:` Maintenance tasks

Examples:
```
feat: add DNS record management to client portal
fix: resolve authentication token refresh issue
docs: update API documentation for new endpoints
```

## Project Structure

```
slykertech/
├── src/                    # Frontend source
│   ├── app/               # Next.js pages
│   ├── components/        # React components
│   └── lib/              # Utility functions
├── backend/               # Django backend
│   ├── accounts/         # Authentication app
│   ├── clients/          # Client management
│   ├── services/         # Services and subscriptions
│   └── integrations/     # External integrations
└── docs/                 # Additional documentation
```

## Testing

### Backend Tests
```bash
cd backend
source venv/bin/activate
python manage.py test
```

### Frontend Tests
```bash
npm run test
npm run lint
```

## Database Migrations

When making model changes:

```bash
cd backend
python manage.py makemigrations
python manage.py migrate
```

Commit both the migration file and model changes together.

## API Changes

When modifying the API:

1. Update the serializers
2. Update the views
3. Update the URL patterns
4. Update API documentation in `backend/README.md`
5. Test the endpoints
6. Update frontend integration if needed

## Security

- Never commit sensitive information (API keys, passwords, etc.)
- Use environment variables for configuration
- Always encrypt sensitive data
- Follow OWASP best practices
- Report security vulnerabilities privately to support@slykertech.co.zw

## Documentation

- Update relevant documentation when making changes
- Keep README files up to date
- Add code comments for complex logic
- Update API documentation for endpoint changes
- Include examples in documentation

## Questions?

If you have questions about contributing:

- Open an issue with the `question` label
- Email: support@slykertech.co.zw
- Check existing documentation

## License

By contributing to Slyker Tech, you agree that your contributions will be licensed under the same license as the project.

---

Thank you for contributing to Slyker Tech! 🚀
