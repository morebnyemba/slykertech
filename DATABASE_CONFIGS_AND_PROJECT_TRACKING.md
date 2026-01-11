# Database-Stored API Configurations & Project Tracking

## Overview

This document describes the new features for managing API configurations in the database and tracking project progress for services like web development, SEO, design, etc.

---

## API Configuration Management

### Why Database Storage?

Instead of storing API credentials in environment variables (`.env`), configurations are now stored in the database with encryption. This provides:

- **Centralized Management**: Admin interface for all API configurations
- **Multi-Environment Support**: Sandbox and production configs
- **Security**: Encrypted storage of sensitive credentials
- **Flexibility**: Easy to update without redeploying
- **Audit Trail**: Track when configurations were created/updated

### APIConfiguration Model

```python
class APIConfiguration(models.Model):
    provider = CharField  # whatsapp, namecheap, cloudflare, stripe, paypal
    name = CharField
    api_url = URLField
    encrypted_api_key = TextField
    encrypted_api_secret = TextField
    encrypted_access_token = TextField
    config_data = JSONField  # Additional provider-specific config
    is_active = BooleanField
    is_sandbox = BooleanField
```

### Setting Up API Configurations

#### Via Django Admin

1. Go to `/admin/integrations/apiconfiguration/`
2. Click "Add API Configuration"
3. Fill in the details:
   - **Provider**: Select from dropdown (WhatsApp, Namecheap, etc.)
   - **Name**: Descriptive name (e.g., "Production WhatsApp API")
   - **API URL**: Base URL for the API
   - **API Key/Secret/Token**: Enter credentials (automatically encrypted)
   - **Config Data**: JSON field for additional settings
   - **Is Active**: Enable/disable without deleting
   - **Is Sandbox**: Mark test environments

#### Via API

```python
from integrations.models import APIConfiguration

# Create WhatsApp configuration
config = APIConfiguration.objects.create(
    provider='whatsapp',
    name='Production WhatsApp',
    api_url='https://graph.facebook.com/v18.0',
    is_active=True,
    is_sandbox=False,
    config_data={
        'phone_number_id': '1234567890',
        'business_account_id': '0987654321'
    }
)

# Set encrypted credentials
config.set_access_token('EAAxxxx...')
config.save()
```

#### Via REST API

```bash
POST /api/integrations/api-configs/
{
  "provider": "whatsapp",
  "name": "Production WhatsApp",
  "api_url": "https://graph.facebook.com/v18.0",
  "access_token": "EAAxxxx...",  # Will be encrypted
  "config_data": {
    "phone_number_id": "1234567890",
    "business_account_id": "0987654321"
  },
  "is_active": true,
  "is_sandbox": false
}
```

### Using API Configurations

Services automatically load from database:

```python
from notifications.whatsapp_service import WhatsAppService

# Automatically loads active WhatsApp config from database
# Falls back to environment variables if no DB config exists
whatsapp = WhatsAppService()
whatsapp.send_message(to="263771234567", message="Hello!")
```

### Fallback Behavior

If no database configuration is found, the system falls back to environment variables:

```env
WHATSAPP_API_URL=https://graph.facebook.com/v18.0
WHATSAPP_PHONE_NUMBER_ID=
WHATSAPP_ACCESS_TOKEN=
```

This ensures backward compatibility and smooth migration.

---

## Project Progress Tracking

### Overview

Track progress for ongoing service projects like:
- Web Development
- SEO Campaigns
- Design Projects
- Website Maintenance
- Marketing Campaigns
- Custom Development

### Models

#### 1. ProjectTracker

Main project tracking model:

```python
class ProjectTracker(models.Model):
    subscription = ForeignKey(ServiceSubscription)
    title = CharField
    description = TextField
    status = CharField  # not_started, planning, in_progress, review, completed, on_hold
    priority = CharField  # low, medium, high, urgent
    
    # Progress tracking
    progress_percentage = IntegerField  # 0-100
    estimated_hours = DecimalField
    actual_hours = DecimalField
    
    # Dates
    start_date = DateField
    estimated_completion_date = DateField
    actual_completion_date = DateField
    
    # Assignment
    assigned_to = ForeignKey(User)
    
    # Additional data
    metadata = JSONField
```

#### 2. ProjectMilestone

Break projects into phases:

```python
class ProjectMilestone(models.Model):
    project = ForeignKey(ProjectTracker)
    title = CharField
    description = TextField
    status = CharField  # pending, in_progress, completed, skipped
    due_date = DateField
    completed_date = DateField
    order = IntegerField  # Display order
```

#### 3. ProjectTask

Detailed task management:

```python
class ProjectTask(models.Model):
    milestone = ForeignKey(ProjectMilestone, optional)
    project = ForeignKey(ProjectTracker)
    title = CharField
    description = TextField
    status = CharField  # todo, in_progress, completed, blocked
    assigned_to = ForeignKey(User)
    estimated_hours = DecimalField
    actual_hours = DecimalField
    due_date = DateField
    order = IntegerField
```

#### 4. ProjectComment

Communication and updates:

```python
class ProjectComment(models.Model):
    project = ForeignKey(ProjectTracker)
    user = ForeignKey(User)
    comment = TextField
    is_internal = BooleanField  # Hidden from clients
    attachments = JSONField  # List of URLs
```

### Creating a Project

#### Via Admin Interface

1. Go to `/admin/services/projecttracker/`
2. Click "Add Project Tracker"
3. Fill in project details
4. Add inline milestones and tasks
5. Save

#### Via API

```bash
POST /api/services/projects/
{
  "subscription": 1,
  "title": "E-commerce Website Development",
  "description": "Full e-commerce site with payment integration",
  "status": "in_progress",
  "priority": "high",
  "estimated_hours": 120,
  "start_date": "2024-01-15",
  "estimated_completion_date": "2024-03-15",
  "assigned_to": 2
}
```

### Managing Milestones

```bash
POST /api/services/milestones/
{
  "project": 1,
  "title": "Design Phase",
  "description": "UI/UX design and mockups",
  "status": "completed",
  "due_date": "2024-01-31",
  "order": 1
}
```

### Managing Tasks

```bash
POST /api/services/tasks/
{
  "project": 1,
  "milestone": 1,
  "title": "Create homepage mockup",
  "status": "completed",
  "assigned_to": 3,
  "estimated_hours": 8,
  "actual_hours": 9,
  "due_date": "2024-01-20"
}
```

### Updating Progress

```bash
POST /api/services/projects/1/update_progress/
{
  "percentage": 45
}
```

The system automatically:
- Updates `progress_percentage` to 45%
- If percentage reaches 100%, sets `status` to 'completed'
- Sets `actual_completion_date` when completed

### Adding Comments

```bash
POST /api/services/project-comments/
{
  "project": 1,
  "comment": "Homepage design approved by client",
  "is_internal": false
}
```

Clients can only see comments where `is_internal=false`.

### Client View

Clients can view their project status:

```bash
GET /api/services/projects/
```

Response includes:
- Project details
- Progress percentage
- Milestones with status
- Tasks assigned
- Comments (non-internal only)
- Time tracking

Example response:
```json
{
  "id": 1,
  "title": "E-commerce Website Development",
  "status": "in_progress",
  "priority": "high",
  "progress_percentage": 45,
  "estimated_hours": 120,
  "actual_hours": 54,
  "milestones": [
    {
      "id": 1,
      "title": "Design Phase",
      "status": "completed",
      "due_date": "2024-01-31"
    },
    {
      "id": 2,
      "title": "Development Phase",
      "status": "in_progress",
      "due_date": "2024-02-28"
    }
  ],
  "tasks": [
    {
      "id": 1,
      "title": "Create homepage mockup",
      "status": "completed",
      "assigned_to": {
        "email": "designer@slykertech.co.zw",
        "full_name": "Jane Designer"
      }
    }
  ]
}
```

---

## Use Cases

### Web Development Project

```python
# Create project
project = ProjectTracker.objects.create(
    subscription=web_dev_subscription,
    title="Company Website Redesign",
    description="Complete redesign with modern UI",
    status='in_progress',
    priority='high',
    estimated_hours=80
)

# Add milestones
milestones = [
    ProjectMilestone.objects.create(
        project=project,
        title="Discovery & Planning",
        order=1
    ),
    ProjectMilestone.objects.create(
        project=project,
        title="Design",
        order=2
    ),
    ProjectMilestone.objects.create(
        project=project,
        title="Development",
        order=3
    ),
    ProjectMilestone.objects.create(
        project=project,
        title="Testing & Launch",
        order=4
    )
]

# Add tasks to milestone
ProjectTask.objects.create(
    project=project,
    milestone=milestones[1],  # Design phase
    title="Create wireframes",
    status='todo',
    assigned_to=designer,
    estimated_hours=16
)
```

### SEO Campaign

```python
# Create SEO project
seo_project = ProjectTracker.objects.create(
    subscription=seo_subscription,
    title="6-Month SEO Campaign",
    description="Improve organic search rankings",
    status='in_progress',
    priority='medium',
    estimated_hours=48  # 8 hours per month
)

# Add monthly milestones
for month in range(1, 7):
    ProjectMilestone.objects.create(
        project=seo_project,
        title=f"Month {month} Optimization",
        order=month
    )

# Add tasks
ProjectTask.objects.create(
    project=seo_project,
    title="Keyword research",
    status='completed',
    estimated_hours=4
)

ProjectTask.objects.create(
    project=seo_project,
    title="On-page optimization",
    status='in_progress',
    estimated_hours=8
)
```

### Design Project

```python
# Logo design project
design_project = ProjectTracker.objects.create(
    subscription=design_subscription,
    title="Company Logo Design",
    status='in_progress',
    priority='urgent',
    estimated_hours=20
)

# Simple milestone structure
ProjectMilestone.objects.create(
    project=design_project,
    title="Initial concepts",
    order=1
)

ProjectMilestone.objects.create(
    project=design_project,
    title="Revisions",
    order=2
)

ProjectMilestone.objects.create(
    project=design_project,
    title="Final delivery",
    order=3
)
```

---

## Admin Workflow

### 1. Create Project
- Client subscribes to a service (web dev, SEO, etc.)
- Admin creates ProjectTracker linked to subscription
- Set estimated hours and completion date
- Assign to team member

### 2. Break Down Work
- Add milestones for major phases
- Add tasks for specific work items
- Assign tasks to team members
- Set due dates

### 3. Track Progress
- Team updates task status as they work
- Add actual hours spent
- Update progress percentage regularly
- Add comments for client updates

### 4. Client Communication
- Clients see real-time progress
- Non-internal comments visible to clients
- Clients can add comments/feedback
- Email/WhatsApp notifications on updates

### 5. Completion
- Mark tasks as completed
- Update progress to 100%
- System auto-sets completion date
- Final review and handoff

---

## Permissions

### Clients
- View projects for their subscriptions
- View non-internal comments
- Add comments
- View progress and status

### Staff/Assigned Users
- View assigned projects
- Update task status
- Add comments (internal/external)
- Log hours

### Admins
- Full access to all projects
- Create/edit/delete projects
- Assign team members
- View all comments including internal

---

## Notifications Integration

Projects can trigger notifications:

```python
from notifications.models import Notification

# Notify client on milestone completion
Notification.objects.create(
    user=project.subscription.client.user,
    notification_type='email',
    category='service',
    title=f'Milestone Completed: {milestone.title}',
    message=f'Great news! We have completed {milestone.title} for your project.'
)

# Send WhatsApp update
if project.subscription.client.user.whatsapp_notifications:
    from notifications.whatsapp_service import WhatsAppService
    whatsapp = WhatsAppService()
    whatsapp.send_message(
        to=project.subscription.client.user.mobile_number,
        message=f"Your project '{project.title}' is now {project.progress_percentage}% complete!"
    )
```

---

## Best Practices

### Project Setup
- Always link projects to subscriptions
- Set realistic estimated hours
- Break large projects into milestones
- Assign clear ownership

### Progress Tracking
- Update progress regularly (weekly minimum)
- Log actual hours for billing/analysis
- Keep milestone due dates achievable
- Use task status consistently

### Communication
- Add comments for significant updates
- Use internal comments for team notes
- Mark external comments for client visibility
- Attach relevant files/screenshots

### Completion
- Review all tasks before marking 100%
- Get client approval before completion
- Document final deliverables
- Archive project details

---

## API Reference Summary

### API Configurations
- `GET /api/integrations/api-configs/` - List configs
- `POST /api/integrations/api-configs/` - Create config
- `GET /api/integrations/api-configs/{id}/` - Config details
- `PUT /api/integrations/api-configs/{id}/` - Update config

### Projects
- `GET /api/services/projects/` - List projects
- `POST /api/services/projects/` - Create project
- `GET /api/services/projects/{id}/` - Project details
- `PUT /api/services/projects/{id}/` - Update project
- `POST /api/services/projects/{id}/update_progress/` - Update progress

### Milestones
- `GET /api/services/milestones/` - List milestones
- `POST /api/services/milestones/` - Create milestone

### Tasks
- `GET /api/services/tasks/` - List tasks
- `POST /api/services/tasks/` - Create task
- `PUT /api/services/tasks/{id}/` - Update task

### Comments
- `GET /api/services/project-comments/` - List comments
- `POST /api/services/project-comments/` - Add comment

---

For more information, see:
- [Backend README](backend/README.md)
- [Integration Guide](INTEGRATION_GUIDE.md)
- [API Testing Guide](API_TESTING.md)
