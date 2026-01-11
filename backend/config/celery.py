import os
from celery import Celery
from celery.schedules import crontab

os.environ.setdefault('DJANGO_SETTINGS_MODULE', 'config.settings')

app = Celery('slykertech')
app.config_from_object('django.conf:settings', namespace='CELERY')
app.autodiscover_tasks()

# Celery Beat Schedule for WHMCS-style cron jobs
app.conf.beat_schedule = {
    'send-renewal-reminders-30-days': {
        'task': 'services.tasks.send_renewal_reminders',
        'schedule': crontab(hour=8, minute=0),  # Daily at 8 AM
        'args': (30,)
    },
    'send-renewal-reminders-14-days': {
        'task': 'services.tasks.send_renewal_reminders',
        'schedule': crontab(hour=8, minute=15),
        'args': (14,)
    },
    'send-renewal-reminders-7-days': {
        'task': 'services.tasks.send_renewal_reminders',
        'schedule': crontab(hour=8, minute=30),
        'args': (7,)
    },
    'send-renewal-reminders-1-day': {
        'task': 'services.tasks.send_renewal_reminders',
        'schedule': crontab(hour=8, minute=45),
        'args': (1,)
    },
    'check-service-suspensions': {
        'task': 'services.tasks.check_suspensions',
        'schedule': crontab(hour=9, minute=0),  # Daily at 9 AM
    },
    'sync-cpanel-usage': {
        'task': 'services.tasks.sync_cpanel_usage',
        'schedule': crontab(minute=0),  # Hourly
    },
    'process-commission-payments': {
        'task': 'reseller.tasks.process_commissions',
        'schedule': crontab(day_of_month=1, hour=10, minute=0),  # Monthly
    },
}

app.conf.timezone = 'UTC'
