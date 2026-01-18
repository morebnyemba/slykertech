from django.contrib import admin
from .models import JobPosting, JobApplication

@admin.register(JobPosting)
class JobPostingAdmin(admin.ModelAdmin):
    list_display = ['title', 'employment_type', 'location', 'posted_date', 'deadline', 'is_active']
    list_filter = ['employment_type', 'location', 'is_active', 'posted_date']
    search_fields = ['title', 'description', 'location']
    date_hierarchy = 'posted_date'
    ordering = ['-posted_date']
    
    fieldsets = (
        ('Basic Information', {
            'fields': ('title', 'employment_type', 'location', 'salary_range', 'is_active')
        }),
        ('Job Details', {
            'fields': ('description', 'requirements', 'responsibilities')
        }),
        ('Dates', {
            'fields': ('deadline',)
        }),
    )

@admin.register(JobApplication)
class JobApplicationAdmin(admin.ModelAdmin):
    list_display = ['applicant', 'job', 'applied_date', 'status']
    list_filter = ['status', 'applied_date']
    search_fields = ['applicant__email', 'job__title']
    date_hierarchy = 'applied_date'
    ordering = ['-applied_date']
    readonly_fields = ['applied_date']
    
    fieldsets = (
        ('Application Information', {
            'fields': ('job', 'applicant', 'status')
        }),
        ('Application Details', {
            'fields': ('resume_url', 'cover_letter')
        }),
        ('Dates', {
            'fields': ('applied_date',)
        }),
    )
