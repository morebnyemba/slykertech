from django.contrib import admin
from .models import (Service, ServiceSubscription, DNSRecord, 
                    ProjectTracker, ProjectMilestone, ProjectTask, ProjectComment)


@admin.register(Service)
class ServiceAdmin(admin.ModelAdmin):
    list_display = ('name', 'category', 'base_price', 'is_active', 'created_at')
    list_filter = ('category', 'is_active', 'created_at')
    search_fields = ('name', 'description')
    readonly_fields = ('created_at', 'updated_at')


class DNSRecordInline(admin.TabularInline):
    model = DNSRecord
    extra = 1


@admin.register(ServiceSubscription)
class ServiceSubscriptionAdmin(admin.ModelAdmin):
    list_display = ('client', 'service', 'status', 'billing_cycle', 'start_date', 'end_date', 'price')
    list_filter = ('status', 'billing_cycle', 'auto_renew', 'created_at')
    search_fields = ('client__company_name', 'service__name')
    readonly_fields = ('created_at', 'updated_at')
    inlines = [DNSRecordInline]
    
    fieldsets = (
        ('Subscription Details', {
            'fields': ('client', 'service', 'status', 'billing_cycle', 'price')
        }),
        ('Dates', {
            'fields': ('start_date', 'end_date', 'auto_renew')
        }),
        ('Additional Information', {
            'fields': ('notes', 'metadata')
        }),
        ('Timestamps', {
            'fields': ('created_at', 'updated_at'),
            'classes': ('collapse',)
        }),
    )


@admin.register(DNSRecord)
class DNSRecordAdmin(admin.ModelAdmin):
    list_display = ('domain', 'record_type', 'name', 'content', 'ttl', 'is_active')
    list_filter = ('record_type', 'is_active', 'created_at')
    search_fields = ('domain', 'name', 'content')
    readonly_fields = ('created_at', 'updated_at')


class ProjectMilestoneInline(admin.TabularInline):
    model = ProjectMilestone
    extra = 1
    fields = ('title', 'status', 'due_date', 'order')


class ProjectTaskInline(admin.TabularInline):
    model = ProjectTask
    extra = 1
    fields = ('title', 'status', 'assigned_to', 'due_date')


@admin.register(ProjectTracker)
class ProjectTrackerAdmin(admin.ModelAdmin):
    list_display = ('title', 'subscription', 'status', 'priority', 'progress_percentage', 
                   'assigned_to', 'estimated_completion_date')
    list_filter = ('status', 'priority', 'created_at')
    search_fields = ('title', 'description', 'subscription__client__company_name')
    readonly_fields = ('created_at', 'updated_at')
    inlines = [ProjectMilestoneInline, ProjectTaskInline]
    
    fieldsets = (
        ('Project Information', {
            'fields': ('subscription', 'title', 'description', 'status', 'priority')
        }),
        ('Progress Tracking', {
            'fields': ('progress_percentage', 'estimated_hours', 'actual_hours')
        }),
        ('Dates', {
            'fields': ('start_date', 'estimated_completion_date', 'actual_completion_date')
        }),
        ('Assignment', {
            'fields': ('assigned_to',)
        }),
        ('Additional Data', {
            'fields': ('metadata',)
        }),
        ('Timestamps', {
            'fields': ('created_at', 'updated_at'),
            'classes': ('collapse',)
        }),
    )


@admin.register(ProjectMilestone)
class ProjectMilestoneAdmin(admin.ModelAdmin):
    list_display = ('title', 'project', 'status', 'due_date', 'order')
    list_filter = ('status', 'created_at')
    search_fields = ('title', 'project__title')
    readonly_fields = ('created_at', 'updated_at')


@admin.register(ProjectTask)
class ProjectTaskAdmin(admin.ModelAdmin):
    list_display = ('title', 'project', 'milestone', 'status', 'assigned_to', 'due_date')
    list_filter = ('status', 'created_at')
    search_fields = ('title', 'project__title')
    readonly_fields = ('created_at', 'updated_at')


@admin.register(ProjectComment)
class ProjectCommentAdmin(admin.ModelAdmin):
    list_display = ('project', 'user', 'is_internal', 'created_at')
    list_filter = ('is_internal', 'created_at')
    search_fields = ('project__title', 'user__email', 'comment')
    readonly_fields = ('created_at', 'updated_at')

