from django.contrib import admin
from django.contrib.auth.admin import UserAdmin as BaseUserAdmin
from django.utils.translation import gettext_lazy as _
from .models import User, StatusChangeLog, ActivityLog, BackupLog


@admin.register(User)
class UserAdmin(BaseUserAdmin):
    fieldsets = (
        (None, {'fields': ('email', 'password')}),
        (_('Personal info'), {'fields': ('first_name', 'last_name', 'phone', 'company_name')}),
        (_('Permissions'), {'fields': ('user_type', 'is_active', 'is_staff', 'is_superuser', 'groups', 'user_permissions')}),
        (_('Important dates'), {'fields': ('last_login', 'date_joined')}),
    )
    add_fieldsets = (
        (None, {
            'classes': ('wide',),
            'fields': ('email', 'password1', 'password2', 'user_type'),
        }),
    )
    list_display = ('email', 'first_name', 'last_name', 'user_type', 'is_staff', 'is_active')
    list_filter = ('user_type', 'is_staff', 'is_superuser', 'is_active')
    search_fields = ('email', 'first_name', 'last_name', 'company_name')
    ordering = ('email',)


@admin.register(StatusChangeLog)
class StatusChangeLogAdmin(admin.ModelAdmin):
    list_display = ('content_type', 'object_id', 'old_status', 'new_status', 'changed_by', 'created_at')
    list_filter = ('content_type', 'new_status', 'created_at')
    search_fields = ('reason',)
    readonly_fields = ('content_type', 'object_id', 'old_status', 'new_status',
                      'changed_by', 'reason', 'metadata', 'created_at')
    date_hierarchy = 'created_at'


@admin.register(ActivityLog)
class ActivityLogAdmin(admin.ModelAdmin):
    list_display = ('user', 'action', 'short_description', 'content_type', 'created_at')
    list_filter = ('action', 'content_type', 'created_at')
    search_fields = ('description', 'user__email')
    readonly_fields = ('user', 'action', 'description', 'content_type', 'object_id',
                      'ip_address', 'user_agent', 'metadata', 'created_at')
    date_hierarchy = 'created_at'

    def short_description(self, obj):
        return obj.description[:80] if obj.description else ''
    short_description.short_description = 'Description'


@admin.register(BackupLog)
class BackupLogAdmin(admin.ModelAdmin):
    list_display = ('filename', 'database_name', 'status', 'file_size_display', 'duration_seconds', 'created_at')
    list_filter = ('status', 'database_name', 'created_at')
    search_fields = ('filename',)
    readonly_fields = ('filename', 'file_size', 'status', 'database_name',
                      'error_message', 'duration_seconds', 'created_at')
    date_hierarchy = 'created_at'

    def file_size_display(self, obj):
        if obj.file_size is None:
            return '-'
        if obj.file_size < 1024:
            return f'{obj.file_size} B'
        elif obj.file_size < 1024 * 1024:
            return f'{obj.file_size / 1024:.1f} KB'
        else:
            return f'{obj.file_size / (1024 * 1024):.1f} MB'
    file_size_display.short_description = 'File Size'

