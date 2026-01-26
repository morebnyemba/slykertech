from django.contrib import admin
from django.db.utils import ProgrammingError, OperationalError
from .models import ReferralProfile, Referral, ReferralReward, ReferralSettings


@admin.register(ReferralProfile)
class ReferralProfileAdmin(admin.ModelAdmin):
    list_display = ['user', 'referral_code', 'total_referrals', 'successful_referrals', 
                    'total_earnings', 'is_active', 'created_at']
    list_filter = ['is_active', 'created_at']
    search_fields = ['user__email', 'referral_code']
    readonly_fields = ['referral_code', 'total_referrals', 'successful_referrals', 
                       'total_earnings', 'created_at', 'updated_at']
    raw_id_fields = ['user', 'referred_by']


@admin.register(Referral)
class ReferralAdmin(admin.ModelAdmin):
    list_display = ['referrer', 'referred_email', 'status', 'reward_amount', 
                    'signup_date', 'conversion_date', 'created_at']
    list_filter = ['status', 'created_at', 'signup_date', 'conversion_date']
    search_fields = ['referrer__user__email', 'referred_email', 'referral_code_used']
    readonly_fields = ['created_at', 'updated_at']
    raw_id_fields = ['referrer', 'referred_user']
    
    actions = ['mark_as_signed_up', 'mark_as_converted', 'mark_as_rewarded']
    
    def mark_as_signed_up(self, request, queryset):
        from django.utils import timezone
        queryset.update(status='signed_up', signup_date=timezone.now())
    mark_as_signed_up.short_description = "Mark selected referrals as signed up"
    
    def mark_as_converted(self, request, queryset):
        from django.utils import timezone
        queryset.update(status='converted', conversion_date=timezone.now())
    mark_as_converted.short_description = "Mark selected referrals as converted"
    
    def mark_as_rewarded(self, request, queryset):
        from django.utils import timezone
        queryset.update(status='rewarded', reward_date=timezone.now())
    mark_as_rewarded.short_description = "Mark selected referrals as rewarded"


@admin.register(ReferralReward)
class ReferralRewardAdmin(admin.ModelAdmin):
    list_display = ['referral_profile', 'reward_type', 'amount', 'status', 
                    'paid_at', 'created_at']
    list_filter = ['reward_type', 'status', 'created_at']
    search_fields = ['referral_profile__user__email']
    readonly_fields = ['created_at', 'updated_at']
    raw_id_fields = ['referral_profile', 'referral']
    
    actions = ['approve_rewards', 'mark_as_paid']
    
    def approve_rewards(self, request, queryset):
        queryset.filter(status='pending').update(status='approved')
    approve_rewards.short_description = "Approve selected rewards"
    
    def mark_as_paid(self, request, queryset):
        from django.utils import timezone
        queryset.filter(status='approved').update(status='paid', paid_at=timezone.now())
    mark_as_paid.short_description = "Mark selected rewards as paid"


@admin.register(ReferralSettings)
class ReferralSettingsAdmin(admin.ModelAdmin):
    list_display = ['signup_bonus_amount', 'conversion_bonus_amount', 
                    'conversion_bonus_percentage', 'minimum_payout', 
                    'is_program_active', 'updated_at']
    readonly_fields = ['created_at', 'updated_at']
    
    def has_add_permission(self, request):
        # Only allow one settings record
        # Handle case where table doesn't exist yet (before migrations)
        try:
            return not ReferralSettings.objects.exists()
        except (ProgrammingError, OperationalError):
            return True
    
    def has_delete_permission(self, request, obj=None):
        return False
