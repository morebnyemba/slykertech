from rest_framework import serializers
from .models import JobPosting, JobApplication

class JobPostingSerializer(serializers.ModelSerializer):
    application_count = serializers.SerializerMethodField()
    
    class Meta:
        model = JobPosting
        fields = [
            'id', 'title', 'description', 'employment_type', 'location',
            'salary_range', 'requirements', 'responsibilities', 'posted_date',
            'deadline', 'is_active', 'application_count', 'created_at', 'updated_at'
        ]
        read_only_fields = ['posted_date', 'created_at', 'updated_at']
    
    def get_application_count(self, obj):
        return obj.applications.count()

class JobApplicationSerializer(serializers.ModelSerializer):
    job_title = serializers.CharField(source='job.title', read_only=True)
    applicant_email = serializers.CharField(source='applicant.email', read_only=True)
    applicant_name = serializers.CharField(source='applicant.full_name', read_only=True)
    
    class Meta:
        model = JobApplication
        fields = [
            'id', 'job', 'job_title', 'applicant', 'applicant_email', 'applicant_name',
            'resume_url', 'cover_letter', 'applied_date', 'status',
            'created_at', 'updated_at'
        ]
        read_only_fields = ['applicant', 'applied_date', 'created_at', 'updated_at']
    
    def create(self, validated_data):
        validated_data['applicant'] = self.context['request'].user
        return super().create(validated_data)

class JobApplicationCreateSerializer(serializers.ModelSerializer):
    class Meta:
        model = JobApplication
        fields = ['job', 'resume_url', 'cover_letter']
    
    def create(self, validated_data):
        validated_data['applicant'] = self.context['request'].user
        return super().create(validated_data)
