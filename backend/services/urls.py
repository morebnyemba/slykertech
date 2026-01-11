from django.urls import path, include
from rest_framework.routers import DefaultRouter
from .views import (ServiceViewSet, ServiceSubscriptionViewSet, DNSRecordViewSet,
                   ProjectTrackerViewSet, ProjectMilestoneViewSet, ProjectTaskViewSet,
                   ProjectCommentViewSet)

router = DefaultRouter()
router.register(r'services', ServiceViewSet, basename='service')
router.register(r'subscriptions', ServiceSubscriptionViewSet, basename='subscription')
router.register(r'dns-records', DNSRecordViewSet, basename='dns-record')
router.register(r'projects', ProjectTrackerViewSet, basename='project')
router.register(r'milestones', ProjectMilestoneViewSet, basename='milestone')
router.register(r'tasks', ProjectTaskViewSet, basename='task')
router.register(r'project-comments', ProjectCommentViewSet, basename='project-comment')

urlpatterns = [
    path('', include(router.urls)),
]
