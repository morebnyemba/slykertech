from django.urls import path, include
from rest_framework.routers import DefaultRouter
from .views import (ServiceViewSet, ServiceSubscriptionViewSet, DNSRecordViewSet,
                   ProjectTrackerViewSet, ProjectMilestoneViewSet, ProjectTaskViewSet,
                   ProjectCommentViewSet, HostingProductViewSet, DomainProductViewSet,
                   ServiceAddonViewSet, DomainRegistrationViewSet, whois_check)

router = DefaultRouter()
router.register(r'services', ServiceViewSet, basename='service')
router.register(r'subscriptions', ServiceSubscriptionViewSet, basename='subscription')
router.register(r'dns-records', DNSRecordViewSet, basename='dns-record')
router.register(r'projects', ProjectTrackerViewSet, basename='project')
router.register(r'milestones', ProjectMilestoneViewSet, basename='milestone')
router.register(r'tasks', ProjectTaskViewSet, basename='task')
router.register(r'project-comments', ProjectCommentViewSet, basename='project-comment')
router.register(r'hosting-products', HostingProductViewSet, basename='hosting-product')
router.register(r'domain-products', DomainProductViewSet, basename='domain-product')
router.register(r'service-addons', ServiceAddonViewSet, basename='service-addon')
router.register(r'domain-registrations', DomainRegistrationViewSet, basename='domain-registration')

urlpatterns = [
    path('whois/check/', whois_check, name='whois-check'),
    path('', include(router.urls)),
]
