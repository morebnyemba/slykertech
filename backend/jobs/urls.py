from django.urls import path, include
from rest_framework.routers import DefaultRouter
from .views import JobPostingViewSet, JobApplicationViewSet

router = DefaultRouter()
router.register(r'postings', JobPostingViewSet, basename='job-posting')
router.register(r'applications', JobApplicationViewSet, basename='job-application')

urlpatterns = [
    path('', include(router.urls)),
]
