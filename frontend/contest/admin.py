import contest.models
from django.contrib import admin

admin.site.register(contest.models.Language)
admin.site.register(contest.models.Submission)
admin.site.register(contest.models.Game)