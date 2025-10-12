from __future__ import annotations

from datetime import datetime, timedelta
from typing import TYPE_CHECKING

from django.contrib import admin
from django.db import models
from django.utils import timezone

if TYPE_CHECKING:
    from django.db.models import Manager

    from . import Choice


class Question(models.Model):
    question_text = models.CharField[str, str](max_length=200)
    pub_date = models.DateTimeField[datetime, datetime]("date published")

    choice_set: Manager[Choice]

    def __str__(self):
        return self.question_text

    # a decorator for ModelAdmin.list_display in admin page
    # see: poll/admin.py:46
    @admin.display(
        description="Published recently?",  # a name to show in admin list page
        boolean=True,  # type == boolean?
        ordering="pub_date",  # sorting by other property works
    )
    def was_published_recently(self):
        present = timezone.now()
        yesterday = present - timedelta(days=1)

        return present >= self.pub_date >= yesterday
