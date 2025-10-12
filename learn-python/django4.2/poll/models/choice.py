from django.db import models

from poll.models.question import Question


class Choice(models.Model):
    question = models.ForeignKey["Question"](Question, on_delete=models.CASCADE)

    choice_text = models.CharField[str, str](max_length=200)
    votes = models.IntegerField[int, int](default=0)

    def __str__(self):
        return self.choice_text
