from django.test import TestCase
from django.urls import reverse

from .helpers import create_question


class DetailViewTests(TestCase):
    def test_future_question(self):
        """
        The detail view of a question with a `pub_date` in the future
        returns a `404` not found.
        """

        future_question = create_question("Future question.", 5)
        response = self.client.get(reverse("poll:detail", args=[future_question.pk]))

        self.assertEqual(response.status_code, 404)

    def test_past_question(self):
        """
        The detail view of a question with a `pub_date` in the past
        displays the question's text.
        """

        question = create_question("Past question.", -5)
        response = self.client.get(reverse("poll:detail", args=[question.pk]))

        self.assertContains(response, question.question_text)
