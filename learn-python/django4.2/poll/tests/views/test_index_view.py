from django.test import TestCase
from django.urls import reverse

from .helpers import create_question


class IndexViewTests(TestCase):
    def test_no_questions(self):
        """
        If no questions exist, an appropriate message is displayed.
        """

        response = self.client.get(reverse("poll:index"))

        self.assertEqual(response.status_code, 200)
        self.assertContains(response, "No polls are available.")
        self.assertQuerySetEqual(response.context["latest_question_list"], [])

    def test_past_question(self):
        """
        Questions with a `pub_date` in the past are displayed on the index page.
        """

        question = create_question("Past question.", days=-30)
        response = self.client.get(reverse("poll:index"))

        self.assertEqual(response.status_code, 200)
        self.assertContains(response, question.question_text)
        self.assertQuerySetEqual(response.context["latest_question_list"], [question])

    def test_future_question(self):
        """
        Questions with a `pub_date` in the future aren't displayed on the index page.
        """

        create_question("Future question.", days=30)
        response = self.client.get(reverse("poll:index"))

        self.assertEqual(response.status_code, 200)
        self.assertContains(response, "No polls are available.")
        self.assertQuerySetEqual(response.context["latest_question_list"], [])

    def test_future_and_past_question(self):
        """
        Even if both past and future questions exist, only past questions are displayed.
        """

        past_question = create_question("Past question.", days=-30)
        future_question = create_question("Future question.", days=30)

        response = self.client.get(reverse("poll:index"))

        self.assertEqual(response.status_code, 200)
        self.assertContains(response, past_question.question_text)
        self.assertNotContains(response, future_question.question_text)
        self.assertQuerySetEqual(
            response.context["latest_question_list"], [past_question]
        )

    def test_two_past_questions(self):
        """
        The questions index page may display multiple questions.
        """

        question1 = create_question("Question#1", -30)
        question2 = create_question("Question#2", -5)

        response = self.client.get(reverse("poll:index"))

        self.assertEqual(response.status_code, 200)
        self.assertContains(response, question1.question_text)
        self.assertContains(response, question2.question_text)
        self.assertQuerySetEqual(
            response.context["latest_question_list"], [question2, question1]
        )
