"""
Each view is responsible for doing one of two things:
- returning an 'HttpResponse' object containing the content for the requested page
- or raising an exception such as 'Http404'
"""

from typing import Any, Dict

from django.http import Http404, HttpRequest, HttpResponse, HttpResponseRedirect
from django.shortcuts import get_object_or_404, render
from django.template import loader
from django.urls import reverse
from django.utils import timezone
from django.views import generic

from .models import Choice, Question


def example_view(request: HttpRequest, arg="World"):
    # return HttpResponse("Hello, %s!" % arg)
    return HttpResponse(f"Hello, {arg}!")


def index_render_manual(request: HttpRequest):
    """
    load and render templates manually using django.template.loader
    """

    latest_question_list = Question.objects.order_by("-pub_date")[:5]

    template = loader.get_template("poll/index.html")
    context = {"latest_question_list": latest_question_list}

    return HttpResponse(template.render(context, request))


def index(request: HttpRequest):
    """
    load and render templates in one go using the 'render' shortcut
    """

    latest_question_list = Question.objects.order_by("-pub_date")[:5]
    context = {"latest_question_list": latest_question_list}

    return render(request, "poll/index.html", context)


class IndexView(generic.ListView):
    """
    this class-based view abstracts common rendering logic as shown above
    """

    # [REQUIRED] the view needs to know what model it will be acting upon
    def get_queryset(self):
        """
        Return the last five published questions
        (not including those set to be published in the future).
        """
        now = timezone.now()
        return Question.objects.filter(pub_date__lte=now).order_by("-pub_date")[:5]

    # default name to lookup: `<app name>/<model name>_detail.html`
    # eg. `poll/question_detail.html`
    template_name = "poll/index.html"

    # default key for the object list in the context: `<model name>_list`
    # eg. `question_list`
    context_object_name = "latest_question_list"


def detail_raise_404_manual(request: HttpRequest, question_id):
    """
    get object and raise Http404 if not found (manually)
    """

    context = {}

    try:
        question = Question.objects.get(pk=question_id)
        context["question"] = question
    except Question.DoesNotExist:
        raise Http404("Question does not exist")

    return render(request, "poll/detail.html", context)


def detail(request: HttpRequest, question_id):
    """
    get object and raise Http404 in one go using the shortcut
        `get_object_or_404`
    """

    context = {}
    context["question"] = get_object_or_404(Question, pk=question_id)

    return render(request, "poll/detail.html", context)


class DetailView(generic.DetailView):
    """
    this class-based view abstracts common rendering logic as shown above
    """

    # [REQUIRED] the view needs to know what model it will be acting upon
    # model = Question

    # either .model/.get_model or .queryset/.get_queryset is [REQUIRED]
    def get_queryset(self):
        return Question.objects.filter(pub_date__lte=timezone.now())

    # default name to lookup: `<app name>/<model name>_detail.html`
    # eg. `poll/question_detail.html`
    template_name = "poll/detail.html"

    # default key for the object in the context: `<model name>`
    # eg. `question`
    # context_object_name = "..."


def results(request: HttpRequest, question_id):
    question = get_object_or_404(Question, pk=question_id)
    context = {"question": question}

    return render(request, "poll/results.html", context)


class ResultsView(generic.DetailView):
    """
    this class-based view abstracts common rendering logic as shown above
    """

    # [REQUIRED] the view needs to know what model it will be acting upon
    model = Question

    # default name to lookup: `<app name>/<model name>_detail.html`
    # eg. `poll/question_detail.html`
    template_name = "poll/results.html"

    # default key for the object in the context: `<model name>`
    # eg. `question`
    # context_object_name = "..."


def vote(request: HttpRequest, question_id):
    """
    Explanation:
    - to get a request body -> `request.POST['KEY']`, `request.GET['KEY']`
    - if 'KEY' is not found in the dict -> raise `KeyError`
    - `reverse()` is basically the same as `{% url %}`
    - `HttpResponseRedirect` redirects a user to specified URL
    """

    question = get_object_or_404(Question, pk=question_id)
    context: Dict[str, Any] = {"question": question}

    try:
        selected_choice = question.choice_set.get(pk=request.POST["choice"])
    except (KeyError, Choice.DoesNotExist):
        context["error_message"] = "You didn't select a choice."
        return render(request, "poll/detail.html", context)

    # the code below can cause 'race conditions'
    # see: https://docs.djangoproject.com/en/4.2/ref/models/expressions/#avoiding-race-conditions-using-f
    selected_choice.votes += 1
    selected_choice.save()

    return HttpResponseRedirect(reverse("poll:results", args=[question_id]))
