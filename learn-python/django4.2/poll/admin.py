from django.contrib import admin

from .models import Choice, Question


class ChoiceInline(admin.TabularInline):
    """
    `StackedInline` -> each attribute is displayed line-by-line
    `TabularInline` -> table-like UI
    """

    # REQUIRED
    model = Choice

    # number of placeholders to display
    extra = 2


class QuestionAdmin(admin.ModelAdmin):
    """
    Pass this subclass to `admin.site.register` to customize the admin page
    """

    """
    Attributes for Create/Edit page
    """

    # fields to be displayed
    # fields = ["pub_date", "question_text"]

    # group fields into a section
    fieldsets = [
        (None, {"fields": ["question_text"]}),
        ("Date information", {"fields": ["pub_date"]}),
    ]

    # embedding related objects
    inlines = [ChoiceInline]

    """
    Attributes for List page
    default: displays the `str()` of each object
    """

    # attributes to be displayed (as columns)
    list_display = [
        "question_text",
        "pub_date",  # we set `name="date published"` in the model
        "was_published_recently",  # method works too!
    ]

    # add filter menu on the right
    list_filter = ["pub_date"]

    # adds a search box at the top of the list page
    search_fields = ["question_text"]


admin.site.register(Question, QuestionAdmin)
