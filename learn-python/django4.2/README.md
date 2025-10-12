# Django V4.2 Tutorial

> https://docs.djangoproject.com/en/4.2/intro

## Setup the project

### From scratch

```sh
# create virtual environment (all 3rd party packages will be downloaded here)
mkdir .venv

# we will use 'pipenv' as our package manager
pip install pipenv

# install and set python version for this project
pipenv --python 3.9.21

# verify project's python version using this command
pipenv run python -V

# install django@4.2
pipenv install django~=4.2

# activate virtual environment to ensure project binaries are accessible
# P.S. for `pyright` LSP to function, nvim must be launched from within this shell
pipenv shell
```

### Generate Django projects/apps

> [!TIP] Project
> A collection of settings for an instance of Django, including database configuration,
> Django-specific options and application-specific settings.

```sh
# requires virtual environment to be activated first
django-admin startproject <project name>
```

> [!TIP] Application
> An app is a web application that does something – e.g., a blog system.
> While a project is a collection of configuration and apps for a particular website.

> A project can contain multiple apps. An app can be in multiple projects.

```sh
python manage.py startapp <app name>
```

## Start development server

> [!ATTENTION] Virtual environment
> The commands below require a virtual environment to be activated.

```sh
# start a server at 127.0.0.1:8000 (default)
python manage.py runserver

# start a server at 127.0.0.1:3000
python manage.py runserver 3000

# for containerized environment (eg. docker)
python manage.py runserver 0.0.0.0:5555
```

## Database integration

### Database drivers

- `postgresql` -> [psycopg](https://www.psycopg.org/psycopg3) (see [PostgreSQL notes](https://docs.djangoproject.com/en/4.2/ref/databases/#postgresql-notes) for further details)

Note: you need to change some keys in the [DATABASES](https://docs.djangoproject.com/en/4.2/ref/settings/#std-setting-DATABASES) config to match your database connection settings.

### How to generate and run migrations

```sh
# generate migration files for all apps specified in INSTALLED_APPS
python manage.py makemigrations

# generate migration files for "some_app"
python manage.py makemigrations "some_app"

# run migrations for all applications specified in INSTALLED_APPS
python manage.py migrate
```

### Inspect generated SQL output

```sh
python manage.py sqlmigrate <app_name> <migration_name eg. 0001>
```

### Project health check

> [!INFO] 'check' command
> useful for checking any problems in your project without making migrations or touching the database.

```sh
python manage.py check
```

## Model APIs

```py
# support for time zones is enabled in the default settings file,
# so django expects a datetime with tzinfo for DateTimeField.
# use timezone.now() instead of datetime.datetime.now()
from django.utils import timezone

from poll.models import Choice, Question

# get all rows
Question.objects.all() # <QuerySet [...]>

# query with a condition (like WHERE statement in sql)
Question.objects.filter(id=1)
Question.objects.filter(question_text__startswith="What")
Question.objects.filter(pub_date__year=timezone.now().year)

# ORDER BY statement
Question.objects.order_by("-pub_date") # ORDER BY pub_date DESC

# it can also be chained with .filter
Question.objects.filter(choice__votes=0).order_by("-pub_date")

# get one by a condition
Question.objects.get(id=2)

# shortcut for primary-key exact lookups 👍
# (identical to the above expression)
Question.objects.get(pk=2)

# create and save a record
q = Question(question_text="What's new?", pub_date=timezone.now())
q.save()

q.id            # now it has an ID (PK)
q.question_text # "What's new?"
q.pub_date      # datetime.datetime(..., tzinfo=datetime.timezone.utc)

# modify a record
q.question_text = "What's up?"
q.save()

# delete a record
q.delete()
```

### Model relation queries

```py
# display one-to-many related objects
q.choice_set.all()
q.choice_set.count()
q.choice_set.filter(choice_text__startswith="Just hacking")

# create model-related objects
q.choice_set.create(choice_text="Not much", votes=0)
q.choice_set.create(choice_text="The sky", votes=0)
c = q.choice_set.create(choice_text="Just hacking again", votes=0)

# access the related object
c.question
c.question_id

# use double underscores to separate relationships
# this works as many levels deep as you want; there's no limit 👍

# "find all choices for any question whose 'pub_date' is in this year"
Choice.objects.filter(question__pub_date__year=timezone.now().year)
```

### Model string representation

```py
from django.db import models

class MyModel(models.Model):
    my_field = models.IntegerField()

    def __str__(self):
        return self.my_field

m = MyModel(my_field=3) # example usage
print(m)                # <MyModel: 3>
```

### Preventing race conditions

> See: https://docs.djangoproject.com/en/4.2/ref/models/expressions/#avoiding-race-conditions-using-f

## Django Admin

```sh
# create super user for the admin dashboard
python manage.py createsuperuser

# (optional) change admin user password
python manage.py changepassword <username>

# start django server and visit "/admin/"
# eg. "localhost:8000/admin/"
```

## Testing

### Test files

Django automatically discovers tests within any `django.test.TestCase` subclass.
For a test method to be found, its name must begin with `test`.

Django searches for these test methods in the following locations:

- An application's `tests.py` file.
- Any Python file whose name starts with `test_` (e.g., `test_question.py`, `choice/test_choice_unit.py`).

### Running tests

```sh
# run all tests inside a project
python manage.py test

# run application tests
python manage.py test <app_name>
```

NOTE: The database is reset **AFTER EACH** test method is run. (unless `--keepdb` flag is specified)

### Testing views

Manually through Django `shell` session.

```py
>>> from django.test.utils import setup_test_environment
>>> from django.test import Client
>>> from django.urls import reverse
>>>
>>> setup_test_environment()
>>> client = Client()
>>>
>>> response = client.get("/")
# Not Found: /
>>> response.status_code
# 404
>>> response = client.get(reverse("poll:index"))
>>> response.status_code
# 200
>>> response.content
# b'...some HTML junkie :)'
>>> response.context["latest_question_list"]
# <QuerySet [<Question: eiei>, ...]>
```

Notes:

**`setup_test_environment()`**

- allows us to examine some additional attributes on `HttpResponse`
  - eg. `response.context`, `response.status_code` and `response.content`
- **DOES NOT** set up a test database.
  - the output will come right from the data in the existing database

**`Client()`**

- Django HTTP test client
- this is not required when writing tests within `django.test.TestCase` methods
  - instead, use `self.client`
