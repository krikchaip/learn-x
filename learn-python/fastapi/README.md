# FastAPI Tutorial

> Quick start: https://fastapi.tiangolo.com
> Tutorials: https://fastapi.tiangolo.com/learn

## Setup the project

```sh
# create and activate virtualenv using pipenv
mkdir .venv
pipenv shell

# install FastAPI ⚡️
pipenv install "fastapi[standard]"
```

**`standard` dependencies?**

- see further -> [dependencies](https://fastapi.tiangolo.com/#dependencies)

## Run the server

```sh
# start server in 'dev' mode
fastapi dev main.py

# start server in 'prod' mode
fastapi run main.py

# start server with specified port
fastapi dev --port 3000 main.py
```

## Live API Docs 🔥

- Swagger UI -> visit `http://localhost:8000/docs`
- Redoc -> visit `http://localhost:8000/redoc`
- OpenAPI JSON schema -> visit `http://localhost:8000/openapi.json`

## Terms

- **path operation functions** -> AKA. request handlers, views (django term)
