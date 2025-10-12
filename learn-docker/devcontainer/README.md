# Docker & Kubernetes Course

Udemy Link: [Docker and Kubernetes the complete guide](https://udemy.com/course/docker-and-kubernetes-the-complete-guide)

## Commands

### Build the app

```bash
$ docker rmi $(basename $PWD); docker build . \
  --no-cache \
  --progress plain \
  -t $(basename $PWD) \
  -f build/Dockerfile.prod
```

### Run the app container

```bash
# assumes redis already running inside docker
$ docker run --rm -it \
  --name node-app-prod \
  --network docker-kubernetes-course_default \
  -p 3000:4001 \
  -e PORT=4001 \
  -e REDIS_URL=redis://redis-server:6379 \
  $(basename $PWD)
```

### Redis CLI

```bash
$ docker compose exec -it redis-server redis-cli
```
