NODE_REDIS:=app

.PHONY: build
build:
	@echo Start building node-redis...
	@docker build                 \
		-f ${NODE_REDIS}/Dockerfile \
		-t node-redis:latest     \
		${NODE_REDIS}
	@echo Loading node-redis into minikube...
	@minikube image load node-redis:latest

.PHONY: deploy
deploy:
	@kubectl apply -f k8s/secrets/database.yaml
	@kubectl apply -f k8s/deployments/redis-server.yaml
	@kubectl apply -f k8s/deployments/node-redis.yaml
	@kubectl apply -f k8s/ingresses/api.yaml
