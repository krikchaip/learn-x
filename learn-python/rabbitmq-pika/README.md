# RabbitMQ Tutorial (Pika)

> https://www.rabbitmq.com/tutorials

## Installation and setup

```sh
# setup virtualenv and install packages
mkdir .venv
pipenv install

# enter pipenv shell before starting nvim
pipenv shell
```

```sh
# start both rabbitmq daemon and management dashboard
docker compose up -d
```

Now you can visit the management dashboard at `http://localhost:15672`.

## Debugging RabbitMQ

```sh
# listing queues
(docker_compose) $ rabbitmqctl list_queues

# check for forgotten acknowledgments
# i.e. missing calls to ch.basic_ack() when auto_ack=False
# columns: name, messages_ready, messages_unacknowledged
(docker_compose) $ rabbitmqctl list_queues name messages_ready messages_unacknowledged

# listing exchanges
(docker_compose) $ rabbitmqctl list_exchanges

# listing bindings
(docker_compose) $ rabbitmqctl list_bindings
```

## Sending and receiving messages

- [basic example](basic/README.md)
- [work queues (aka. task queues)](work_queues/README.md)
- [publish/subscribe (aka. broadcast)](pub_sub/README.md)
- [routing example](routing/README.md)
- [topics (aka. `*` and `#`)](topics/README.md)
- [rpc (remote procedure call)](rpc/README.md)

## Note on the use of `auto_ack=True`

```py
# ...some imaginary rabbmitmq consumer
channel.basic_consume(..., auto_ack=True)
```

- once RabbitMQ delivers message to this consumer, it **immediately** marks the message for deletion.
- say, if you terminate this worker while it's processing some message, the message will be lost.

in order to fix the above issues, your consumers need to send `ACK` to rabbitmq manually. see `work_queues` example for more details.
