import os

from pika.exchange_type import ExchangeType
from util import setup

connection, channel = setup()

# to enable pub/sub, we must declare an exchange of type 'fanout'.
# 'fanout' exchange broadcasts all the messages it receives to all the queues it knows
channel.exchange_declare(exchange="logs", exchange_type=ExchangeType.fanout)

# create a new, temporary queue with a random name for each run.
declaration = channel.queue_declare(
    queue="",  # let rabbitmq choose a random name for us
    exclusive=True,  # DELETE this queue after the connection is closed
)

# bind this temporary queue to the 'logs' exchange
channel.queue_bind(
    exchange="logs",  # subscribe this queue to 'logs'
    queue=declaration.method.queue,  # this contains a random queue name
)


def callback(_ch, _method, _properties, body: bytes):
    # display the payload (bytes) in utf-8 format
    print(f" [x] body {body.decode()}")


if __name__ == "__main__":
    try:
        # setup a consumer before actually starting to receive messages
        # note: auto_ack=True is set because message loss is acceptable in pub/sub
        channel.basic_consume(
            queue=declaration.method.queue,
            on_message_callback=callback,
            auto_ack=True,
        )

        print(" [*] Waiting for messages. To exit press CTRL+C")

        # this is a BLOCKING operation
        channel.start_consuming()
    except KeyboardInterrupt:
        os._exit(0)
