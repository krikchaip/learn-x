import os
import sys

from pika.exchange_type import ExchangeType
from pika.spec import Basic
from util import setup

EXCHANGE = "topic_logs"

connection, channel = setup()

# 'topic' exchange sends messages to the queue whose 'routing_key'
# MATCHES the 'routing_key' of the message.
channel.exchange_declare(exchange=EXCHANGE, exchange_type=ExchangeType.topic)

# create a new, temporary queue with a random name for each run.
declaration = channel.queue_declare(
    queue="",  # let rabbitmq choose a random name for us
    exclusive=True,  # DELETE this queue after the connection is closed
)

ROUTING_KEYS = sys.argv[1:]

if not ROUTING_KEYS:
    print(f"Usage: {sys.argv[0]} [binding_key]...")
    os._exit(0)

# create a new BINDING for each `routing_key` we're interested in.
for routing_key in ROUTING_KEYS:
    channel.queue_bind(
        exchange=EXCHANGE,
        queue=declaration.method.queue,  # this contains a random queue name
        routing_key=routing_key,
    )


def callback(_ch, method: Basic.Deliver, _properties, body: bytes):
    # get the routing_key from the method variable
    print(f" [x] {method.routing_key}: {body.decode()}")


if __name__ == "__main__":
    try:
        # setup a consumer before actually starting to receive messages
        # note: auto_ack=True is set because message loss is acceptable
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
