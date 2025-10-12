import os
import sys

from pika.exchange_type import ExchangeType
from pika.spec import Basic
from util import LogSeverity, setup

EXCHANGE = "direct_logs"

connection, channel = setup()

# 'direct' exchange sends messages to the queue whose 'routing_key'
# EXACTLY MATCHES the 'routing_key' of the message.
channel.exchange_declare(exchange=EXCHANGE, exchange_type=ExchangeType.direct)

# create a new, temporary queue with a random name for each run.
declaration = channel.queue_declare(
    queue="",  # let rabbitmq choose a random name for us
    exclusive=True,  # DELETE this queue after the connection is closed
)

# select log severities to subscribe to from the command line arguments.
severities = [LogSeverity(arg) for arg in sys.argv[1:]]

if not severities:
    print(f"Usage: {sys.argv[0]} [info] [warning] [error]")
    os._exit(0)

# create a new binding for each severity we're interested in.
for severity in severities:
    channel.queue_bind(
        exchange=EXCHANGE,
        queue=declaration.method.queue,  # this contains a random queue name
        routing_key=severity,
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
