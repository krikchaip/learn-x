import sys

from pika.exchange_type import ExchangeType
from util import setup

connection, channel = setup()

# to enable pub/sub, we must declare an exchange of type 'fanout'.
# 'fanout' exchange broadcasts all the messages it receives to all the queues it knows
channel.exchange_declare(exchange="logs", exchange_type=ExchangeType.fanout)

# read log from command line arguments
MESSAGE = " ".join(sys.argv[1:]) or "Hello World!"

# a message can never be sent directly to the queue,
# it always needs to go through an exchange
channel.basic_publish(
    exchange="logs",  # publish a message to the 'logs' exchange
    routing_key="",  # we don't need a routing_key for 'fanout' exchanges
    body=MESSAGE,
)

# the messages will be lost if no queue is bound to the exchange yet,
# but that's okay for us; if no consumer is listening yet-
# we can safely discard the message.

print(f" [x] Sent {MESSAGE}")

connection.close()
