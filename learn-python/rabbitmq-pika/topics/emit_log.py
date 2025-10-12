import sys

from pika.exchange_type import ExchangeType
from util import setup

EXCHANGE = "topic_logs"

connection, channel = setup()

# 'topic' exchange sends messages to the queue whose 'routing_key'
# MATCHES the 'routing_key' of the message.
channel.exchange_declare(exchange=EXCHANGE, exchange_type=ExchangeType.topic)

ROUTING_KEY = sys.argv[1] if len(sys.argv) > 2 else "anonymous.info"
MESSAGE = " ".join(sys.argv[2:]) or "Hello World!"

# a message can never be sent directly to the queue,
# it always needs to go through an exchange
channel.basic_publish(
    exchange=EXCHANGE,
    routing_key=ROUTING_KEY,
    body=MESSAGE,
)

# messages are lost if no queue is bound to the exchange
# or no matching routing key is found.

print(f" [x] Sent {ROUTING_KEY}:{MESSAGE}")

connection.close()
