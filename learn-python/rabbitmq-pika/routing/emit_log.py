import sys

from pika.exchange_type import ExchangeType
from util import LogSeverity, setup

EXCHANGE = "direct_logs"

connection, channel = setup()

# 'direct' exchange sends messages to the queue whose 'routing_key'
# EXACTLY MATCHES the 'routing_key' of the message.
channel.exchange_declare(exchange=EXCHANGE, exchange_type=ExchangeType.direct)

# parse log severity from the first command line argument
severity = LogSeverity(sys.argv[1]) if len(sys.argv) > 1 else LogSeverity.INFO

# the actual log message is after the first argument
MESSAGE = " ".join(sys.argv[2:]) or "Hello World!"

# a message can never be sent directly to the queue,
# it always needs to go through an exchange
channel.basic_publish(
    exchange=EXCHANGE,
    routing_key=severity,
    body=MESSAGE,
)

# messages are lost if no queue is bound to the exchange
# or no matching routing key is found.

print(f" [x] Sent {severity}:{MESSAGE}")

connection.close()
