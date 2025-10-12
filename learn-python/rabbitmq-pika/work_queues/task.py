import sys

from pika import BasicProperties, DeliveryMode
from util import setup

connection, channel = setup()

# declare a QUEUE that will PERSIST across rabbitmq restarts
channel.queue_declare(queue="task_queue", durable=True)

# mark messages as PERSISTENT -- i.e. tell rabbitmq to PERSIST messages on disk
properties = BasicProperties(delivery_mode=DeliveryMode.Persistent)

# read payload from command line arguments
MESSAGE = " ".join(sys.argv[1:]) or "Hello World!"

# a message can never be sent directly to the queue,
# it always needs to go through an exchange
channel.basic_publish(
    exchange="",  # publish a message to the default exchange
    routing_key="task_queue",  # if using the default exchange, routing_key == queue
    properties=properties,  # additional configs for publishing message
    body=MESSAGE,
)

print(f" [x] Sent {MESSAGE}")

connection.close()
