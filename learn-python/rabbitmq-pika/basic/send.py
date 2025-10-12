import os

from pika import BlockingConnection, ConnectionParameters, PlainCredentials

DAEMON_PORT = os.getenv("DAEMON_PORT", "5672")
USERNAME = os.getenv("USERNAME", "admin")
PASSWORD = os.getenv("PASSWORD", "password")

# create a connection, AKA. a client
credentials = PlainCredentials(username=USERNAME, password=PASSWORD)
parameters = ConnectionParameters(
    host="localhost",
    port=DAEMON_PORT,
    credentials=credentials,
)
connection = BlockingConnection(parameters=parameters)

# each connection can have multiple clients
channel = connection.channel()

# this is to ensure that the queue 'hello' exists before sending messages to it
# otherwise, RabbitMQ will just drop the message
channel.queue_declare(queue="hello")

# a message can never be sent directly to the queue,
# it always needs to go through an exchange
channel.basic_publish(
    exchange="",  # publish a message to the default exchange
    routing_key="hello",  # if using the default exchange, routing_key == queue
    body="Hello World!",
)

# don't forget to close the connection before exiting the program
connection.close()
