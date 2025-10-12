import os

from pika import BlockingConnection, ConnectionParameters, PlainCredentials

DAEMON_PORT = os.getenv("DAEMON_PORT", "5672")
USERNAME = os.getenv("USERNAME", "admin")
PASSWORD = os.getenv("PASSWORD", "password")

# the code responsible for connecting to rabbitmq is the same as send.py
credentials = PlainCredentials(username=USERNAME, password=PASSWORD)
parameters = ConnectionParameters(
    host="localhost",
    port=DAEMON_PORT,
    credentials=credentials,
)
connection = BlockingConnection(parameters=parameters)

channel = connection.channel()

# BEST PRACTICE 👍🏻
# this is an idempotent operation. you can call this as many as you like,
# and only one queue will be created.
channel.queue_declare(queue="hello")


def callback(ch, method, properties, body):
    print(f" [x] ch {ch}")
    print(f" [x] method {method}")
    print(f" [x] properties {properties}")
    print(f" [x] body {body}")


if __name__ == "__main__":
    try:
        # setup a consumer before actually starting to receive messages
        channel.basic_consume(
            queue="hello",  # specify a queue to consume messages
            auto_ack=True,  # see ../README.md for details
            on_message_callback=callback,
        )

        print(" [*] Waiting for messages. To exit press CTRL+C")

        # this is a BLOCKING operation
        channel.start_consuming()
    except KeyboardInterrupt:
        os._exit(0)
