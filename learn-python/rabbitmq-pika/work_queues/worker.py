import os
import time

from pika import spec
from pika.adapters.blocking_connection import BlockingChannel
from util import setup

connection, channel = setup()

# declare a QUEUE that will PERSIST across rabbitmq restarts
channel.queue_declare(queue="task_queue", durable=True)


def callback(ch: BlockingChannel, method: spec.Basic.Deliver, properties, body: bytes):
    print(f" [x] ch {ch}")
    print(f" [x] method {method}")
    print(f" [x] properties {properties}")

    # display the payload (bytes) in utf-8 format
    print(f" [x] body {body.decode()}")

    # count dots in the payload and sleep for total count in seconds
    # note: b"..." syntax treats "..." as bytes instead of string
    time.sleep(body.count(b"."))

    print(" [x] Done")

    # send ACK to rabbitmq, once we're done with a task.
    # note: even if a worker is terminated for some reason,
    #       maybe while it was processing a message. the message is still available,
    #       and it will be redelivered to another worker instead.
    return ch.basic_ack(
        delivery_tag=method.delivery_tag,
    )


if __name__ == "__main__":
    try:
        # this tells rabbitmq not to give > 1 message to a worker at a time.
        # rabbitmq will dispatch a message to the next available worker instead.
        channel.basic_qos(prefetch_count=1)

        # setup a consumer before actually starting to receive messages
        # note: we removed `auto_ack=True` option and handled message ACK manually.
        channel.basic_consume(
            queue="task_queue",  # specify a queue to consume messages
            on_message_callback=callback,
        )

        print(" [*] Waiting for messages. To exit press CTRL+C")

        # this is a BLOCKING operation
        channel.start_consuming()
    except KeyboardInterrupt:
        os._exit(0)
