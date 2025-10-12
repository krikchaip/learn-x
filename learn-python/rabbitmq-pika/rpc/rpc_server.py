import os

from pika import BasicProperties, spec
from pika.adapters.blocking_connection import BlockingChannel
from util import setup

connection, channel = setup()

RPC_QUEUE = "rpc_queue"

# this is the queue where the RPC server receives requests
channel.queue_declare(RPC_QUEUE)


def fib(n: int) -> int:
    match n:  # noqa
        case 0:
            return 0
        case 1:
            return 1
        case _:
            return fib(n - 1) + fib(n - 2)


def callback(
    ch: BlockingChannel,
    method: spec.Basic.Deliver,
    properties: spec.BasicProperties,
    body: bytes,
):
    n = int(body)

    print(f" [.] {RPC_QUEUE}({n})")

    response = fib(n)

    # sends messages back to the 'callback' queue
    ch.basic_publish(
        exchange="",
        routing_key=str(properties.reply_to),
        properties=BasicProperties(
            correlation_id=str(properties.correlation_id),
        ),
        body=str(response),
    )

    # send ACK to rabbitmq, once we're done with a task.
    # note: even if a worker is terminated for some reason,
    #       maybe while it was processing a message. the message is still available,
    #       and it will be redelivered to another worker instead.
    return ch.basic_ack(method.delivery_tag)


if __name__ == "__main__":
    try:
        # this tells rabbitmq not to give > 1 message to a worker at a time.
        # rabbitmq will dispatch a message to the next available worker instead.
        channel.basic_qos(prefetch_count=1)

        # setup a consumer before actually starting to receive messages
        # note: we removed `auto_ack=True` option and handled message ACK manually.
        channel.basic_consume(
            queue=RPC_QUEUE,
            on_message_callback=callback,
        )

        print(" [*] Awaiting RPC requests")

        # this is a BLOCKING operation
        channel.start_consuming()
    except KeyboardInterrupt:
        os._exit(0)
