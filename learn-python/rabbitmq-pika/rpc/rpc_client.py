import uuid
from typing import Any, cast

from pika import BasicProperties, BlockingConnection, spec
from pika.adapters.blocking_connection import BlockingChannel
from util import setup

RPC_QUEUE = "rpc_queue"


class RpcClient:
    connection: BlockingConnection
    channel: BlockingChannel

    callback_queue: str
    correlation_id: str
    response: Any

    def __init__(self):
        self.connection, self.channel = setup()

        # create a new, temporary queue with a random name for each instance.
        declaration = self.channel.queue_declare(
            queue="",  # let rabbitmq choose a random name for us
            exclusive=True,  # DELETE this queue after the connection is closed
        )

        # this contains a random queue name.
        self.callback_queue = cast(str, declaration.method.queue)

        # setup the callback queue for this RPC client.
        # P.S. for `auto_ack=True`, this tells rabbitmq not to re-queue
        # the message if the RPC client fails.
        self.channel.basic_consume(
            queue=self.callback_queue,
            on_message_callback=self.on_response,
            auto_ack=True,
        )

    def on_response(
        self,
        ch: BlockingChannel,  # pylint: disable=unused-argument
        method: spec.Basic.Deliver,  # pylint: disable=unused-argument
        properties: spec.BasicProperties,
        body: bytes,
    ):
        # simply ignore all unrelated results by checking correlation_id
        if properties.correlation_id == self.correlation_id:
            self.response = body

    def call(self, arg: Any):
        # clear the response before calling the RPC server
        self.response = None

        # generate a random unique correlation_id for each RPC call.
        # the `on_response` callback function will use this value
        # to catch the appropriate response.
        self.correlation_id = str(uuid.uuid4())

        print(f" [x] Requesting {RPC_QUEUE}({str(arg)})")

        # send a message to the rpc_queue with properties
        self.channel.basic_publish(
            exchange="",
            routing_key=RPC_QUEUE,
            properties=BasicProperties(
                reply_to=self.callback_queue,
                correlation_id=self.correlation_id,
            ),
            body=str(arg),
        )

        # wait until the proper response arrives and return the response back to the user
        while self.response is None:
            self.connection.process_data_events(time_limit=None)  # type:ignore

        print(f" [.] Got {str(self.response)}")

        return self.response
