import os

from pika import BlockingConnection, ConnectionParameters, PlainCredentials
from pika.adapters.blocking_connection import BlockingChannel

DAEMON_PORT = os.getenv("DAEMON_PORT", "5672")
USERNAME = os.getenv("USERNAME", "admin")
PASSWORD = os.getenv("PASSWORD", "password")


def create_connection() -> BlockingConnection:
    credentials = PlainCredentials(username=USERNAME, password=PASSWORD)
    parameters = ConnectionParameters(
        host="localhost",
        port=DAEMON_PORT,
        credentials=credentials,
    )
    connection = BlockingConnection(parameters=parameters)

    return connection


def create_channel(connection: BlockingConnection) -> BlockingChannel:
    return connection.channel()


def setup():
    connection = create_connection()
    channel = create_channel(connection)

    return connection, channel
