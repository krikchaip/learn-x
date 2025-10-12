# RPC example

```ascii
+--------+   +--------------------------+   +-----------+   +--------+   +-----------+
| Client |-->| Request                  |-->| rpc_queue |-->| Server |-->| amq.gen...|
+--------+   | reply_to=amq.gen...      |   +-----------+   +--------+   +-----------+
    ↑        | correlation_id=abc       |                                       |
    |        +--------------------------+                                       |
    |                                                                           |
    |        +--------------------------+                                       |
    +--------| Reply                    |<--------------------------------------+
             | correlation_id=abc       |
             +--------------------------+
```

## Sending and receiving messages

first, start RPC servers to handle RPC requests.

> **Note**: If the RPC server is too slow, you can scale up by just running another one.

```sh
(shell_1) $ pipenv run python rpc/rpc_server.py
(shell_2) $ pipenv run python rpc/rpc_server.py

# ...
```

to request a fibonacci number run the client:

```sh
$ pipenv run python -i rpc/rpc_client.py

# in python shell...
>>> fib_client = RpcClient()
>>> fib_client.call(3)

# [x] Requesting rpc_queue(3)
# [.] Got b'2'
```
