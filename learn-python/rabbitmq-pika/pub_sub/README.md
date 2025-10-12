# Publish/subscribe example

> The **publish/subscribe** (or broadcast) pattern is a messaging pattern where all **receivers** receive the same message from a publisher **simultaneously**.

```ascii
                             +---------+
                             | Queue#1 |-->...
                             +---------+
                            /
+-----------+   +----------+
| Publisher |-->| Exchange |
+-----------+   +----------+
                            \
                             +---------+
                             | Queue#2 |-->...
                             +---------+
```

## Sending and receiving messages

first, start two log receivers in a separate terminal.

```sh
(shell_1) $ pipenv run python pub_sub/receive_logs.py
(shell_2) $ pipenv run python pub_sub/receive_logs.py
```

run logs emitter in another terminal.

```sh
pipenv run python pub_sub/emit_log.py
```
