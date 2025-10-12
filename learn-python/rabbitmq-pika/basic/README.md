# Basic example

```ascii
+------+     +-------+     +------+
|  P   | --> | hello | --> |  C   |
+------+     +-------+     +------+
```

## Sending and receiving messages

```sh
# first, let's start a consumer...
pipenv run python basic/receive.py

# then, start the producer in a new terminal
pipenv run python basic/send.py
```
