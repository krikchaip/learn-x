# Work queues example

> **Work Queue** (also known as **Task Queue**) is a pattern used to distribute time-consuming tasks among multiple workers.

```ascii
                    +----+
                 -->| C1 |
                /   +----+
               /
+---+    +-------+
| P |--->| Queue |
+---+    +-------+
               \
                \   +----+
                 -->| C2 |
                    +----+
```

## Sending and receiving messages

> every dot in the payload will take one second to run...
> eg. "Hello..." will take 3 seconds.

### Round-robin dispatching

```sh
# first, run the worker script in 2 separate terminals
(shell_1) $ pipenv run python work_queues/worker.py
(shell_2) $ pipenv run python work_queues/worker.py
```

```sh
# in the third terminal we'll publish tasks to the queue.
# run all the following commands at once and observe output
# in the worker terminals.

pipenv run python work_queues/task.py "eiei."
pipenv run python work_queues/task.py "eiei.."
pipenv run python work_queues/task.py "eiei..."
pipenv run python work_queues/task.py "eiei...."
pipenv run python work_queues/task.py "eiei....."
```
