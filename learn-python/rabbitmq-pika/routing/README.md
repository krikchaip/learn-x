# Routing example

> In this example, we'll use an exchange of type `direct` to make it possible for consumers to subscribe only to a subset of the messages.

```ascii
                                 +--------------------+
                +------error---->|   amq.gen-S9b...   |----->(C₁)
                |                +--------------------+
                |
       +--------+
(P)--->| direct |
       +--------+
                |
                +------warn----->+--------------------+
                |                |                    |
                +------info----->+   amq.gen-Ag1...   +----->(C₂)
                |                |                    |
                +------error---->+--------------------+
```

## Sending and receiving messages

saving only `warn` and `error` log messages to a file.

```sh
# -u is used to avoid buffering of the messages printed to the standard output
pipenv run python -u routing/receive_logs.py warn error | save --append warn_error.log
```

display all log messages on the screen.

```sh
pipenv run python routing/receive_logs.py warn error info
```

emit an `error` log message.

```sh
pipenv run python routing/emit_log.py error "Super 'based' log in the world!"
```
