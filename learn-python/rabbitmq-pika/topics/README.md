# Topics example

> **Producer**: messages sent to a `topic` exchange can't have an arbitrary `routing_key` - it must be a list of words, delimited by dots, eg. `stock.usd.nyse`, `nyse.vmw`, `quick.orange.rabbit`.

> **Consumer**: functions like a `direct` exchange, but its `binding_key` (routing key) allows for the following special characters.
>
> - `*` can substitute for **EXACTLY** one word
> - `#` can substitute for **ZERO OR MORE** words

```ascii
                                +--------------------+
               +---*.orange.*-->|   amq.gen-S9b...   |----->(C₁)
               |                +--------------------+
               |
       +-------+
(P)--->| topic |
       +-------+
               |
               +---*.*.rabbit-->+--------------------+
               |                |                    |
               +-----lazy.#---->+   amq.gen-Ag1...   +----->(C₂)
                                |                    |
                                +--------------------+
```

Example messages with the folowing `routing_key`:

- `quick.orange.rabbit` -> will be delivered to **BOTH** queues
- `lazy.orange.elephant` -> will be delivered to **BOTH** queues
- `quick.orange.fox` -> will only go to the **FIRST** queue
- `lazy.brown.fox` -> will only go to the **SECOND** queue
- `lazy.pink.rabbit` -> will only go to the **SECOND** queue **ONCE**
- `quick.brown.fox` -> will be discarded ❌
- `orange` -> will be discarded ❌
- `quick.orange.new.rabbit` -> will be discarded ❌
- `lazy.orange.new.rabbit` -> will only go to the **SECOND** queue

## Sending and receiving messages

> Routing key structure: `<service>.<severity>`

```sh
# receive ALL the logs
pipenv run python topics/receive_logs.py "#"

# receive all the logs from the "kern" service
pipenv run python topics/receive_logs.py "kern.*"

# receive only "critical" logs
pipenv run python topics/receive_logs.py "*.critical"

# receive logs from multiple bindings
pipenv run python topics/receive_logs.py "*.warn" "os.*"
```

```sh
# emitting a log with some routing_key
pipenv run python topics/emit_log.py "kern.critical" "I feel so high RN..."
```
