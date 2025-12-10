# K6 get started

To install dependencies:

```bash
bun install
```

To run:

```bash
# run with configurations defined in the file
k6 run tests/<your_test_case>.ts

# run with cli options
k6 run --vus 10 --duration 30s tests/<your_test_case>.ts
```

This project was created using `bun init` in bun v1.3.1. [Bun](https://bun.com) is a fast all-in-one JavaScript runtime.
