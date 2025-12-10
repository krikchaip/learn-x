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

# Command cheatsheet 🧀

## Include only some statistics in the output/report

```sh
# displays only median, p95, and p99.9 values in the output
k6 run --iterations=100 --vus=10 --summary-trend-stats="med,p(95),p(99.9)" script.js
```

## Stream time series output to external services

reference: https://grafana.com/docs/k6/latest/get-started/results-output/#time-series-and-external-outputs

```sh
# can stream to multiple destinations at the same time!
k6 run \
  --out json=test.json \
  --out influxdb=http://localhost:8086/k6
```
