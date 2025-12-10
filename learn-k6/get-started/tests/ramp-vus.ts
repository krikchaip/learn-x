import { sleep, check } from "k6";

import { type Options } from "k6/options";
import http from "k6/http";

export const options: Options = {
  stages: [
    { target: 20, duration: "5s" }, // 0->20 VUs for 5s
    { target: 10, duration: "10s" }, // 20->10 VUs for 10s
    { target: 0, duration: "15s" }, // 10->0 VUs for 15s
  ],
};

// check if response is successful and wait for 1-second after
export default function () {
  const res = http.get("https://quickpizza.grafana.com");

  check(res, {
    "status was 200": (r) => r.status === 200,
  });

  sleep(1);
}
