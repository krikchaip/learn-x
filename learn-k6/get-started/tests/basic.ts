import { sleep } from "k6";

import { type Options } from "k6/options";
import http from "k6/http";

export const options: Options = {
  iterations: 10, // execute the default function 10 times (1 VU)
};

// wait for 1-second delay between requests
export default function () {
  http.get("https://quickpizza.grafana.com");
  sleep(1);
}
