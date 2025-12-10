import { sleep } from "k6";

import { type Options } from "k6/options";
import http from "k6/http";

export const options: Options = {
  vus: 10, // run 10 default exported function (VU) simultaneously
  duration: "30s",
};

// wait for 1-second delay between requests
export default function () {
  http.get("http://test.k6.io");
  sleep(1);
}
