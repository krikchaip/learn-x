import http from "k6/http";

export default function () {
  const url = "http://test.k6.io/login";

  // must serialize the request body before sending
  const payload = JSON.stringify({
    email: "aaa",
    password: "bbb",
  });

  const params = {
    headers: {
      "Content-Type": "application/json",
    },
  };

  http.post(url, payload, params);
}
