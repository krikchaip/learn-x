import "~/styles/index.css";

import { createRoot } from "react-dom/client";

import App from "~/app";

const render = (id: string, elem: React.ReactNode) => {
  const mountElem = document.getElementById(id)!;
  return createRoot(mountElem).render(elem);
};

render("root", <App />);
