import "./assets/css/main.pcss";
import { Elm } from "./Main.elm";
import * as serviceWorker from "./serviceWorker";

Elm.Main.init({
  node: document.getElementById("root"),
  flags: {
    width: window.innerWidth,
    height: window.innerHeight,
  },
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.register();
