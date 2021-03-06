import "./init";
import detectOs from "./detectOs";
import preventDefaultGlobalKeyboardEvents from "./preventDefaultGlobalKeyboardEvents";
import { Elm } from "./Ucm.elm";

const basePath = new URL(document.baseURI).pathname;

let apiBasePath;

if (basePath === "/") {
  apiBasePath = ["api"];
} else {
  apiBasePath = basePath
    .replace("ui", "api")
    .split("/")
    .filter((p) => p !== "");
}

const flags = {
  operatingSystem: detectOs(window.navigator),
  basePath,
  apiBasePath,
  appContext: "Ucm",
};

preventDefaultGlobalKeyboardEvents();

// The main entry point for the `ucm` target of the Codebase UI.
Elm.Ucm.init({ flags });
