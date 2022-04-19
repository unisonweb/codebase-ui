import "./css/ui.css";
import "./css/themes/unison-light.css";
import "./css/code.css";
import "./css/unison-local.css";
import "./UI/CopyOnClick"; // Include web components
import detectOs from "./Lib/detectOs";
import preventDefaultGlobalKeyboardEvents from "./Lib/preventDefaultGlobalKeyboardEvents";
import { Elm } from "./UnisonLocal.elm";

console.log(`
 _____     _
|  |  |___|_|___ ___ ___
|  |  |   | |_ -| . |   |
|_____|_|_|_|___|___|_|_|


`);

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
};

preventDefaultGlobalKeyboardEvents();

// The main entry point for the `UnisonLocal` target of the Codebase UI.
Elm.UnisonLocal.init({ flags });
