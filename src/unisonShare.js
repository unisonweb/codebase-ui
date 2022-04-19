import "./css/ui.css";
import "./css/themes/unison-light.css";
import "./css/code.css";
import "./css/unison-share.css";
import "./UI/CopyOnClick"; // Include web components
import detectOs from "./Lib/detectOs";
import preventDefaultGlobalKeyboardEvents from "./Lib/preventDefaultGlobalKeyboardEvents";
import { Elm } from "./UnisonShare.elm";

console.log(`
 _____     _
|  |  |___|_|___ ___ ___
|  |  |   | |_ -| . |   |
|_____|_|_|_|___|___|_|_|


`);

const basePath = new URL(document.baseURI).pathname;
const apiBasePath = ["api"];

const flags = {
  operatingSystem: detectOs(window.navigator),
  basePath,
  apiBasePath,
};

preventDefaultGlobalKeyboardEvents();

// The main entry point for the `UnisonShare` target of the Codebase UI.
Elm.UnisonShare.init({ flags });
