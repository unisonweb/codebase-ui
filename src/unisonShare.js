import "./css/unison-share.css";
import "./UI/CopyOnClick"; // Include web components
import detectOs from "./detectOs";
import preventDefaultGlobalKeyboardEvents from "./preventDefaultGlobalKeyboardEvents";
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
