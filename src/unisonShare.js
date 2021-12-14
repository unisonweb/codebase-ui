import "./init";
import "./css/unison-share.css";
import detectOs from "./detectOs";
import preventDefaultGlobalKeyboardEvents from "./preventDefaultGlobalKeyboardEvents";
import { Elm } from "./UnisonShare.elm";

const basePath = new URL(document.baseURI).pathname;
const apiBasePath = ["api"];

const flags = {
  operatingSystem: detectOs(window.navigator),
  basePath,
  apiBasePath,
  appContext: "UnisonShare",
};

preventDefaultGlobalKeyboardEvents();

// The main entry point for the `UnisonShare` target of the Codebase UI.
Elm.UnisonShare.init({ flags });
