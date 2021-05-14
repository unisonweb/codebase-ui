import "./init";
import detectOs from "./detectOs";
import { Elm } from "./UnisonShare.elm";

const basePath = new URL(document.baseURI).pathname;
const apiBasePath = ["api"];

const flags = {
  operatingSystem: detectOs(window.navigator),
  basePath,
  apiBasePath,
  appContext: "UnisonShare",
};

// The main entry point for the `UnisonShare` target of the Codebase UI.
Elm.UnisonShare.init({ flags });
