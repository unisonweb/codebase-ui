import "./init";
import detectOs from "./detectOs";
import { Elm } from "./Hub.elm";

const basePath = new URL(document.baseURI).pathname;
const apiBasePath = ["api"];

const flags = {
  operatingSystem: detectOs(window.navigator),
  basePath,
  apiBasePath,
};

// The main entry point for the `hub` target of the Codebase UI.
Elm.Hub.init({ flags });
