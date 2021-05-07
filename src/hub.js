import "./init";
import system from "./system";
import { Elm } from "./Hub.elm";

const flags = { system: system() };

// The main entry point for the `hub` target of the Codebase UI.
Elm.Hub.init({ flags });
