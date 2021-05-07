import "./init";
import system from "./system";
import { Elm } from "./Ucm.elm";

const flags = { system: system() };

// The main entry point for the `ucm` target of the Codebase UI.
Elm.Ucm.init({ flags });
