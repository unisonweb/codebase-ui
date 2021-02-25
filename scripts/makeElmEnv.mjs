// Generate src/Env.elm based on a API_TOKEN environment variable.

import { writeFile } from "fs";

const ENV_FILE_PATH = "src/Env.elm";
const API_TOKEN = process.env.API_TOKEN;

function generateContent(apiToken) {
  return `module Env exposing (..)

-- DO NOT MODIFY This is an auto-generated environment file


apiToken : String
apiToken =
    "${apiToken}"
`;
}

function saveEnvFile(envFilePath, content) {
  const data = new Uint8Array(Buffer.from(content));
  writeFile(envFilePath, data, (err) => {
    if (err) console.error(`Could not save file: ${err}`);
  });
}

if (API_TOKEN) {
  saveEnvFile(ENV_FILE_PATH, generateContent(API_TOKEN));
} else {
  console.error(
    "Please provide an API_TOKEN environment variable. Grab it from the `ucm` startup output"
  );
}
