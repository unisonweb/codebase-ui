#!/usr/bin/env node

const fs = require("fs/promises");
const p = require("child_process");

function install(dep) {
  return `npx elm-json install ${dep.name}@${dep.version} --yes`;
}

run("npx elm-git-install")
  .then(() =>
    fs.readFile("./elm-stuff/gitdeps/github.com/unisonweb/ui-core/elm.json")
  )
  .then(JSON.parse)
  .then((uiCore) => {
    return Object.keys(uiCore.dependencies).map((name) => {
      // A version range looks like so: "1.0.0 <= v < 2.0.0"
      const versionRange = uiCore.dependencies[name];
      // take the major version
      let version = versionRange.split(".")[0];

      return { name, version };
    });
  })
  .then((deps) =>
    deps.reduce((p, d) => p.then((_) => run(install(d))), Promise.resolve())
  );

function run(cmd) {
  return new Promise((resolve, _reject) => {
    p.exec(cmd, { maxBuffer: 1024 * 500 }, (error, stdout, stderr) => {
      if (error) {
        console.warn(error);
      } else if (stdout) {
        console.log(stdout);
      } else {
        console.log(stderr);
      }
      resolve(stdout ? true : false);
    });
  });
}
