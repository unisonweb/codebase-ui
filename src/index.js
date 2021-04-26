require("./index.html");

var Elm = require("./Main.elm").Elm;

var app = Elm.Main.init({
  node: document.querySelector("main"),
});
