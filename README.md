Unison Codebase UI
==================

![CI](https://github.com/unisonweb/codebase-ui/workflows/CI/badge.svg)

Running
-------

Start `ucm` and copy the API URL and API Token (this URL is uniquely generated
by `ucm` at start-up) from the `ucm` start-up output (It's formatted as
a single URL with a query string. Get the token by from the query string).

Then start the development server, run:

```sh
API_URL="<API URL FROM UCM>" API_TOKEN="<API TOKEN FROM UCM>" npm start
```

Then visit `http://localhost:8000` to use the UI.

Generating Icon Sprite
----------------------

To add new icons, copy the svg markup to the `/public/img/icons.svg` file with
a wrapping `<symbol>` tag with an `id`. The color (`fill` or `stroke`) of the
shape must be
[`currentColor`](https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/color).

Additionally new icons added needs a new variant in `/src/UI/Icons.elm`.

This whole process is manual and not amazing, but happens rarely.
