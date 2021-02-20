Unison Codebase UI
==================

![CI](https://github.com/unisonweb/codebase-ui/workflows/CI/badge.svg)

Running
-------

With a `ucm` running, run `npm start` in the `codebase-browser` directory to start the development server


Generating Icon Sprite
----------------------

To add new icons, copy the svg markup to the `/public/img/icons.svg` file with
a wrapping `<symbol>` tag with an `id`. The color (`fill` or `stroke`) of the
shape must be
[`currentColor`](https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/color).

Additionally new icons added needs a new variant in `/src/UI/Icons.elm`.

This whole process is manual and not amazing, but happens rarely.
