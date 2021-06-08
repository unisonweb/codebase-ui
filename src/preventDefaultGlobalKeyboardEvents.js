/*
 * Firefox has binds for Ctrl+k, Cmd+k, and "/" We want to use those to open
 * the Finder.
 *
 * Unfortunately we can't do this in Elm, since Browser.Events doesn't support
 * preventDefault, so we're duplicating the shortcuts and calling
 * preventDefault in JS. The Elm handler picks up the event later on.
 *
 * TODO: This is a bit brittle and relies on the Elm handler being added after
 * this one. There might be a better solution build with ports for this.
 */

function preventDefaultGlobalKeyboardEvents() {
  window.addEventListener("keydown", (ev) => {
    if (
      ev.key === "/" ||
      (ev.metaKey && ev.key == "k") ||
      (ev.ctrlKey && ev.key == "k")
    ) {
      ev.preventDefault();
    }
  });
}

export default preventDefaultGlobalKeyboardEvents;
