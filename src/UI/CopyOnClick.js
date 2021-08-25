// <copy-on-click text="text-to-copy">
//   clickable content
// </copy-on-click>
//
// Use from Elm with an Icon:
// node "copy-on-click" [ ] [ UI.Icon.view UI.Icon.clipboard ]
class CopyOnClick extends HTMLElement {
  constructor() {
    super();
  }

  connectedCallback() {
    this.addEventListener("click", () => {
      const text = this.getAttribute("text");

      // writeText returns a promise with success/failure that we should
      // probably do something with...
      navigator.clipboard.writeText(text);
    });
  }

  static get observedAttributes() {
    return ["text"];
  }
}

customElements.define("copy-on-click", CopyOnClick);
