const { Elm } = require("./src/Main.elm");

document.addEventListener("DOMContentLoaded", () => {
    Elm.Main.init({
        node: document.getElementById("container"),
        flags: {
            winWidth: window.innerWidth,
            winHeight: window.innerHeight,
        }
    });
});
