import {Elm} from "./F80.elm.mjs";

const app = Elm.F80.init();

app.ports.println.subscribe((x) => {
    process.stdout.write(`$x\n`);
});
