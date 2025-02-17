import {Elm} from "./Compiler.elm.mjs";

const app = Elm.Compiler.init();

app.ports.println.subscribe((x) => {
    process.stdout.write(`${x}\n`);
});
