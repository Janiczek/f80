import {Elm} from "./F80.elm.mjs";
import fs from "fs";

const filename = process.argv[2];

if (!filename) {
    console.error("Usage: node F80.mjs <filename.f80>");
    process.exit(1);
}

const file = fs.readFileSync(filename, "utf-8");

const app = Elm.F80.init({flags: {filename,file}});

app.ports.println.subscribe((x) => {
    process.stdout.write(`${x}\n`);
});

app.ports.writeFile.subscribe(({path,contents}) => {
    fs.writeFileSync(path, contents);
});
