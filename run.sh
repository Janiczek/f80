#!/usr/bin/env bash

set -e
set -o pipefail

npx elm-esm make src/Compiler.elm --output=Compiler.elm.js
mv Compiler.elm.js Compiler.elm.mjs
node F80.mjs
