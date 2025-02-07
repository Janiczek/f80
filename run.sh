#!/usr/bin/env bash

set -e
set -o pipefail

npx elm-esm make src/F80.elm --output=F80.elm.js
mv F80.elm.js F80.elm.mjs
node F80.mjs
