#!/bin/bash

cd node
pulp build
pulp browserify -m FrontendMain -O | npx uglifyjs --compress --mangle --toplevel -o static/main.min.js
