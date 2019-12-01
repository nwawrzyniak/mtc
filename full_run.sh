#!/bin/bash

cd node
npm install
bower install
pulp build
pulp browserify -m FrontendMain -O -t static/main.js
PORT="${PORT:-8080}" pulp run
