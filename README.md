# simple-text-html
A *super* lightweight text post server for over-anonymous chatting
## Dependencies
You will need to install the following tools before building simple-text-html

- Node.js
- npm
- bower
- pulp
- purs
## Installation
After cloning this repository you will need to fetch a git submodule. This can be done with
```
git submodule update
```

Execute the following commands to build simple-text-html
```
./install_deps.sh
./build.sh
```
## Starting the server
To start the server just execute
```
./run.sh
```
By default it will listen on port 8080.

To specify a different port you can execute
```
PORT=XXXX ./run.sh
```
e.g.
```
PORT=1337 ./run.sh
```
to listen on port 1337 instead.

## Install dependencies, build and run
To install all (missing) dependencies, rebuild the program and execute it, you can also use
```
./full_run.sh
```

## Annotations
The simple-text-html server is preconfigured to run only through the "Tor" network.
If you want to use this server through the clear web or in a local network you should [follow this guide](https://github.com/nwawrzyniak/simple-text-html/wiki/simple-text-html-for-clear-web).
