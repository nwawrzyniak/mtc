# simple-text-html
A *super* lightweight text post server for over-anonymous chatting
## Dependencies
You will need to install the following tools before building simple-text-html

- Node.js
- npm
- bower
- pulp
- purescript or purs
## Installation
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

## Install dependencies, build and run
To install all (missing) dependencies, (re-)build the programm and execute it (e.g. after a version update), you can also execute
```
./full_run.sh
```

## Annotations
The simple-text-html server is preconfigured to run only through the "Tor" network.
If you want to use this server through the clear web or in a local network you should change
`listenHostHttp (app db) port "127.0.0.1" \_ ->` to `listenHttp (app db) port \_ ->` in `Main.purs` and import `listenHttp`.
