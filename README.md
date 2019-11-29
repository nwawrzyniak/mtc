# simple-text-html
A *super* lightweight text post server for over-anonymous chatting

## Installation

### Pre-requirements

You will need `nodejs` (and `npm`), `purescript`, `pulp` and `bower`.

Start with installing [nodejs](http://www.purescript.org/) which should come with `npm`.

Then install purescript `npm install -g purescript`.
After that install pulp & bower `npm install -g pulp bower`.

### Dependencies
Either use `install_dep.sh` or do
```
npm install
bower install
```
inside the `node` folder of this repo.

### Installing

Install is more of a compile here, so use `build.sh` and you are done.

## Running the application
Either use `run.sh` or `pulp run` to start the server. See `run.sh` for how to pass the port
to the application, else it will use 8080 by default.
