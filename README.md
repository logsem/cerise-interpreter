# Cerise interpreter
This repository contains an interpreter of [Cerise](https://github.com/logsem/cerise), a model of a capability machine.

## Build the interpreter

Dependencies: opam

```
git clone https://github.com/logsem/cerise.git 
cd cerise
git checkout dev-interpreter
cd interpreter
opam switch create -y --repositories=default . ocaml-base-compiler.4.14.0
eval $(opam env --set-switch)
make
```

## Usage
Executable: `./_build/default/src/interactive <file>`
Assembly examples in `./tests` (for the syntax)

Press `SPACE` to take a step, and `ESC` to exit.
