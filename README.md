# Cerise interpreter
This repository contains an interpreter of [Cerise](https://github.com/logsem/cerise), a model of a capability machine.

## Build the interpreter

Dependencies: opam

```
git clone https://github.com/logsem/cerise-interpreter.git 
cd cerise-interpreter
opam switch create -y --repositories=default . ocaml-base-compiler.4.14.0
eval $(opam env --set-switch)
make
```

Finally, the command `make install` creates a symbolic link to the interpreter in this repository.

## Usage
Executable: `./interpreter <file>`
Assembly examples in `./tests/test_files` (for the syntax)

The default version of the interpreter uses a version of Cerise with seals, uninitialized and directed capabilities. 
For a version of Cerise without these features, use `./interpreter --version vanilla`.

For more information about the options, `./interpreter --help`.

Press `SPACE` to take a step, and `ESC` to exit.
