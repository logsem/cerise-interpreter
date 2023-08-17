# Cerise interpreter
This repository contains an interpreter of [Cerise](https://github.com/logsem/cerise), a model of a capability machine.

## Compiler branch
This branch is used for experimental purposes.

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

## Interactive interpreter
For an interactive version of the interpreter: `./interpreter -I <file>`

| Binding                 | Effect                           |
|-------------------------|----------------------------------|
| `ESC` or `q`            | exit                             |
| `SPACE`                 | next step                        |
| `n`                     | next 10 steps                    |
| `BACKSPACE`             | cancel the last step(s)          |
| `Arrow Up`              | navigate up memory (1 address)   |
| `Arrow Down`            | navigate down memory (1 address) |
| `Arrow Left`            | navigate up memory  (1 page)     |
| `Arrow Right`           | navigate down memory  (1 page)   |
| `Arrow Left` + `SHIFT`  | navigate up memory  (10 pages)   |
| `Arrow Right` + `SHIFT` | navigate down memory  (10 pages) |
| `TAB`                   | follow the cursor of PC          |
| `TAB` + `SHIFT`         | follow the cursor of STK         |


The `Arrow` keybindings can be combined with `CTLR` for navigating in the stack.
It is possible to scroll for navigating through the memory and the stack (depending on the position of cursor of the mouse). Combine mouse scroll + `CTLR` for navigating faster.
