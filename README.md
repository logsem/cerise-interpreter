# Cerise interpreter
This repository contains an interpreter of [Cerise](https://github.com/logsem/cerise), a model of a capability machine.

## Build the interpreter

Dependencies: opam

```
git clone https://github.com/logsem/cerise-interpreter.git 
cd cerise-interpreter
opam switch create -y --repositories=default . ocaml-base-compiler.5.4.1
eval $(opam env --set-switch)
make
```

Finally, the command `make install` creates a symbolic link to the interpreter in this repository.

## Usage
Executable: `./interpreter <file>`
Assembly examples in `./tests/test_files` (for the syntax)

See [assembler.md](assembler.md) for the complete Griotte assembly language, register-file, integer
definition, and sequence-macro reference.

The interpreter uses the Griotte capability-machine configuration.

Example:

```
./interpreter -I --regfile ./tests/test_files/case_studies/mutually_distrustful.reg ./tests/test_files/case_studies/mutually_distrustful.s
```

For more information about the options, `./interpreter --help`.

## Machine backends

The default backend is handwritten. The `extracted` backend uses Griotte's
Rocq-extracted step function:

```sh
./interpreter --backend extracted program.s
```

The generated OCaml files are committed to this repository. To refresh them
from the latest Griotte `interpreter-extraction` branch, run:

```sh
make regenerate-extracted
```

This command needs Git, Nix, and network access. It clones Griotte into a
temporary directory, uses Griotte's Nix extraction package, and removes the
clone when finished. Set `GRIOTTE_URL` or `GRIOTTE_BRANCH` to refresh from
another repository or branch.

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

## Using Nix

To compile

```
nix build
```

To run
```
nix run #. -- <args>
```
For instance
```
nix run .# -- -I --regfile ./tests/test_files/case_studies/mutually_distrustful.reg ./tests/test_files/case_studies/mutually_distrustful.s
```

To format

```
nix develop --command dune fmt
```
