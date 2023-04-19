#!/usr/bin/env sh

cp ../../cerise-compilation/extract.ml ./lib/extract.ml
make no-warning
./compile
# ./interactive --regfile asm-toys/bank_loaded.reg asm-toys/bank_loaded.s
./interactive --regfile asm-toys/stack_loaded.reg asm-toys/stack_loaded.s
# ./interactive --regfile asm-toys/dummy.reg asm-toys/dummy.s
