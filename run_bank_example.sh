#!/usr/bin/env sh

cp ../../cerise-compilation/extract.ml ./lib/extract.ml
make no-warning
./compile
./interactive --regfile asm-toys/bank_loaded.reg asm-toys/bank_loaded.s
# ./compile > l.s
# ./interactive --regfile tests/test_files/l.reg l.s
# ./compile_bank > asm-toys/bank_mod.s
# ./compile_adv > asm-toys/adv.s
# python3 linker.py asm-toys/bank_mod.s asm-toys/adv.s > asm-toys/bank.s
# ./interactive --regfile tests/test_files/bank_example.reg asm-toys/bank.s
