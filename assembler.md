# Griotte assembler reference

This document describes the assembly and register-file syntax accepted by the interpreter. Complete
programs are available under `tests/test_files`.

## Source text

Whitespace separates tokens but is otherwise insignificant. A semicolon starts a comment that
continues to the end of the line. Integer literals may be decimal or hexadecimal and must fit in an
OCaml machine integer when read. Identifiers and labels are case-sensitive.

```asm
mov r1 42
mov r2 0x2a ; hexadecimal
loop:
```

Griotte only supports finite integers. `Inf`, `inf`, and `∞` are rejected by the lexer.

## Registers and values

General registers are `r0` through `r31`, plus `pc` and the case-insensitive CHERIoT aliases:

| Aliases | Registers |
|---|---|
| `cnull`, `cra`, `csp`, `cgp`, `ctp` | `r0`, `r1`, `r2`, `r3`, `r4` |
| `ct0`–`ct2`, `ct3`–`ct6` | `r5`–`r7`, `r28`–`r31` |
| `cs0`–`cs1`, `cs2`–`cs11` | `r8`–`r9`, `r18`–`r27` |
| `ca0`–`ca7` | `r10`–`r17` |

The system register is `mtdc`. An instruction value accepts a general register, an expression, or an
encoded architectural value:

| Category | Syntax |
|---|---|
| Capability permission | `O` or `[rx w dl dro]` |
| `rx` component | `Orx`, `R`, `X`, `XSR` |
| `w` component | `Ow`, `W`, `WL` |
| `dl` component | `DL`, `LG` |
| `dro` component | `DRO`, `LM` |
| Sealing permission | `SO`, `S`, `U`, `SU` |
| Locality | `GLOBAL`, `LOCAL` (or title case) |
| Word type | `Int`, `Cap`, `SealRange`, `Sealed`, `Sentry` |
| Permission/locality pair | `([R W DL DRO], GLOBAL)` |
| Sealing-permission/locality pair | `(S, LOCAL)` |

## Instructions

Here `r` is a general register, `sr` a system register, and `v` a value. Operands are separated by
whitespace rather than commas.

| Form | Meaning |
|---|---|
| `jalr r r` | jump and link |
| `jmp v` | jump through a register or to an immediate target |
| `jnz r v` | conditional jump |
| `readsr r sr` | read a system register |
| `writesr sr r` | write a system register |
| `mov r v` | move a register or constant |
| `load r r` | load through a capability |
| `store r v` | store through a capability |
| `add r v v`, `sub r v v`, `mul r v v` | arithmetic |
| `rem r v v`, `div r v v` | remainder and division |
| `land r v v`, `lor r v v` | bitwise operations |
| `lshiftl r v v`, `lshiftr r v v` | shifts |
| `lt r v v` | less-than comparison |
| `lea r v` | modify a capability address |
| `restrict r v` | restrict permission/locality |
| `subseg r v v` | restrict capability bounds |
| `getl r r`, `getb r r`, `gete r r`, `geta r r` | inspect capability fields |
| `getp r r`, `getotype r r`, `getwtype r r` | inspect permission/type fields |
| `seal r r r`, `unseal r r r` | seal and unseal |
| `fail`, `halt` | enter the failed or halted state |

## Expressions and labels

Expressions contain finite integers, labels, parentheses, unary `-`, and these binary operators:

```text
+  -  *  &&  ||  <<  >>
```

Use parentheses around a compound instruction operand:

```asm
start:
mov r1 (end - start)
halt
end:
```

A label denotes the address of the next emitted instruction or word. Labels may be referenced before
their declarations and do not themselves emit a word.

## Literal words

Prefix literal program data with `#`:

```asm
# 42
# ([R W DL DRO], GLOBAL, 0, 20, 0)
# [SU, LOCAL, 0, 10, 0]
# (E-[X Ow DL DRO], GLOBAL, 0, 10, 0)
# {7: ([R Ow DL DRO], GLOBAL, 0, 10, 0)}
```

The forms are:

- `# expression` for an integer word.
- `# (permission, locality, base, end, address)` for a capability.
- `# [sealing-permission, locality, base, end, address]` for a sealing range.
- `# (E-permission, locality, base, end, address)` for a sentry.
- `# {otype: sealable}` for a sealed capability or sealing range.

All bounds, addresses, and object types are finite expressions.

## Current address

The special expression `&CURRENT_ADDR` evaluates to the address of the instruction or literal word
that contains it:

```asm
mov r1 &CURRENT_ADDR
# (&CURRENT_ADDR + 1)
```

Labels and declarations occupy no address. Current addresses are resolved after sequence macros
expand, so every expanded instruction receives its actual address.

## Integer definitions

`%define` creates an immutable, file-wide integer symbol:

```asm
%define VALUE 3
%define BUFFER_SIZE buffer_end - buffer_start

mov r1 VALUE
mov r2 (VALUE + 1)
```

The form is `%define NAME expression`. Definitions may reference other definitions and labels before
their declarations. A definition emits no word, and labels are computed after sequence macros have
expanded. Definitions may appear anywhere an integer expression is accepted.

Registers, permissions, localities, word types, duplicates, recursive cycles, and names also used by
labels are rejected as integer definitions.

## Sequence macros

A sequence macro expands a typed body of instructions, labels, and literal words:

```asm
%macro increment(dst: reg, amount: expr)
  add $dst $dst $amount
%endmacro

%increment(r1, 4)
```

Whitespace is insignificant, but the line-oriented layout above is recommended. Declarations are
file-wide, so calls may precede declarations. Zero-argument declarations and calls use `()`.

Parameters use `$name` in the body and declare one of these types:

| Type | Accepted argument and use |
|---|---|
| `reg` | a register; usable in register or value positions |
| `value` | any instruction value; usable only in value positions |
| `expr` | an integer expression; usable in expression or value positions |
| `perm` | a Griotte capability permission |
| `sealperm` | a sealing permission |
| `locality` | a locality |
| `wtype` | a word type |

Arguments are comma-separated. Delimiters inside an argument keep their commas within that argument.
For example, `([R W DL DRO], GLOBAL)` is one argument.

Labels declared inside a macro are private and renamed uniquely for every call. External label
references remain file-scoped. Macros are deliberately flat: a macro body cannot contain another
macro call, `%define`, or `%macro` declaration.

## Register files

A register file initializes general and system registers with complete words:

```asm
pc := ([X Ow DL DRO], GLOBAL, 0, MAX_ADDR, 0)
csp := 42
mtdc := [SU, LOCAL, 0, 10, 0]
```

Register-file expressions support finite integers, `MAX_ADDR`, parentheses, and the same arithmetic,
bitwise, and shift operators as program expressions. They do not support labels, `%define`, sequence
macros, or `&CURRENT_ADDR`.
