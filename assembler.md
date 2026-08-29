# Cerise assembler reference

This document describes the assembly and register-file syntax accepted by the Cerise interpreter.
Assembly examples are available under `tests/test_files`.

## Source text

Whitespace separates tokens but is otherwise insignificant to ordinary assembly. A semicolon starts
a comment that continues to the end of the line.

```asm
mov r1 42       ; decimal integer
mov r2 0x2a     ; hexadecimal integer
```

Integer literals must fit in an OCaml machine integer when read. Negative integers use unary `-`.
Identifiers start with a letter or underscore and continue with letters, digits, or underscores.
Labels are case-sensitive. Instruction names are lowercase; `loadU`, `storeU`, and `promoteU` also
accept a lowercase `u`. Registers are case-insensitive. Permissions and word types are case-sensitive.
Localities accept uppercase or title-case spellings, such as `GLOBAL` and `Global`.

## Registers and values

Registers are `r0` through `r31`, plus these aliases:

| Name | Register |
|---|---:|
| `pc` | program counter |
| `ddc` | `r0` |
| `stk` | `r31` |

An instruction `value` operand accepts a register, an integer expression, or one of the encoded
architectural constants below.

| Category | Accepted syntax |
|---|---|
| Capability permission | `O`, `E`, `RO`, `RX`, `RW`, `RWX`, `RWL`, `RWLX`, `URW`, `URWX`, `URWL`, `URWLX` |
| Sealing permission | `SO`, `S`, `U`, `SU` |
| Locality | `GLOBAL`, `LOCAL`, `DIRECTED` (or title case) |
| Word type | `Int`, `Cap`, `SealRange`, `Sealed` |
| Permission/locality pair | `(RW, GLOBAL)` |
| Sealing-permission/locality pair | `(S, LOCAL)` |

Some values or instructions are rejected when they are unavailable in the selected backend.

## Instructions

The metavariables in this table are `r` for a register and `v` for a value.

| Form | Operands |
|---|---|
| `jmp r` | jump through a register |
| `jnz r r` | conditional jump |
| `mov r v` | move a register or constant |
| `load r r` | load through a capability |
| `store r v` | store through a capability |
| `add r v v` | addition |
| `sub r v v` | subtraction |
| `mul r v v` | multiplication |
| `rem r v v` | remainder |
| `div r v v` | division |
| `lt r v v` | less-than comparison |
| `lea r v` | modify a capability address |
| `restrict r v` | restrict permission/locality |
| `subseg r v v` | restrict capability bounds |
| `getl r r` | read locality |
| `getb r r` | read lower bound |
| `gete r r` | read upper bound |
| `geta r r` | read current address |
| `getp r r` | read permission |
| `getotype r r` | read object type |
| `getwtype r r` | read word type |
| `seal r r r` | seal a word |
| `unseal r r r` | unseal a word |
| `invoke r r` | invoke sealed capabilities |
| `loadU r r v` | uninitialized-capability load |
| `storeU r v v` | uninitialized-capability store |
| `promoteU r` | promote an uninitialized capability |
| `fail` | enter the failed state |
| `halt` | enter the halted state |

Operands are separated by whitespace rather than commas.

`rem` and `div` remain supported by backends where applicable, but are unavailable in both
`griotte` and `griotte-extracted`.

## Expressions and labels

Expressions contain integer literals, labels, parentheses, binary `+` and `-`, and unary `-`.
Use parentheses around compound expressions when they occur as an instruction operand:

```asm
start:
mov r1 (end - start)
halt
end:
```

A label marks the address of the next emitted instruction or word. Labels may be referenced before
their definitions. Labels themselves emit no word.

## Literal words

Prefix literal program data with `#`:

```asm
# 42
# (RWX, GLOBAL, 0, 1024, 0)
# [SU, LOCAL, 0, 10, 0]
# {7: (RO, GLOBAL, 0, 10, 0)}
```

The forms are:

- `# expression` for an integer word.
- `# (permission, locality, base, end, address)` for a capability.
- `# [sealing-permission, locality, base, end, address]` for a sealing range.
- `# {otype: sealable}` for a sealed capability or sealing range.

Bounds, addresses, and object types are finite integer expressions.

## Current address

The special expression `&CURRENT_ADDR` evaluates to the address of the instruction or literal word
that contains it:

```asm
mov r1 &CURRENT_ADDR
# (&CURRENT_ADDR + 1)
```

Labels and assembler declarations do not occupy addresses. Current-address expressions are resolved
after sequence macros expand, so each expanded macro instruction receives its actual address. The
expression may be combined with integers, labels, and definitions using `+` and `-` anywhere an
ordinary integer expression is accepted.

## Integer definitions

`%define` creates an immutable, file-wide integer symbol:

```asm
%define VALUE 3
%define BUFFER_SIZE buffer_end - buffer_start

mov r1 VALUE
mov r2 (VALUE + 1)
```

The directive has the form `%define NAME expression`. Its expression may use
finite decimal or hexadecimal integers, parentheses, `+`, `-`, other integer definitions, and labels.
Definitions and labels may be referenced before declaration. A definition emits no word, and its
label-based value is computed using addresses after sequence macros have expanded.

Definitions may appear anywhere an integer expression is accepted, including instruction values,
literal words, bounds, addresses, object types, and `expr` macro arguments. Registers,
permissions, localities, word types, duplicate definitions, recursive definition cycles, and names
reserved by the assembler are rejected. Integer definitions and labels share a namespace, so a file
may not define both with the same name.

## Sequence macros

A sequence macro expands a typed body of instructions, labels, and literal words:

```asm
%macro increment(dst: reg, amount: expr)
  add $dst $dst $amount
%endmacro

%increment(r1, 4)
```

Newlines are ordinary whitespace, so the `%macro` header, body, `%endmacro`, and calls do not
technically need separate lines; the layout above is recommended for readability. Declarations are
file-wide, so a call may precede its declaration. Zero-argument declarations and calls use `()`.
Macro declarations emit no words.

Parameters use `$name` in the body and must declare one of these types:

| Type | Accepted argument and use |
|---|---|
| `reg` | a register; usable in register or value positions |
| `value` | any instruction value; usable only in value positions |
| `expr` | an integer expression; usable in expression or value positions |
| `perm` | a capability permission |
| `sealperm` | a sealing permission |
| `locality` | a locality |
| `wtype` | a word type |

Arguments are comma-separated. Parentheses, brackets, or braces inside an argument keep their commas
within that argument, allowing values such as `(RW, GLOBAL)`.

Labels declared in a macro body are private and are renamed uniquely for each call. References in the
same body resolve to that private label; references to labels not declared by the body resolve at file
scope. A private label name may not collide with an integer definition.

Macro bodies may contain calls to declared sequence macros and `%define` directives. Calls nested in
macro bodies expand recursively (recursive cycles are rejected). Definitions emitted by a macro are
file-wide and share the label/definition namespace; hygienic private-label allocation avoids those
names. `%macro` declarations cannot be nested. Macros cannot replace instruction names or complete
statements through a parameter. Duplicate names or parameters, unknown calls or parameters, wrong
arity, wrong argument types, and duplicate private labels are errors.

## Register files

A register file is supplied with `--regfile` and uses assignments of complete words:

```asm
pc := (RX, GLOBAL, 0, MAX_ADDR, 0)
r1 := 42
r2 := [SU, LOCAL, 0, 10, 0]
r3 := {7: (RO, GLOBAL, 0, 10, 0)}
stk := (RWLX, LOCAL, 0, STK_ADDR, STK_ADDR)
```

Register-file expressions support integers, parentheses, `+`, `-`, and the predefined
`MAX_ADDR` and `STK_ADDR` values. They do not support labels, `%define`, or sequence macros.

## Backends

The command-line `--backend` option selects an independent backend (default: `vanilla`). The
available backends are:

| Backend | Main features |
|---|---|
| `vanilla` | global-only capabilities with sealing; no locality or uninitialized permissions |
| `cerise` | alias for `vanilla` |
| `locality-cerise` | global and local capabilities with sealing; no uninitialized permissions |
| `ucerise` | historical uninitialized capabilities with global and local locality; no sealing |
| `mcerise` | historical uninitialized capabilities with global, local, and directed locality; no sealing |
| `cerisier` | historical enclave backend with sealing, uninitialized, and directed capabilities |
| `griotte` | Griotte capability machine with its own parser and instruction set |
| `griotte-extracted` | Rocq-extracted Griotte implementation behind a differential adapter |

Each backend parses and validates its own syntax. The common construction-block frontend handles
expressions, labels, definitions, and typed sequence macros; instruction encodings use backend
codec tables. Consequently, an instruction, permission, locality, or word can be rejected by the
selected backend even when it is valid in another one.
