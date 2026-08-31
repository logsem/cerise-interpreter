# Interpreter internals

This guide is a source-level tour for contributors who already know OCaml but do not yet know how
this interpreter fits together. It follows real Vanilla values through the implementation and then
opens up the two shared codec engines. For assembly syntax, see [assembler.md](assembler.md); for
extension checklists and validation commands, see [DEVELOPMENT.md](DEVELOPMENT.md); for the shorter
module map, see [architecture.md](architecture.md).

## The representations and their owners

The most useful distinction is between source syntax, semantic values, and renderer-facing data.
They are deliberately different types:

```text
source text
  │  shared lexer + merged shared/backend Menhir grammar
  ▼
located Assembly_construction.item values
  │  macro validation and expansion; symbol resolution
  ▼
backend-owned Asm_ir.statement values
  │  runtime-dependent concrete assembly + instruction encoding
  ▼
backend-owned Ast.word values
  │  machine initialization and persistent state transitions
  ▼
backend-owned Backend.state
  │  Backend.inspect
  ▼
backend-neutral Machine_view.t
```

The directories divide responsibility as follows:

| Area | Owns | Must not own |
|---|---|---|
| `src/` | CLI wiring, application history/navigation, terminal input and rendering | Backend semantic values |
| `lib/machine_session.ml` and `lib/backend_registry.ml` | Backend selection and the existential package of backend, configuration, and state | Knowledge of a concrete backend's word or state type |
| `lib/assembly/` | The broad token universe plus common expressions, declarations, labels, and sequence-macro grammar | A backend's accepted instruction set or semantic AST |
| `lib/core/assembly_construction.*` | Located source items, hygienic macro expansion, and symbol resolution | Instruction-specific substitution or concrete assembly |
| `lib/backends/<backend>/asm_ir.ml` and `grammar.mly` | Parser-facing instruction/word terms, typed macro hooks, and conversion to semantic values | Cross-backend semantics |
| `lib/backends/<backend>/ast.ml` | Executable registers, words, metadata, and instructions | Labels, parameters, or unresolved expressions |
| `lib/core/instruction_codec.*` and each backend's `codec.ml` | Shared codec mechanics and the backend's encoding declaration | Machine transition semantics |
| `lib/backends/<backend>/machine.ml` | Dynamic semantic state and transitions | Runtime configuration as persistent state, parsing, or terminal presentation |
| `Machine_backend.S` and `Machine_view` | The adapter contract and pure frontend snapshot | A common machine word type |

`Machine_session` resolves a CLI name to a first-class `Machine_backend.S` module, asks it to parse
and initialize, and existentially packages its private state. The session retains the immutable
`Runtime_config.t` and supplies that same value to every step, inspection, and edit. A backend
machine state therefore contains only changing semantic state. `Machine_view.t` is the other major
boundary: it is stable, pure data, so the UI never has to inspect a backend `Ast.word` or parse a
printer string to recover capability structure.

The handwritten backends share construction and codec *mechanisms*, not semantic types or encoding
tables. The generated part of `griotte-extracted` is a separate trust boundary and must not be
hand-edited; regenerate it with `make regenerate-griotte-extracted`.

## A Vanilla `mov` from text to a rendered step

Consider a session created from:

```asm
mov r1 7
halt
```

The first instruction crosses all of the main boundaries.

### 1. Lexing and parsing

`lib/assembly/lexer.mll` classifies `mov` as `MOVE "mov"`, `r1` as `REGISTER "r1"`, and `7` as an
arbitrary-precision `INTEGER (Z.of_int 7)`. Keyword classification is shared and intentionally
broad; the selected backend grammar decides whether a recognized token is actually accepted.

Dune merges `token_spec.mly`, `common_parser.mly`, and Vanilla's `grammar.mly` into the parser engine
included by `generated_parser.ml`. The common grammar owns the top-level `program` and wraps every
emitting construct in a located `Assembly_construction.item`. Vanilla's `statement` production owns
the instruction shape and constructs, schematically:

```ocaml
Statement
  (Op
     (Move_term
        (Named (Reg 1),
         Constant_term (Expression (Integer (Z.of_int 7))))))
```

The names here matter. `Asm_ir.Move_term` can still contain source expressions and macro
parameters; `Ast.Move` cannot. `Parser.parse_program` catches lexer, Menhir, and construction
exceptions as located `Diagnostic.t` values, then passes the parsed item list to
`Asm_ir.assemble_source_program`.

### 2. Macro processing and symbol resolution

Vanilla instantiates `Assembly_construction.Make (Asm_ir.Syntax)`. Even this macro-free program
passes through the same ordered pipeline:

1. Collect every macro declaration and validate its parameters and backend-specific parameter
   uses, including declarations that are never called.
2. Expand calls recursively. Bind and type-check arguments, substitute backend-specific terms, and
   give private labels fresh names per invocation.
3. Resolve definitions, labels, and `&CURRENT_ADDR`, remove non-emitting declarations, and return
   only backend statements.

For `mov r1 7`, expansion has nothing to replace and symbol resolution leaves the integer literal
unchanged. The result remains an `Asm_ir.Op (Move_term ...)`, but it no longer contains a symbol,
current-address marker, or macro parameter. Runtime constants such as `MAX_ADDR` and `STK_ADDR` are
the intentional exception: they remain expression nodes until concrete assembly receives the
session's runtime configuration.

### 3. Concrete assembly and encoding

Parsing stops at `Asm_ir.statement list`. `Vanilla.Backend.init`, not the parser, next calls
`Asm_ir.assemble_program config`:

- `assemble_register` turns `Named (Reg 1)` into `Ast.Reg 1`.
- `assemble_constant` evaluates the expression to `Z.of_int 7`.
- `assemble_instruction` constructs `Ast.Move (Reg 1, Constant (Z.of_int 7))`.
- `Codec.encode` turns that semantic instruction into a non-negative integer.
- `assemble_program` wraps the integer as `Ast.I encoded`, because program memory contains words.

In Vanilla's declaration table, `Jmp` owns opcode 0 and `Jnz` owns opcode 1. `Move` begins at opcode
2 and has span two: opcode 2 is its register-source variant and opcode 3 is its constant-source
variant. Thus this example has low byte 3. The high payload pairs register encoding 2 (`Reg 1` is
encoded as `1 + 1`) with the constant 7. Their paired payload is 184, so the complete current
encoding is `(184 << 8) | 3 = 47107`. The codec section below explains that pairing.

`Machine.init` initializes registers, creates the initial PC capability from the runtime address
limit, and places the encoded `Ast.I` words at consecutive addresses beginning at zero. This is the
point where the program becomes executable machine state.

### 4. Fetch, decode, execute, and inspect

`Machine.step config state` first validates the PC capability and fetch address. At address zero it
finds `Ast.I encoded`, calls `Codec.decode`, and recovers the same
`Ast.Move (Reg 1, Constant 7)`. `Machine.execute` resolves the constant operand to `Ast.I 7`, writes
that word to `r1`, and then advances the PC cursor from zero to one. Maps and records are rebuilt, so
the prior session remains unchanged.

`Vanilla.Backend.inspect` finally converts the private state to `Machine_view.t`. After the step,
`r1` is a view word with `kind = Integer` and `integer = Some 7`. When inspecting address zero,
`view_word` also attempts to decode the integer as an instruction and records
`decoded_instruction = Some "mov r1 7"`. Frontends consume that optional field and the structured
word metadata; they do not call the Vanilla codec themselves.

The trace is protected at several levels:

- `tests/active_backend_tests.ml` covers parsing, every Vanilla instruction constructor, large
  operands, execution, and macro resolution.
- `tests/instruction_codec_tests.ml` covers declaration-order variants, inverse round trips, large
  paired values, and structured failures in the shared engine.
- `tests/core_contract_tests.ml` covers persistent stepping, session configuration, and stable
  inspection data.

## What macro expansion and symbol resolution actually do

This example puts typed parameters, a private label, and `&CURRENT_ADDR` in the same macro:

```asm
%macro record(dst: reg, bias: expr)
local:
  mov $dst (local + $bias)
  mov r30 &CURRENT_ADDR
%endmacro

%record(r1, 10)
%record(r2, 20)
halt
```

It moves through construction in these phases.

### Parsed source items

The common grammar produces one `Macro_definition`, two `Macro_call` items, and one ordinary
`Statement`. The macro body contains a `Label "local"` and two statements. Vanilla's grammar turns
`$dst` into `Register_parameter "dst"`; the shared expression grammar turns `$bias` into
`Expression.Parameter "bias"`, `local` into `Expression.Symbol "local"`, and
`&CURRENT_ADDR` into `Expression.Current_address`.

### Declaration validation and call binding

`collect_and_validate_macro_definitions` records `record` before expanding any call. That is why a
call may appear before its declaration. Vanilla's `Syntax` hooks establish that:

- `reg` accepts the `Register_argument (Reg 1)` and later `Reg 2`;
- `expr` accepts the integer-expression arguments 10 and 20;
- `$dst` is legal in the destination register position; and
- `$bias` is legal in an expression position.

Arity, argument kinds, duplicate parameters, and invalid parameter uses fail here or while binding
a call, with the relevant source location.

### Hygienic expansion

Each invocation gets a monotonically numbered private-label base. With no colliding user symbol,
the two `local` labels become:

```text
__macro_0_record_local
__macro_1_record_local
```

If either generated spelling is already reserved by a label or definition, the allocator appends a
numeric suffix until it finds an unused name. Expression rewriting substitutes `$bias` and rewrites
references to `local`; Vanilla's term substitution independently replaces `$dst`. Macro
declarations disappear, while the expanded labels and statements retain located source items.
Nested calls are expanded recursively with the current bindings; an expansion stack rejects call
cycles.

Ignoring locations, the expanded stream is now equivalent to:

```asm
__macro_0_record_local:
mov r1 (__macro_0_record_local + 10)
mov r30 &CURRENT_ADDR
__macro_1_record_local:
mov r2 (__macro_1_record_local + 20)
mov r30 &CURRENT_ADDR
halt
```

### Two-pass symbol resolution

The first pass assigns addresses. A statement or literal word consumes one address; labels,
definitions, and macro declarations do not. The first private label is therefore 0, the second is
2, and `halt` is at 4. The pass also collects integer definitions and reports duplicate or
label-conflicting names.

The second pass rewrites every expression at the address of its containing emitted item. It follows
definition references recursively, reports cycles and unknown names, replaces labels and
`Current_address` with integers, and folds operations whose operands are now integers. The emitted
Vanilla statements are consequently equivalent to:

```asm
mov r1 10
mov r30 1
mov r2 22
mov r30 3
halt
```

Only after that does Vanilla concrete assembly evaluate any remaining runtime constants and encode
the instructions. This ordering is the reason private labels cannot collide across calls and why
`&CURRENT_ADDR` sees an instruction's address *after* macro expansion.

When changing this pipeline, keep the boundary between common and backend-specific work clear.
`Assembly_construction` owns item ordering, hygiene, and symbol arithmetic. A backend's
`Asm_ir.Syntax` owns traversal of its terms, legal typed-parameter positions, and substitution of
its parameter-bearing constructors.

## Instruction codec internals

`Instruction_codec` is a typed declaration language compiled into one bidirectional table. It has
four authoring layers:

1. A `'a scalar_codec` gives an atomic semantic value one `Z.t` representation in both directions.
2. A `'a operand_codec` combines typed operands and reports both an opcode `variant` and a payload.
3. An `'instruction encoding_pattern` names one operand shape and supplies `construct` and
   `project` functions.
4. `compile` assigns each pattern a contiguous opcode range and returns the immutable codec used by
   `encode` and `decode`.

### Scalars, spans, and variants

An operand codec has an integer `span`. The span is how many adjacent opcodes that operand shape
needs:

- `unit`, `scalar codec`, `register codec`, and `signed_zarith` have span one.
- `register_or_constant registers constants` has span two. Variant 0 carries a register; variant 1
  carries a constant.
- `pair left right` has `left.span * right.span` variants. Its variant index is
  `left_variant * right.span + right_variant`.
- `triple a b c` is a typed façade over `pair a (pair b c)`.

Span multiplication saturates at 257, one beyond the 256-opcode field, instead of overflowing an
OCaml `int`. Compilation can then report `Opcode_overflow` safely even for deeply nested operand
codecs.

The scalar helpers have different contracts. `zarith` is the identity in both directions and may
produce a signed child payload. `nonnegative_zarith` rejects negative values. `signed_zarith`
zig-zag encodes a signed `Z.t` into a non-negative scalar payload. `enum` assigns values consecutive
encodings in list order and uses structural equality. Backend codecs may instead supply exact
architectural mappings, as Vanilla does for registers: `PC` is 0 and `Reg n` is `n + 1` for
`0 <= n <= 31`.

### Pairing arbitrary-precision payloads

A pair must preserve two child payloads without assuming either fits a machine word. The codec
therefore uses a reversible arbitrary-precision pairing:

1. Bit 0 records whether the left payload is negative and bit 1 records whether the right payload
   is negative.
2. The magnitudes' bits are interleaved above those sign bits: even positions come from the left
   magnitude and odd positions from the right.
3. Decoding removes the sign bits, splits the alternating magnitude bits, and restores each sign.

The implementation processes the `Z.t` byte strings with small lookup tables; it never converts a
payload to a bounded host integer. This is why a Vanilla `Move` can round-trip a constant with more
than 100,000 bits. Pairing also turns signed child payloads into the non-negative top-level payload
required by instruction encoding.

### Compilation and the low eight bits

Compilation walks patterns in declaration order, beginning with opcode zero. A span-one pattern
claims one opcode; a span-two pattern claims two, and so on, up to the 256-entry space. Pattern names
must be unique. Reordering, inserting, or changing the span of an early pattern therefore changes
the encoded integers of later instructions and must be treated as an encoding change.

For a successful encoding:

```text
encoded = (first_opcode + variant) OR (payload << 8)
```

The low eight bits are always the opcode, and every higher bit belongs to a non-negative `Z.t`
payload. Decoding rejects a negative input, extracts the low byte with `Z.extract encoded 0 8`, and
shifts right by eight for the payload. It finds the one compiled range containing the opcode,
derives the variant from the range offset, and lets that operand codec decode the payload.

### Why `construct` and `project` come as a pair

For each pattern, `construct : operand -> instruction` is the decoding direction and
`project : instruction -> operand option` is the encoding direction. They are an authoring
contract, not something OCaml's type system or `compile` can prove. For values owned by a pattern,
they must satisfy both:

```text
project (construct operand) = Some operand
construct (the value returned by project instruction) = instruction
```

`encode` asks every compiled projector about the instruction. No match yields
`Unrecognized_instruction`; multiple matches yield `Ambiguous_instruction`; exactly one match is
encoded. This permits multiple deliberately disjoint patterns for different operand forms of one
semantic constructor, but overlapping projectors are always an error at use time.

Other failures remain structured rather than raising: invalid scalar or operand values become
`Invalid_operand`; a negative top-level payload is rejected; an unknown low byte becomes
`Unknown_opcode`; and a selected operand codec's decoding failure becomes `Malformed_encoding`
with its opcode and pattern name. Duplicate names and opcode exhaustion are returned as an error
list by `compile`. Constructor/projector round trips, every variant, boundary-size operands, and
malformed external integers belong in tests whenever a table changes.

## Tagged metadata internals: Vanilla permissions

Capability metadata uses a smaller repository-private engine, `Tagged_metadata_codec`. The six
handwritten backends declare their layouts with it; extracted Griotte keeps its independent
extracted representation. The wire shape is:

```text
encoded metadata = three-bit tag OR (payload << 3)
```

The tag distinguishes the *kind* of metadata. The payload carries a finite scalar or two finite
scalars packed into fixed-width fields.

### The Vanilla permission declaration

Vanilla's `codec.ml` declares one finite scalar mapping, which is the source of truth for both
encoding and decoding:

| Permission | Scalar payload |
|---|---:|
| `O` | 0 |
| `E` | 1 |
| `RO` | 4 |
| `RX` | 5 |
| `RW` | 6 |
| `RWX` | 7 |

`permission_pattern` gives that scalar payload tag 0 and the backend's two public error messages.
`metadata_layout` compiles it together with the seal-permission pattern at tag 1 and word-type
pattern at tag 3. Thus `RO` encodes as `(4 << 3) | 0 = 32`. `decode_permission 32` checks the tag,
shifts off the low three bits, finds scalar 4 in the same declaration, and returns `RO`.

The pattern is also an identity-bearing typed value. Creating an encoding pattern allocates a
private identifier; `compile` stores the identifiers of the exact heterogeneous pattern values it
validated. `encode` and `decode` reject a pattern that was not part of that layout, even if a newly
constructed pattern has the same name and tag. Backend code must therefore define each pattern once,
compile those values, and reuse those same values in its named wrappers.

For packed metadata in backends that have it, `packed_pair ~low_width ~high_width low high` places
the first scalar in the low field and the second above it. Compilation proves that every declared
scalar fits its field. Decoding extracts the low field, shifts out the high field, and rejects bits
beyond the two declared widths. Vanilla is global-only, so its permission pattern itself uses the
simpler scalar payload rather than a packed permission/locality pair.

### Where permission integers are consumed

For source such as:

```asm
restrict r1 RO
```

Vanilla's grammar first records `RO` as `Asm_ir.Permission RO`, not as an unexplained integer.
Concrete assembly calls `Codec.encode_permission`, producing constant 32 in the semantic
`Ast.Restrict`. At execution, `Machine.execute` resolves that operand to `Ast.I 32`, calls
`Codec.decode_permission`, and only then applies `permission_flows` to the capability in `r1`.
`GetP` performs the other direction: it reads a semantic capability permission and writes its
tagged encoding as an integer word.

This route keeps parsing, numeric layout, and permission-flow semantics separate. Adding a semantic
permission therefore requires all three decisions: what syntax names it, what scalar encoding it
has, and how it participates in machine operations.

### Validation and malformed values

Layout compilation reports all declaration problems it finds:

- blank or duplicate pattern names;
- tags outside 0 through 7 or duplicate tags;
- blank scalar names, duplicate semantic values, duplicate numeric encodings, or negative scalar
  encodings; and
- non-positive packed-field widths or declared values that do not fit their field.

A layout remembers only validated patterns. Encoding an unmapped semantic value or using a pattern
from another layout returns an error. Decoding is total over arbitrary `Z.t` inputs: a negative
integer or wrong low tag returns the pattern's `wrong_tag_error`; an unknown scalar, an invalid
packed field, or extra packed high bits returns its `malformed_payload_error`. The backend's public
permission decoders preserve these established strings.

The Vanilla encoder wrappers turn an unexpected encoding error into `failwith`, because the closed
semantic variants and statically compiled layout are expected to make that path unreachable. A new
AST constructor without a matching scalar entry violates that assumption, so exhaustive golden and
round-trip tests in `tests/tagged_metadata_codec_tests.ml` are part of the layout contract. That test
also exercises wrong tags, malformed payloads, declaration failures, and enormous external inputs.

## Finding the right extension point

Use these paths to start a change; [DEVELOPMENT.md](DEVELOPMENT.md) has the complete checklists.

| Change | Start here | Follow the value through |
|---|---|---|
| Add or change an instruction | The backend's `ast.ml`, `asm_ir.ml`, and `grammar.mly` | Concrete assembly, codec pattern, printer, machine transition, view implications, and tests |
| Add a metadata value | The backend AST and parser-facing term | Finite scalar mapping, printer, consumers such as `Restrict`/`GetP`, malformed cases, and numeric goldens |
| Change expression, label, definition, or macro behavior | `lib/assembly/common_parser.mly` and `lib/core/assembly_construction.*` | Every backend `Asm_ir.Syntax` hook and shared construction tests |
| Add a new lexical keyword class | `lib/assembly/token_spec.mly` and `lexer.mll` | Each merged backend grammar and its Dune unused-token list |
| Change instruction encoding only | The backend's `codec.ml` | Declaration-order opcode impact plus `tests/instruction_codec_tests.ml` and backend round trips |
| Change execution or inspection | The backend's `machine.ml` and `backend.ml` | `Machine_backend.S`, persistent session behavior, structured `Machine_view`, and UI fixtures |
| Add a backend | A nearby handwritten backend directory | Public group/signature, registry aliases, Dune parser merge, complete backend contract, and documentation |

Do not edit a generated parser engine to change syntax: edit the shared or backend `.mly` source and
let Dune regenerate it. Do not copy an encoding integer from another backend merely because the
printed instruction looks the same. The backend-owned `Ast`, codec table, and machine semantics are
the authoritative unit; shared modules provide typed mechanisms around that unit.
