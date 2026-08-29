(** Backend-independent, resolved assembler syntax. The shared frontend removes macros and labels
    before a value of [program] is produced. *)

type register = string

type expression =
  | Integer of Z.t
  | Max_address
  | Stack_address
  | Add of expression * expression
  | Subtract of expression * expression

type permission = O | E | RO | RX | RW | RWX | RWL | RWLX | URW | URWL | URWX | URWLX
type locality = Global | Local | Directed
type seal_permission = { seal : bool; unseal : bool }
type word_type = Integer_type | Capability_type | Seal_range_type | Sealed_type

type constant =
  | Constant_expression of expression
  | Permission of permission
  | Seal_permission of seal_permission
  | Locality of locality
  | Word_type of word_type
  | Permission_locality of permission * locality
  | Seal_permission_locality of seal_permission * locality

type operand = Register of register | Constant of constant

type sealable =
  | Capability of permission * locality * expression * expression * expression
  | Seal_range of seal_permission * locality * expression * expression * expression

type word = Integer_word of expression | Sealable of sealable | Sealed of expression * sealable
type instruction = { opcode : string; operands : operand list }
type statement_node = Instruction of instruction | Word of word
type statement = { node : statement_node; location : Diagnostic.source_location option }
type program = statement list

type regfile_entry = {
  register : register;
  word : word;
  location : Diagnostic.source_location option;
}

type regfile = regfile_entry list

val parse_program : ?filename:string -> string -> (program, Diagnostic.t list) result
val parse_regfile : ?filename:string -> string -> (regfile, Diagnostic.t list) result

val resolve_regfile : Runtime_config.t -> regfile -> regfile
(** Resolve the runtime address symbols in a register file. Program expressions have already been
    resolved by [parse_program]. *)

val parse_word : ?filename:string -> string -> (word, Diagnostic.t list) result
(** Parse one editable word. Runtime symbols remain symbolic so a backend can resolve them against
    the state being edited. *)

val evaluate_expression : Runtime_config.t -> expression -> Z.t
