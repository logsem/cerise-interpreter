(* This facade owns Griotte assembly syntax and its translation to executable words.
   Parsed terms flow through macro validation and hygienic substitution before the
   concrete-assembly phase evaluates expressions and encodes instructions. *)

open Ast
open Assembly_construction

let ( let* ) (type value next error) (result : (value, error) result)
    (continuation : value -> (next, error) result) : (next, error) result =
  Result.bind result continuation

let diagnostic (message : string) : ('a, Diagnostic.t list) result =
  Error [ Diagnostic.error message ]

module Syntax_types = struct
  type expression = Assembly_construction.Expression.t
  type register_term = Named of register | Register_parameter of string
  type permission_term = Permission_literal of permission | Permission_parameter of string

  type seal_permission_term =
    | Seal_permission_literal of seal_permission
    | Seal_permission_parameter of string

  type locality_term = Locality_literal of locality | Locality_parameter of string
  type word_type_term = Word_type_literal of word_type | Word_type_parameter of string

  type constant_term =
    | Expression of expression
    | Permission of permission
    | Seal_permission of seal_permission
    | Permission_locality of permission_term * locality_term
    | Seal_permission_locality of seal_permission_term * locality_term
    | Word_type of word_type
    | Locality of locality
    | Value_parameter of string

  type operand_term = Register_term of register_term | Constant_term of constant_term

  type sealable_term =
    | Cap_term of permission_term * locality_term * expression * expression * expression
    | Seal_range_term of seal_permission_term * locality_term * expression * expression * expression

  type word_term =
    | I_term of expression
    | Sealable_term of sealable_term
    | Sentry_term of permission_term * locality_term * expression * expression * expression
    | Sealed_term of expression * sealable_term

  type word = word_term

  type instruction_term =
    | Jalr_term of register_term * register_term
    | Jmp_term of operand_term
    | Jnz_term of register_term * operand_term
    | ReadSR_term of register_term * system_register
    | WriteSR_term of system_register * register_term
    | Move_term of register_term * operand_term
    | Load_term of register_term * register_term
    | Store_term of register_term * operand_term
    | Add_term of register_term * operand_term * operand_term
    | Sub_term of register_term * operand_term * operand_term
    | Mul_term of register_term * operand_term * operand_term
    | LAnd_term of register_term * operand_term * operand_term
    | LOr_term of register_term * operand_term * operand_term
    | LShiftL_term of register_term * operand_term * operand_term
    | LShiftR_term of register_term * operand_term * operand_term
    | Lt_term of register_term * operand_term * operand_term
    | Lea_term of register_term * operand_term
    | Restrict_term of register_term * operand_term
    | SubSeg_term of register_term * operand_term * operand_term
    | GetL_term of register_term * register_term
    | GetB_term of register_term * register_term
    | GetE_term of register_term * register_term
    | GetA_term of register_term * register_term
    | GetP_term of register_term * register_term
    | GetOType_term of register_term * register_term
    | GetWType_term of register_term * register_term
    | Seal_term of register_term * register_term * register_term
    | UnSeal_term of register_term * register_term * register_term
    | Fail_term
    | Halt_term

  type statement = Op of instruction_term | Word of word_term
  type program = statement list

  type regfile_entry =
    | Register_entry of register * word_term
    | System_register_entry of system_register * word_term

  type regfile = regfile_entry list

  let parse_register_name (name : string) : register option =
    match String.lowercase_ascii name with
    | "pc" -> Some PC
    | "cnull" -> Some cnull
    | "cra" -> Some cra
    | "csp" -> Some csp
    | "cgp" -> Some cgp
    | "ctp" -> Some ctp
    | "ct0" -> Some ct0
    | "ct1" -> Some ct1
    | "ct2" -> Some ct2
    | "ct3" -> Some ct3
    | "ct4" -> Some ct4
    | "ct5" -> Some ct5
    | "ct6" -> Some ct6
    | "cs0" -> Some cs0
    | "cs1" -> Some cs1
    | "cs2" -> Some cs2
    | "cs3" -> Some cs3
    | "cs4" -> Some cs4
    | "cs5" -> Some cs5
    | "cs6" -> Some cs6
    | "cs7" -> Some cs7
    | "cs8" -> Some cs8
    | "cs9" -> Some cs9
    | "cs10" -> Some cs10
    | "cs11" -> Some cs11
    | "ca0" -> Some ca0
    | "ca1" -> Some ca1
    | "ca2" -> Some ca2
    | "ca3" -> Some ca3
    | "ca4" -> Some ca4
    | "ca5" -> Some ca5
    | "ca6" -> Some ca6
    | "ca7" -> Some ca7
    | name when String.length name > 1 && name.[0] = 'r' ->
        Option.bind
          (int_of_string_opt (String.sub name 1 (String.length name - 1)))
          (fun n -> if n >= 0 && n <= 31 then Some (Reg n) else None)
    | _ -> None

  let parse_rx (term : string) : rx_permission option =
    match term with
    | "orx" -> Some Orx
    | "r" -> Some R
    | "x" -> Some X
    | "xsr" -> Some XSR
    | _ -> None

  let parse_write (term : string) : write_permission option =
    match term with "ow" -> Some Ow | "w" -> Some W | "wl" -> Some WL | _ -> None

  let parse_dl (term : string) : deep_local_permission option =
    match term with "dl" -> Some DL | "lg" -> Some LG | _ -> None

  let parse_dro (term : string) : deep_read_only_permission option =
    match term with "dro" -> Some DRO | "lm" -> Some LM | _ -> None

  let parse_locality_name (name : string) : locality option =
    match String.lowercase_ascii name with
    | "local" -> Some Local
    | "global" -> Some Global
    | _ -> None

  let parse_seal_permission_name (name : string) : (bool * bool) option =
    match String.uppercase_ascii name with
    | "SO" -> Some (false, false)
    | "S" -> Some (true, false)
    | "U" -> Some (false, true)
    | "SU" -> Some (true, true)
    | _ -> None

  let parse_word_type_name (name : string) : word_type option =
    match String.lowercase_ascii name with
    | "int" -> Some W_I
    | "cap" -> Some W_Cap
    | "sealrange" -> Some W_SealRange
    | "sealed" -> Some W_Sealed
    | "sentry" -> Some W_Sentry
    | _ -> None

  type parameter_kind =
    | Register_kind
    | Expression_kind
    | Value_kind
    | Permission_kind
    | Seal_permission_kind
    | Locality_kind
    | Word_type_kind

  type macro_argument = Register_argument of register | Constant_argument of constant_term
end

include Syntax_types

(* Macro processing: describe the backend-specific terms to the shared assembler. *)
module Macro_processing = struct
  type nonrec statement = statement
  type raw_word = word_term
  type nonrec macro_argument = macro_argument
  type nonrec parameter_kind = parameter_kind

  let statement_of_raw_word (w : raw_word) : statement = Word w

  let parameter_kind (term : string) : parameter_kind option =
    match term with
    | "reg" -> Some Register_kind
    | "expr" -> Some Expression_kind
    | "value" -> Some Value_kind
    | "perm" -> Some Permission_kind
    | "sealperm" -> Some Seal_permission_kind
    | "locality" -> Some Locality_kind
    | "wtype" -> Some Word_type_kind
    | _ -> None

  let parameter_kind_name (term : parameter_kind) : string =
    match term with
    | Register_kind -> "reg"
    | Expression_kind -> "expr"
    | Value_kind -> "value"
    | Permission_kind -> "perm"
    | Seal_permission_kind -> "sealperm"
    | Locality_kind -> "locality"
    | Word_type_kind -> "wtype"

  let argument_kind (term : macro_argument) : parameter_kind =
    match term with
    | Register_argument _ -> Register_kind
    | Constant_argument (Expression _) -> Expression_kind
    | Constant_argument (Permission _) -> Permission_kind
    | Constant_argument (Seal_permission _) -> Seal_permission_kind
    | Constant_argument (Locality _) -> Locality_kind
    | Constant_argument (Word_type _) -> Word_type_kind
    | Constant_argument _ -> Value_kind

  let accepts_argument (kind : parameter_kind) (arg : macro_argument) : bool =
    kind = Value_kind || kind = argument_kind arg

  let expression_of_argument (term : macro_argument) : expression option =
    match term with Constant_argument (Expression e) -> Some e | _ -> None

  let map_constant (f : expression -> expression) (term : constant_term) : constant_term =
    match term with Expression e -> Expression (f e) | c -> c

  let map_operand (f : expression -> expression) (term : operand_term) : operand_term =
    match term with
    | Register_term r -> Register_term r
    | Constant_term c -> Constant_term (map_constant f c)

  let map_sealable (f : expression -> expression) (term : sealable_term) : sealable_term =
    match term with
    | Cap_term (p, l, b, e, a) -> Cap_term (p, l, f b, f e, f a)
    | Seal_range_term (p, l, b, e, a) -> Seal_range_term (p, l, f b, f e, f a)

  let map_word (f : expression -> expression) (term : raw_word) : raw_word =
    match term with
    | I_term e -> I_term (f e)
    | Sealable_term s -> Sealable_term (map_sealable f s)
    | Sentry_term (p, l, b, e, a) -> Sentry_term (p, l, f b, f e, f a)
    | Sealed_term (o, s) -> Sealed_term (f o, map_sealable f s)

  let map_instruction (f : expression -> expression) (term : instruction_term) : instruction_term =
    match term with
    | Jmp_term o -> Jmp_term (map_operand f o)
    | Jnz_term (r, o) -> Jnz_term (r, map_operand f o)
    | Move_term (r, o) -> Move_term (r, map_operand f o)
    | Store_term (r, o) -> Store_term (r, map_operand f o)
    | Add_term (r, a, b) -> Add_term (r, map_operand f a, map_operand f b)
    | Sub_term (r, a, b) -> Sub_term (r, map_operand f a, map_operand f b)
    | Mul_term (r, a, b) -> Mul_term (r, map_operand f a, map_operand f b)
    | LAnd_term (r, a, b) -> LAnd_term (r, map_operand f a, map_operand f b)
    | LOr_term (r, a, b) -> LOr_term (r, map_operand f a, map_operand f b)
    | LShiftL_term (r, a, b) -> LShiftL_term (r, map_operand f a, map_operand f b)
    | LShiftR_term (r, a, b) -> LShiftR_term (r, map_operand f a, map_operand f b)
    | Lt_term (r, a, b) -> Lt_term (r, map_operand f a, map_operand f b)
    | Lea_term (r, o) -> Lea_term (r, map_operand f o)
    | Restrict_term (r, o) -> Restrict_term (r, map_operand f o)
    | SubSeg_term (r, a, b) -> SubSeg_term (r, map_operand f a, map_operand f b)
    | op -> op

  let map_statement_expressions (f : expression -> expression) (term : statement) : statement =
    match term with Op op -> Op (map_instruction f op) | Word w -> Word (map_word f w)

  let map_raw_word_expressions (mapper : expression -> expression) (word : word_term) : word_term =
    map_word mapper word

  let map_regfile_expressions (f : expression -> expression)
      ((regs, sregs) : ('a * raw_word) list * ('b * raw_word) list) :
      ('a * raw_word) list * ('b * raw_word) list =
    ( List.map (fun (r, w) -> (r, map_word f w)) regs,
      List.map (fun (r, w) -> (r, map_word f w)) sregs )

  let map_argument_expressions (f : expression -> expression) (term : macro_argument) :
      macro_argument =
    match term with Constant_argument c -> Constant_argument (map_constant f c) | a -> a

  let rec validate_expression (parameters : (string * parameter_kind) list)
      (accumulator : Diagnostic.t list) (term : expression) : Diagnostic.t list =
    match term with
    | Expression.Parameter name -> (
        match List.assoc_opt name parameters with
        | Some (Expression_kind | Value_kind) -> accumulator
        | Some _ ->
            Diagnostic.error (Printf.sprintf "$%s is not an expression parameter." name)
            :: accumulator
        | None ->
            Diagnostic.error (Printf.sprintf "Unknown macro parameter $%s." name) :: accumulator)
    | Add (a, b)
    | Subtract (a, b)
    | Multiply (a, b)
    | Logand (a, b)
    | Logor (a, b)
    | Shift_left (a, b)
    | Shift_right (a, b) ->
        validate_expression parameters (validate_expression parameters accumulator a) b
    | _ -> accumulator

  let valid_param (parameters : ('a * 'b) list) (expected : 'b list) (name : 'a)
      (message : ('a -> string, unit, string) format) (accumulator : Diagnostic.t list) :
      Diagnostic.t list =
    match List.assoc_opt name parameters with
    | Some kind when List.mem kind expected -> accumulator
    | _ -> Diagnostic.error (Printf.sprintf message name) :: accumulator

  let validate_register_term (parameters : (string * parameter_kind) list)
      (accumulator : Diagnostic.t list) (term : register_term) : Diagnostic.t list =
    match term with
    | Named _ -> accumulator
    | Register_parameter n ->
        valid_param parameters [ Register_kind; Value_kind ] n "Invalid register parameter $%s."
          accumulator

  let validate_permission_term (parameters : (string * parameter_kind) list)
      (accumulator : Diagnostic.t list) (term : permission_term) : Diagnostic.t list =
    match term with
    | Permission_literal _ -> accumulator
    | Permission_parameter n ->
        valid_param parameters [ Permission_kind ] n "Invalid permission parameter $%s." accumulator

  let validate_seal_permission_term (parameters : (string * parameter_kind) list)
      (accumulator : Diagnostic.t list) (term : seal_permission_term) : Diagnostic.t list =
    match term with
    | Seal_permission_literal _ -> accumulator
    | Seal_permission_parameter n ->
        valid_param parameters [ Seal_permission_kind ] n "Invalid seal permission parameter $%s."
          accumulator

  let validate_locality_term (parameters : (string * parameter_kind) list)
      (accumulator : Diagnostic.t list) (term : locality_term) : Diagnostic.t list =
    match term with
    | Locality_literal _ -> accumulator
    | Locality_parameter n ->
        valid_param parameters [ Locality_kind ] n "Invalid locality parameter $%s." accumulator

  let validate_constant_term (parameters : (string * parameter_kind) list)
      (accumulator : Diagnostic.t list) (term : constant_term) : Diagnostic.t list =
    match term with
    | Expression e -> validate_expression parameters accumulator e
    | Value_parameter n ->
        valid_param parameters
          [
            Register_kind;
            Value_kind;
            Expression_kind;
            Permission_kind;
            Seal_permission_kind;
            Locality_kind;
            Word_type_kind;
          ]
          n "Invalid value parameter $%s." accumulator
    | Permission_locality (Permission_literal _, l) ->
        validate_locality_term parameters accumulator l
    | Permission_locality (Permission_parameter n, l) ->
        validate_locality_term parameters
          (valid_param parameters
             [ Permission_kind; Seal_permission_kind ]
             n "Invalid permission parameter $%s." accumulator)
          l
    | Seal_permission_locality (p, l) ->
        validate_locality_term parameters (validate_seal_permission_term parameters accumulator p) l
    | _ -> accumulator

  let validate_operand_term (parameters : (string * parameter_kind) list)
      (accumulator : Diagnostic.t list) (term : operand_term) : Diagnostic.t list =
    match term with
    | Register_term r -> validate_register_term parameters accumulator r
    | Constant_term c -> validate_constant_term parameters accumulator c

  let validate_sealable_term (parameters : (string * parameter_kind) list)
      (accumulator : Diagnostic.t list) (term : sealable_term) : Diagnostic.t list =
    match term with
    | Cap_term (p, l, b, e, a) ->
        validate_expression parameters
          (validate_expression parameters
             (validate_expression parameters
                (validate_locality_term parameters
                   (validate_permission_term parameters accumulator p)
                   l)
                b)
             e)
          a
    | Seal_range_term (p, l, b, e, a) ->
        validate_expression parameters
          (validate_expression parameters
             (validate_expression parameters
                (validate_locality_term parameters
                   (validate_seal_permission_term parameters accumulator p)
                   l)
                b)
             e)
          a

  let validate_word_term (parameters : (string * parameter_kind) list)
      (accumulator : Diagnostic.t list) (term : raw_word) : Diagnostic.t list =
    match term with
    | I_term e -> validate_expression parameters accumulator e
    | Sealable_term s -> validate_sealable_term parameters accumulator s
    | Sentry_term (p, l, b, e, a) ->
        validate_expression parameters
          (validate_expression parameters
             (validate_expression parameters
                (validate_locality_term parameters
                   (validate_permission_term parameters accumulator p)
                   l)
                b)
             e)
          a
    | Sealed_term (o, s) ->
        validate_sealable_term parameters (validate_expression parameters accumulator o) s

  let validate_statement ~(parameters : (string * parameter_kind) list) (statement : statement) :
      Diagnostic.t list =
    let accumulator = ref [] in
    let validate_register_usage (register : register_term) : unit =
      accumulator := validate_register_term parameters !accumulator register
    and validate_operand_usage (operand : operand_term) : unit =
      accumulator := validate_operand_term parameters !accumulator operand
    in
    (match statement with
    | Word w -> accumulator := validate_word_term parameters !accumulator w
    | Op op -> (
        match op with
        | Jmp_term a -> validate_operand_usage a
        | Jalr_term (a, b)
        | Load_term (a, b)
        | GetL_term (a, b)
        | GetB_term (a, b)
        | GetE_term (a, b)
        | GetA_term (a, b)
        | GetP_term (a, b)
        | GetOType_term (a, b)
        | GetWType_term (a, b) ->
            validate_register_usage a;
            validate_register_usage b
        | Jnz_term (a, b)
        | Move_term (a, b)
        | Store_term (a, b)
        | Lea_term (a, b)
        | Restrict_term (a, b) ->
            validate_register_usage a;
            validate_operand_usage b
        | ReadSR_term (a, _) | WriteSR_term (_, a) -> validate_register_usage a
        | Add_term (a, b, c)
        | Sub_term (a, b, c)
        | Mul_term (a, b, c)
        | LAnd_term (a, b, c)
        | LOr_term (a, b, c)
        | LShiftL_term (a, b, c)
        | LShiftR_term (a, b, c)
        | Lt_term (a, b, c)
        | SubSeg_term (a, b, c) ->
            validate_register_usage a;
            validate_operand_usage b;
            validate_operand_usage c
        | Seal_term (a, b, c) | UnSeal_term (a, b, c) ->
            validate_register_usage a;
            validate_register_usage b;
            validate_register_usage c
        | Fail_term | Halt_term -> ()));
    List.rev !accumulator

  let validate_raw_word ~(parameters : (string * parameter_kind) list) (word : raw_word) :
      Diagnostic.t list =
    List.rev (validate_word_term parameters [] word)

  let lookup (args : ('a * 'b) list) (name : 'a) : 'b option = List.assoc_opt name args

  let substitute_register (args : (string * macro_argument) list) (term : register_term) :
      (register_term, Diagnostic.t list) result =
    match term with
    | Named _ as r -> Ok r
    | Register_parameter n -> (
        match lookup args n with
        | Some (Register_argument r) -> Ok (Named r)
        | _ -> diagnostic (Printf.sprintf "No register argument for $%s." n))

  let substitute_permission (args : (string * macro_argument) list) (term : permission_term) :
      (permission_term, Diagnostic.t list) result =
    match term with
    | Permission_literal _ as p -> Ok p
    | Permission_parameter n -> (
        match lookup args n with
        | Some (Constant_argument (Permission p)) -> Ok (Permission_literal p)
        | _ -> diagnostic (Printf.sprintf "No permission argument for $%s." n))

  let substitute_seal_permission (args : (string * macro_argument) list)
      (term : seal_permission_term) : (seal_permission_term, Diagnostic.t list) result =
    match term with
    | Seal_permission_literal _ as p -> Ok p
    | Seal_permission_parameter n -> (
        match lookup args n with
        | Some (Constant_argument (Seal_permission p)) -> Ok (Seal_permission_literal p)
        | _ -> diagnostic (Printf.sprintf "No seal permission argument for $%s." n))

  let substitute_locality (args : (string * macro_argument) list) (term : locality_term) :
      (locality_term, Diagnostic.t list) result =
    match term with
    | Locality_literal _ as l -> Ok l
    | Locality_parameter n -> (
        match lookup args n with
        | Some (Constant_argument (Locality l)) -> Ok (Locality_literal l)
        | _ -> diagnostic (Printf.sprintf "No locality argument for $%s." n))

  let substitute_constant (args : (string * macro_argument) list) (term : constant_term) :
      (constant_term, Diagnostic.t list) result =
    match term with
    | Permission_locality ((Permission_literal _ as p), l) ->
        Result.map (fun l -> Permission_locality (p, l)) (substitute_locality args l)
    | Permission_locality (Permission_parameter n, l) -> (
        match lookup args n with
        | Some (Constant_argument (Permission p)) ->
            Result.map
              (fun l -> Permission_locality (Permission_literal p, l))
              (substitute_locality args l)
        | Some (Constant_argument (Seal_permission p)) ->
            Result.map
              (fun l -> Seal_permission_locality (Seal_permission_literal p, l))
              (substitute_locality args l)
        | _ -> diagnostic (Printf.sprintf "No permission argument for $%s." n))
    | Seal_permission_locality (p, l) ->
        let* p = substitute_seal_permission args p in
        Result.map (fun l -> Seal_permission_locality (p, l)) (substitute_locality args l)
    | c -> Ok c

  let substitute_constant_as_operand (args : (string * macro_argument) list) (term : constant_term)
      : (operand_term, Diagnostic.t list) result =
    match term with
    | Value_parameter n -> (
        match lookup args n with
        | Some (Constant_argument c) -> Ok (Constant_term c)
        | Some (Register_argument r) -> Ok (Register_term (Named r))
        | None -> diagnostic (Printf.sprintf "No value argument for $%s." n))
    | c -> Result.map (fun c -> Constant_term c) (substitute_constant args c)

  let substitute_operand (args : (string * macro_argument) list) (term : operand_term) :
      (operand_term, Diagnostic.t list) result =
    match term with
    | Register_term r -> Result.map (fun r -> Register_term r) (substitute_register args r)
    | Constant_term c -> substitute_constant_as_operand args c

  let substitute_one_register (c : register_term -> 'a) (args : (string * macro_argument) list)
      (a : register_term) : ('a, Diagnostic.t list) result =
    Result.map c (substitute_register args a)

  let substitute_register_pair (c : register_term -> register_term -> 'a)
      (args : (string * macro_argument) list) (a : register_term) (b : register_term) :
      ('a, Diagnostic.t list) result =
    let* a = substitute_register args a in
    Result.map (c a) (substitute_register args b)

  let substitute_one_operand (c : operand_term -> 'a) (args : (string * macro_argument) list)
      (o : operand_term) : ('a, Diagnostic.t list) result =
    Result.map c (substitute_operand args o)

  let substitute_register_and_operand (c : register_term -> operand_term -> 'a)
      (args : (string * macro_argument) list) (r : register_term) (o : operand_term) :
      ('a, Diagnostic.t list) result =
    let* r = substitute_register args r in
    Result.map (c r) (substitute_operand args o)

  let substitute_register_and_operands (c : register_term -> operand_term -> operand_term -> 'a)
      (args : (string * macro_argument) list) (r : register_term) (a : operand_term)
      (b : operand_term) : ('a, Diagnostic.t list) result =
    let* r = substitute_register args r in
    let* a = substitute_operand args a in
    Result.map (c r a) (substitute_operand args b)

  let substitute_three_registers (c : register_term -> register_term -> register_term -> 'a)
      (args : (string * macro_argument) list) (a : register_term) (b : register_term)
      (d : register_term) : ('a, Diagnostic.t list) result =
    let* a = substitute_register args a in
    let* b = substitute_register args b in
    Result.map (c a b) (substitute_register args d)

  let substitute_instruction (args : (string * macro_argument) list) (term : instruction_term) :
      (instruction_term, Diagnostic.t list) result =
    match term with
    | Jmp_term o -> substitute_one_operand (fun o -> Jmp_term o) args o
    | Jalr_term (a, b) -> substitute_register_pair (fun a b -> Jalr_term (a, b)) args a b
    | Jnz_term (r, o) -> substitute_register_and_operand (fun r o -> Jnz_term (r, o)) args r o
    | ReadSR_term (r, s) -> substitute_one_register (fun r -> ReadSR_term (r, s)) args r
    | WriteSR_term (s, r) -> substitute_one_register (fun r -> WriteSR_term (s, r)) args r
    | Move_term (r, o) -> substitute_register_and_operand (fun r o -> Move_term (r, o)) args r o
    | Load_term (a, b) -> substitute_register_pair (fun a b -> Load_term (a, b)) args a b
    | Store_term (r, o) -> substitute_register_and_operand (fun r o -> Store_term (r, o)) args r o
    | Add_term (r, a, b) ->
        substitute_register_and_operands (fun r a b -> Add_term (r, a, b)) args r a b
    | Sub_term (r, a, b) ->
        substitute_register_and_operands (fun r a b -> Sub_term (r, a, b)) args r a b
    | Mul_term (r, a, b) ->
        substitute_register_and_operands (fun r a b -> Mul_term (r, a, b)) args r a b
    | LAnd_term (r, a, b) ->
        substitute_register_and_operands (fun r a b -> LAnd_term (r, a, b)) args r a b
    | LOr_term (r, a, b) ->
        substitute_register_and_operands (fun r a b -> LOr_term (r, a, b)) args r a b
    | LShiftL_term (r, a, b) ->
        substitute_register_and_operands (fun r a b -> LShiftL_term (r, a, b)) args r a b
    | LShiftR_term (r, a, b) ->
        substitute_register_and_operands (fun r a b -> LShiftR_term (r, a, b)) args r a b
    | Lt_term (r, a, b) ->
        substitute_register_and_operands (fun r a b -> Lt_term (r, a, b)) args r a b
    | Lea_term (r, o) -> substitute_register_and_operand (fun r o -> Lea_term (r, o)) args r o
    | Restrict_term (r, o) ->
        substitute_register_and_operand (fun r o -> Restrict_term (r, o)) args r o
    | SubSeg_term (r, a, b) ->
        substitute_register_and_operands (fun r a b -> SubSeg_term (r, a, b)) args r a b
    | GetL_term (a, b) -> substitute_register_pair (fun a b -> GetL_term (a, b)) args a b
    | GetB_term (a, b) -> substitute_register_pair (fun a b -> GetB_term (a, b)) args a b
    | GetE_term (a, b) -> substitute_register_pair (fun a b -> GetE_term (a, b)) args a b
    | GetA_term (a, b) -> substitute_register_pair (fun a b -> GetA_term (a, b)) args a b
    | GetP_term (a, b) -> substitute_register_pair (fun a b -> GetP_term (a, b)) args a b
    | GetOType_term (a, b) -> substitute_register_pair (fun a b -> GetOType_term (a, b)) args a b
    | GetWType_term (a, b) -> substitute_register_pair (fun a b -> GetWType_term (a, b)) args a b
    | Seal_term (a, b, c) ->
        substitute_three_registers (fun a b c -> Seal_term (a, b, c)) args a b c
    | UnSeal_term (a, b, c) ->
        substitute_three_registers (fun a b c -> UnSeal_term (a, b, c)) args a b c
    | Fail_term -> Ok Fail_term
    | Halt_term -> Ok Halt_term

  let substitute_sealable (args : (string * macro_argument) list) (term : sealable_term) :
      (sealable_term, Diagnostic.t list) result =
    match term with
    | Cap_term (p, l, b, e, a) ->
        let* p = substitute_permission args p in
        Result.map (fun l -> Cap_term (p, l, b, e, a)) (substitute_locality args l)
    | Seal_range_term (p, l, b, e, a) ->
        let* p = substitute_seal_permission args p in
        Result.map (fun l -> Seal_range_term (p, l, b, e, a)) (substitute_locality args l)

  let substitute_word (args : (string * macro_argument) list) (term : raw_word) :
      (raw_word, Diagnostic.t list) result =
    match term with
    | I_term _ as w -> Ok w
    | Sealable_term s -> Result.map (fun s -> Sealable_term s) (substitute_sealable args s)
    | Sentry_term (p, l, b, e, a) ->
        let* p = substitute_permission args p in
        Result.map (fun l -> Sentry_term (p, l, b, e, a)) (substitute_locality args l)
    | Sealed_term (o, s) -> Result.map (fun s -> Sealed_term (o, s)) (substitute_sealable args s)

  let substitute_statement ~(arguments : (string * macro_argument) list) (term : statement) :
      (statement, Diagnostic.t list) result =
    match term with
    | Op op -> Result.map (fun op -> Op op) (substitute_instruction arguments op)
    | Word w -> Result.map (fun w -> Word w) (substitute_word arguments w)

  let substitute_raw_word ~(arguments : (string * macro_argument) list) (w : raw_word) :
      (raw_word, Diagnostic.t list) result =
    substitute_word arguments w

  let substitute_argument ~(arguments : (string * macro_argument) list) (term : macro_argument) :
      (macro_argument, Diagnostic.t list) result =
    match term with
    | Register_argument _ as a -> Ok a
    | Constant_argument (Value_parameter n) -> (
        match lookup arguments n with
        | Some a -> Ok a
        | None -> diagnostic (Printf.sprintf "No argument for $%s." n))
    | Constant_argument c ->
        Result.map (fun c -> Constant_argument c) (substitute_constant arguments c)
end

let valid_parameter_kind (name : string) : bool =
  Option.is_some (Macro_processing.parameter_kind name)

let parameter_kind (name : string) : parameter_kind =
  Option.get (Macro_processing.parameter_kind name)

type source_program =
  (statement, word_term, macro_argument, parameter_kind) Assembly_construction.item list

module Assembly_pipeline = Assembly_construction.Make (Macro_processing)

let assemble_source_program (program : source_program) : (statement list, Diagnostic.t list) result
    =
  Assembly_pipeline.assemble_source_program program

(* Concrete assembly: reject any unexpanded terms, evaluate expressions, and emit
   the handwritten machine's words without changing the public [Asm_ir] paths. *)
module Concrete_assembly = struct
  let eval (config : Runtime_config.t) (expression : expression) : (Z.t, Diagnostic.t list) result =
    match Assembly_construction.Expression.evaluate_with_runtime_config config expression with
    | Ok value -> Ok value
    | Error message -> diagnostic message

  let assemble_register (term : register_term) : (register, Diagnostic.t list) result =
    match term with
    | Named register -> Ok register
    | Register_parameter name ->
        diagnostic (Printf.sprintf "Unexpanded register parameter $%s." name)

  let assemble_permission (term : permission_term) : (permission, Diagnostic.t list) result =
    match term with
    | Permission_literal permission -> Ok permission
    | Permission_parameter name ->
        diagnostic (Printf.sprintf "Unexpanded permission parameter $%s." name)

  let assemble_seal_permission (term : seal_permission_term) :
      (seal_permission, Diagnostic.t list) result =
    match term with
    | Seal_permission_literal permission -> Ok permission
    | Seal_permission_parameter name ->
        diagnostic (Printf.sprintf "Unexpanded seal permission parameter $%s." name)

  let assemble_locality (term : locality_term) : (locality, Diagnostic.t list) result =
    match term with
    | Locality_literal locality -> Ok locality
    | Locality_parameter name ->
        diagnostic (Printf.sprintf "Unexpanded locality parameter $%s." name)

  let assemble_constant (config : Runtime_config.t) (term : constant_term) :
      (Z.t, Diagnostic.t list) result =
    match term with
    | Expression expression -> eval config expression
    | Permission permission -> Ok (Codec.encode_permission permission)
    | Seal_permission permission -> Ok (Codec.encode_seal_permission permission)
    | Permission_locality (permission, locality) ->
        let* permission = assemble_permission permission in
        Result.map (Codec.encode_permission_locality permission) (assemble_locality locality)
    | Seal_permission_locality (permission, locality) ->
        let* permission = assemble_seal_permission permission in
        Result.map (Codec.encode_seal_permission_locality permission) (assemble_locality locality)
    | Word_type word_type -> Ok (Codec.encode_word_type word_type)
    | Locality locality -> Ok (Codec.encode_locality locality)
    | Value_parameter name -> diagnostic (Printf.sprintf "Unexpanded value parameter $%s." name)

  let assemble_operand (config : Runtime_config.t) (term : operand_term) :
      (reg_or_const, Diagnostic.t list) result =
    match term with
    | Register_term register ->
        Result.map (fun register -> Register register) (assemble_register register)
    | Constant_term constant ->
        Result.map (fun constant -> Constant constant) (assemble_constant config constant)

  let assemble_sealable (config : Runtime_config.t) (term : sealable_term) :
      (sealable, Diagnostic.t list) result =
    match term with
    | Cap_term (permission, locality, base, limit, cursor) ->
        let* permission = assemble_permission permission in
        let* locality = assemble_locality locality in
        let* base = eval config base in
        let* limit = eval config limit in
        Result.map
          (fun cursor -> Cap (permission, locality, base, limit, cursor))
          (eval config cursor)
    | Seal_range_term (permission, locality, base, limit, cursor) ->
        let* permission = assemble_seal_permission permission in
        let* locality = assemble_locality locality in
        let* base = eval config base in
        let* limit = eval config limit in
        Result.map
          (fun cursor -> SealRange (permission, locality, base, limit, cursor))
          (eval config cursor)

  let assemble_word (config : Runtime_config.t) (term : word) : (Ast.word, Diagnostic.t list) result
      =
    match term with
    | I_term expression -> Result.map (fun value -> I value) (eval config expression)
    | Sealable_term sealable ->
        Result.map (fun value -> Sealable value) (assemble_sealable config sealable)
    | Sentry_term (permission, locality, base, limit, cursor) ->
        let* permission = assemble_permission permission in
        let* locality = assemble_locality locality in
        let* base = eval config base in
        let* limit = eval config limit in
        Result.map
          (fun cursor -> Sentry (permission, locality, base, limit, cursor))
          (eval config cursor)
    | Sealed_term (object_type, sealable) ->
        let* object_type = eval config object_type in
        Result.map (fun value -> Sealed (object_type, value)) (assemble_sealable config sealable)

  let assemble_instruction (config : Runtime_config.t) (instruction : instruction_term) :
      (instruction, Diagnostic.t list) result =
    let register = assemble_register and operand = assemble_operand config in
    let assemble_register_pair (construct : register * register -> 'a) (first : register_term)
        (second : register_term) : ('a, Diagnostic.t list) result =
      let* first = register first in
      Result.map (fun second -> construct (first, second)) (register second)
    in
    let assemble_register_and_operand (construct : register * reg_or_const -> 'a)
        (destination : register_term) (source : operand_term) : ('a, Diagnostic.t list) result =
      let* destination = register destination in
      Result.map (fun source -> construct (destination, source)) (operand source)
    in
    let assemble_register_and_two_operands
        (construct : register * reg_or_const * reg_or_const -> 'a) (destination : register_term)
        (left : operand_term) (right : operand_term) : ('a, Diagnostic.t list) result =
      let* destination = register destination in
      let* left = operand left in
      Result.map (fun right -> construct (destination, left, right)) (operand right)
    in
    let assemble_three_registers (construct : register * register * register -> 'a)
        (destination : register_term) (source : register_term) (sealing : register_term) :
        ('a, Diagnostic.t list) result =
      let* destination = register destination in
      let* source = register source in
      Result.map (fun sealing -> construct (destination, source, sealing)) (register sealing)
    in
    match instruction with
    | Jalr_term (first, second) ->
        assemble_register_pair (fun (first, second) -> Jalr (first, second)) first second
    | Jmp_term target -> Result.map (fun target -> Jmp target) (operand target)
    | Jnz_term (condition, target) ->
        assemble_register_and_operand
          (fun (condition, target) -> Jnz (condition, target))
          condition target
    | ReadSR_term (destination, system_register) ->
        Result.map (fun destination -> ReadSR (destination, system_register)) (register destination)
    | WriteSR_term (system_register, source) ->
        Result.map (fun source -> WriteSR (system_register, source)) (register source)
    | Move_term (destination, source) ->
        assemble_register_and_operand
          (fun (destination, source) -> Move (destination, source))
          destination source
    | Load_term (destination, source) ->
        assemble_register_pair
          (fun (destination, source) -> Load (destination, source))
          destination source
    | Store_term (destination, source) ->
        assemble_register_and_operand
          (fun (destination, source) -> Store (destination, source))
          destination source
    | Add_term (destination, left, right) ->
        assemble_register_and_two_operands
          (fun (destination, left, right) -> Add (destination, left, right))
          destination left right
    | Sub_term (destination, left, right) ->
        assemble_register_and_two_operands
          (fun (destination, left, right) -> Sub (destination, left, right))
          destination left right
    | Mul_term (destination, left, right) ->
        assemble_register_and_two_operands
          (fun (destination, left, right) -> Mul (destination, left, right))
          destination left right
    | LAnd_term (destination, left, right) ->
        assemble_register_and_two_operands
          (fun (destination, left, right) -> LAnd (destination, left, right))
          destination left right
    | LOr_term (destination, left, right) ->
        assemble_register_and_two_operands
          (fun (destination, left, right) -> LOr (destination, left, right))
          destination left right
    | LShiftL_term (destination, left, right) ->
        assemble_register_and_two_operands
          (fun (destination, left, right) -> LShiftL (destination, left, right))
          destination left right
    | LShiftR_term (destination, left, right) ->
        assemble_register_and_two_operands
          (fun (destination, left, right) -> LShiftR (destination, left, right))
          destination left right
    | Lt_term (destination, left, right) ->
        assemble_register_and_two_operands
          (fun (destination, left, right) -> Lt (destination, left, right))
          destination left right
    | Lea_term (destination, source) ->
        assemble_register_and_operand
          (fun (destination, source) -> Lea (destination, source))
          destination source
    | Restrict_term (destination, source) ->
        assemble_register_and_operand
          (fun (destination, source) -> Restrict (destination, source))
          destination source
    | SubSeg_term (destination, left, right) ->
        assemble_register_and_two_operands
          (fun (destination, left, right) -> SubSeg (destination, left, right))
          destination left right
    | GetL_term (destination, source) ->
        assemble_register_pair
          (fun (destination, source) -> GetL (destination, source))
          destination source
    | GetB_term (destination, source) ->
        assemble_register_pair
          (fun (destination, source) -> GetB (destination, source))
          destination source
    | GetE_term (destination, source) ->
        assemble_register_pair
          (fun (destination, source) -> GetE (destination, source))
          destination source
    | GetA_term (destination, source) ->
        assemble_register_pair
          (fun (destination, source) -> GetA (destination, source))
          destination source
    | GetP_term (destination, source) ->
        assemble_register_pair
          (fun (destination, source) -> GetP (destination, source))
          destination source
    | GetOType_term (destination, source) ->
        assemble_register_pair
          (fun (destination, source) -> GetOType (destination, source))
          destination source
    | GetWType_term (destination, source) ->
        assemble_register_pair
          (fun (destination, source) -> GetWType (destination, source))
          destination source
    | Seal_term (destination, source, sealing) ->
        assemble_three_registers
          (fun (destination, source, sealing) -> Seal (destination, source, sealing))
          destination source sealing
    | UnSeal_term (destination, source, sealing) ->
        assemble_three_registers
          (fun (destination, source, sealing) -> UnSeal (destination, source, sealing))
          destination source sealing
    | Fail_term -> Ok Fail
    | Halt_term -> Ok Halt

  let assemble_program (config : Runtime_config.t) (program : statement list) :
      (Ast.word list, Diagnostic.t list) result =
    let rec loop (words : Ast.word list) (term : statement list) :
        (Ast.word list, Diagnostic.t list) result =
      match term with
      | [] -> Ok (List.rev words)
      | Op instruction :: rest -> (
          let* instruction = assemble_instruction config instruction in
          match Codec.encode instruction with
          | Ok encoded -> loop (I encoded :: words) rest
          | Error error -> diagnostic (Instruction_codec.error_message error))
      | Word word :: rest ->
          let* word = assemble_word config word in
          loop (word :: words) rest
    in
    loop [] program

  let assemble_regfile (config : Runtime_config.t) (entries : regfile_entry list) :
      ((register * Ast.word) list * (system_register * Ast.word) list, Diagnostic.t list) result =
    List.fold_left
      (fun result entry ->
        let* registers, system_registers = result in
        match entry with
        | Register_entry (register, word) ->
            Result.map
              (fun word -> ((register, word) :: registers, system_registers))
              (assemble_word config word)
        | System_register_entry (register, word) ->
            Result.map
              (fun word -> (registers, (register, word) :: system_registers))
              (assemble_word config word))
      (Ok ([], []))
      entries
    |> Result.map (fun (registers, system_registers) ->
        (List.rev registers, List.rev system_registers))
end

include Concrete_assembly
