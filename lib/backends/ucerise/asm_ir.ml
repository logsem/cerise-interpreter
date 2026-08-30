(** Assembly terms preserve symbolic expressions and macro parameters until the shared assembly
    pipeline has expanded macros and resolved labels. The final concrete-assembly phase then
    converts those terms into executable uCerise words. *)

open Ast

(** Public syntax types remain at this façade so their constructor paths stay stable for parsers and
    clients. The processing modules below are private. *)

type expression = Assembly_construction.Expression.t
type register_term = Named of register | Register_parameter of string
type permission_term = Permission_literal of permission | Permission_parameter of string
type locality_term = Locality of locality | Locality_parameter of string

type constant_term =
  | Expression of expression
  | Permission of permission
  | Permission_locality of permission_term * locality_term
  | Parameterized_permission_locality of string * locality_term
  | Locality_constant of locality
  | Value_parameter of string

type operand_term = Register_term of register_term | Constant_term of constant_term

type word_term =
  | I_term of expression
  | Cap_term of permission_term * locality_term * expression * expression * expression

type word = word_term

type instruction_term =
  | Jmp_term of register_term
  | Jnz_term of register_term * register_term
  | Move_term of register_term * operand_term
  | Load_term of register_term * register_term
  | Store_term of register_term * operand_term
  | Add_term of register_term * operand_term * operand_term
  | Sub_term of register_term * operand_term * operand_term
  | Lt_term of register_term * operand_term * operand_term
  | Lea_term of register_term * operand_term
  | Restrict_term of register_term * operand_term
  | SubSeg_term of register_term * operand_term * operand_term
  | IsPtr_term of register_term * register_term
  | GetP_term of register_term * register_term
  | GetL_term of register_term * register_term
  | GetB_term of register_term * register_term
  | GetE_term of register_term * register_term
  | GetA_term of register_term * register_term
  | Fail_term
  | Halt_term
  | LoadU_term of register_term * register_term * operand_term
  | StoreU_term of register_term * operand_term * operand_term
  | PromoteU_term of register_term

type statement = Op of instruction_term | Word of word_term
type program = statement list
type regfile = (register * word_term) list

let parse_register_name (name : string) : register option =
  match String.lowercase_ascii name with
  | "pc" -> Some PC
  | "ddc" | "r0" -> Some (Reg 0)
  | "stk" | "r31" -> Some (Reg 31)
  | name when String.length name > 1 && name.[0] = 'r' -> (
      match int_of_string_opt (String.sub name 1 (String.length name - 1)) with
      | Some n when n >= 0 && n <= 31 -> Some (Reg n)
      | _ -> None)
  | _ -> None

let parse_permission (term : string) : permission option =
  match term with
  | "O" -> Some O
  | "E" -> Some E
  | "RO" -> Some RO
  | "RX" -> Some RX
  | "RW" -> Some RW
  | "RWX" -> Some RWX
  | "RWL" -> Some RWL
  | "RWLX" -> Some RWLX
  | "URW" -> Some URW
  | "URWX" -> Some URWX
  | "URWL" -> Some URWL
  | "URWLX" -> Some URWLX
  | _ -> None

let parse_locality (term : string) : locality option =
  match term with "GLOBAL" | "Global" -> Some Global | "LOCAL" | "Local" -> Some Local | _ -> None

type parameter_kind =
  | Register_kind
  | Expression_kind
  | Value_kind
  | Permission_kind
  | Locality_kind

type macro_argument = Register_argument of register | Constant_argument of constant_term

(** Macro validation and hygienic parameter substitution. *)
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
    | "locality" -> Some Locality_kind
    | _ -> None

  let parameter_kind_name (term : parameter_kind) : string =
    match term with
    | Register_kind -> "reg"
    | Expression_kind -> "expr"
    | Value_kind -> "value"
    | Permission_kind -> "perm"
    | Locality_kind -> "locality"

  let argument_kind (term : macro_argument) : parameter_kind =
    match term with
    | Register_argument _ -> Register_kind
    | Constant_argument (Expression _) -> Expression_kind
    | Constant_argument (Permission _) -> Permission_kind
    | Constant_argument (Locality_constant _) -> Locality_kind
    | Constant_argument _ -> Value_kind

  let accepts_argument (k : parameter_kind) (a : macro_argument) : bool =
    k = Value_kind || k = argument_kind a

  let expression_of_argument (term : macro_argument) : expression option =
    match term with Constant_argument (Expression e) -> Some e | _ -> None

  let map_constant (f : expression -> expression) (term : constant_term) : constant_term =
    match term with Expression e -> Expression (f e) | c -> c

  let map_operand (f : expression -> expression) (term : operand_term) : operand_term =
    match term with
    | Register_term r -> Register_term r
    | Constant_term c -> Constant_term (map_constant f c)

  let map_instruction (f : expression -> expression) (term : instruction_term) : instruction_term =
    match term with
    | Move_term (r, o) -> Move_term (r, map_operand f o)
    | Store_term (r, o) -> Store_term (r, map_operand f o)
    | Add_term (r, a, b) -> Add_term (r, map_operand f a, map_operand f b)
    | Sub_term (r, a, b) -> Sub_term (r, map_operand f a, map_operand f b)
    | Lt_term (r, a, b) -> Lt_term (r, map_operand f a, map_operand f b)
    | Lea_term (r, o) -> Lea_term (r, map_operand f o)
    | Restrict_term (r, o) -> Restrict_term (r, map_operand f o)
    | SubSeg_term (r, a, b) -> SubSeg_term (r, map_operand f a, map_operand f b)
    | LoadU_term (r, s, o) -> LoadU_term (r, s, map_operand f o)
    | StoreU_term (r, a, b) -> StoreU_term (r, map_operand f a, map_operand f b)
    | op -> op

  let map_word (f : expression -> expression) (term : raw_word) : raw_word =
    match term with
    | I_term e -> I_term (f e)
    | Cap_term (p, l, b, e, a) -> Cap_term (p, l, f b, f e, f a)

  let map_statement_expressions (f : expression -> expression) (term : statement) : statement =
    match term with Op op -> Op (map_instruction f op) | Word w -> Word (map_word f w)

  let map_raw_word_expressions (mapper : expression -> expression) (word : word_term) : word_term =
    map_word mapper word

  let map_regfile_expressions (f : expression -> expression) :
      ('a * raw_word) list -> ('a * raw_word) list =
    List.map (fun (r, w) -> (r, map_word f w))

  let map_argument_expressions (f : expression -> expression) (term : macro_argument) :
      macro_argument =
    match term with Constant_argument c -> Constant_argument (map_constant f c) | a -> a

  let rec expression_parameters (parameters : (string * parameter_kind) list)
      (accumulator : Diagnostic.t list) (term : expression) : Diagnostic.t list =
    match term with
    | Assembly_construction.Expression.Parameter name -> (
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
        expression_parameters parameters (expression_parameters parameters accumulator a) b
    | _ -> accumulator

  let validate_register (parameters : (string * parameter_kind) list)
      (accumulator : Diagnostic.t list) (term : register_term) : Diagnostic.t list =
    match term with
    | Named _ -> accumulator
    | Register_parameter name -> (
        match List.assoc_opt name parameters with
        | Some (Register_kind | Value_kind) -> accumulator
        | _ ->
            Diagnostic.error (Printf.sprintf "Invalid register parameter $%s." name) :: accumulator)

  let validate_locality (parameters : (string * parameter_kind) list)
      (accumulator : Diagnostic.t list) (term : locality_term) : Diagnostic.t list =
    match term with
    | Locality _ -> accumulator
    | Locality_parameter name -> (
        match List.assoc_opt name parameters with
        | Some Locality_kind -> accumulator
        | _ ->
            Diagnostic.error (Printf.sprintf "Invalid locality parameter $%s." name) :: accumulator)

  let validate_permission (parameters : (string * parameter_kind) list)
      (accumulator : Diagnostic.t list) (term : permission_term) : Diagnostic.t list =
    match term with
    | Permission_literal _ -> accumulator
    | Permission_parameter name -> (
        match List.assoc_opt name parameters with
        | Some Permission_kind -> accumulator
        | _ ->
            Diagnostic.error (Printf.sprintf "Invalid permission parameter $%s." name)
            :: accumulator)

  let validate_constant (parameters : (string * parameter_kind) list)
      (accumulator : Diagnostic.t list) (term : constant_term) : Diagnostic.t list =
    match term with
    | Expression e -> expression_parameters parameters accumulator e
    | Value_parameter name -> (
        match List.assoc_opt name parameters with
        | Some (Value_kind | Expression_kind | Permission_kind | Locality_kind) -> accumulator
        | _ -> Diagnostic.error (Printf.sprintf "Invalid value parameter $%s." name) :: accumulator)
    | Permission_locality (permission, locality) ->
        validate_locality parameters
          (validate_permission parameters accumulator permission)
          locality
    | Parameterized_permission_locality (name, locality) ->
        let accumulator =
          match List.assoc_opt name parameters with
          | Some Permission_kind -> accumulator
          | _ ->
              Diagnostic.error (Printf.sprintf "Invalid permission parameter $%s." name)
              :: accumulator
        in
        validate_locality parameters accumulator locality
    | Permission _ | Locality_constant _ -> accumulator

  let validate_operand (parameters : (string * parameter_kind) list)
      (accumulator : Diagnostic.t list) (term : operand_term) : Diagnostic.t list =
    match term with
    | Register_term r -> validate_register parameters accumulator r
    | Constant_term c -> validate_constant parameters accumulator c

  let validate_word (parameters : (string * parameter_kind) list) (accumulator : Diagnostic.t list)
      (term : raw_word) : Diagnostic.t list =
    match term with
    | I_term e -> expression_parameters parameters accumulator e
    | Cap_term (permission, locality, b, e, a) ->
        let accumulator = validate_permission parameters accumulator permission in
        let accumulator = validate_locality parameters accumulator locality in
        let accumulator = expression_parameters parameters accumulator b in
        let accumulator = expression_parameters parameters accumulator e in
        expression_parameters parameters accumulator a

  let validate_statement ~(parameters : (string * parameter_kind) list) (statement : statement) :
      Diagnostic.t list =
    let accumulator = ref [] in
    let validate_register_usage (register : register_term) : unit =
      accumulator := validate_register parameters !accumulator register
    and validate_operand_usage (operand : operand_term) : unit =
      accumulator := validate_operand parameters !accumulator operand
    in
    (match statement with
    | Word word -> accumulator := validate_word parameters !accumulator word
    | Op op -> (
        match op with
        | Jmp_term a | PromoteU_term a -> validate_register_usage a
        | Jnz_term (a, b)
        | Load_term (a, b)
        | IsPtr_term (a, b)
        | GetP_term (a, b)
        | GetL_term (a, b)
        | GetB_term (a, b)
        | GetE_term (a, b)
        | GetA_term (a, b) ->
            validate_register_usage a;
            validate_register_usage b
        | Move_term (a, b) | Store_term (a, b) | Lea_term (a, b) | Restrict_term (a, b) ->
            validate_register_usage a;
            validate_operand_usage b
        | Add_term (a, b, c)
        | Sub_term (a, b, c)
        | Lt_term (a, b, c)
        | SubSeg_term (a, b, c)
        | StoreU_term (a, b, c) ->
            validate_register_usage a;
            validate_operand_usage b;
            validate_operand_usage c
        | LoadU_term (a, b, c) ->
            validate_register_usage a;
            validate_register_usage b;
            validate_operand_usage c
        | Fail_term | Halt_term -> ()));
    List.rev !accumulator

  let validate_raw_word ~(parameters : (string * parameter_kind) list) (word : raw_word) :
      Diagnostic.t list =
    validate_word parameters [] word |> List.rev

  let lookup (args : ('a * 'b) list) (n : 'a) : 'b option = List.assoc_opt n args

  let substitute_register (args : (string * macro_argument) list) (term : register_term) :
      (register_term, Diagnostic.t list) result =
    match term with
    | Named _ as r -> Ok r
    | Register_parameter n -> (
        match lookup args n with
        | Some (Register_argument r) -> Ok (Named r)
        | _ -> Error [ Diagnostic.error (Printf.sprintf "No register argument for $%s." n) ])

  let substitute_locality (args : (string * macro_argument) list) (term : locality_term) :
      (locality_term, Diagnostic.t list) result =
    match term with
    | Locality _ as l -> Ok l
    | Locality_parameter n -> (
        match lookup args n with
        | Some (Constant_argument (Locality_constant l)) -> Ok (Locality l)
        | _ -> Error [ Diagnostic.error (Printf.sprintf "No locality argument for $%s." n) ])

  let substitute_permission (args : (string * macro_argument) list) (term : permission_term) :
      (permission_term, Diagnostic.t list) result =
    match term with
    | Permission_literal _ as p -> Ok p
    | Permission_parameter n -> (
        match lookup args n with
        | Some (Constant_argument (Permission p)) -> Ok (Permission_literal p)
        | _ -> Error [ Diagnostic.error (Printf.sprintf "No permission argument for $%s." n) ])

  let substitute_constant (args : (string * macro_argument) list) (term : constant_term) :
      (constant_term, Diagnostic.t list) result =
    match term with
    | Permission_locality (p, l) ->
        Result.bind (substitute_permission args p) (fun p ->
            Result.map (fun l -> Permission_locality (p, l)) (substitute_locality args l))
    | Parameterized_permission_locality (n, l) -> (
        match lookup args n with
        | Some (Constant_argument (Permission p)) ->
            Result.map
              (fun l -> Permission_locality (Permission_literal p, l))
              (substitute_locality args l)
        | _ -> Error [ Diagnostic.error (Printf.sprintf "No permission argument for $%s." n) ])
    | c -> Ok c

  let substitute_constant_operand (args : (string * macro_argument) list) (term : constant_term) :
      (operand_term, Diagnostic.t list) result =
    match term with
    | Value_parameter n -> (
        match lookup args n with
        | Some (Constant_argument c) -> Ok (Constant_term c)
        | Some (Register_argument r) -> Ok (Register_term (Named r))
        | None -> Error [ Diagnostic.error (Printf.sprintf "No value argument for $%s." n) ])
    | c -> Result.map (fun c -> Constant_term c) (substitute_constant args c)

  let substitute_operand (args : (string * macro_argument) list) (term : operand_term) :
      (operand_term, Diagnostic.t list) result =
    match term with
    | Register_term r -> Result.map (fun r -> Register_term r) (substitute_register args r)
    | Constant_term c -> substitute_constant_operand args c

  let substitute_register_with (c : register_term -> 'a) (args : (string * macro_argument) list)
      (a : register_term) : ('a, Diagnostic.t list) result =
    Result.map c (substitute_register args a)

  let substitute_two_registers_with (c : register_term -> register_term -> 'a)
      (args : (string * macro_argument) list) (a : register_term) (b : register_term) :
      ('a, Diagnostic.t list) result =
    Result.bind (substitute_register args a) (fun a ->
        Result.map (c a) (substitute_register args b))

  let substitute_register_and_operand_with (c : register_term -> operand_term -> 'a)
      (args : (string * macro_argument) list) (r : register_term) (o : operand_term) :
      ('a, Diagnostic.t list) result =
    Result.bind (substitute_register args r) (fun r -> Result.map (c r) (substitute_operand args o))

  let substitute_register_and_operands_with
      (c : register_term -> operand_term -> operand_term -> 'a)
      (args : (string * macro_argument) list) (r : register_term) (a : operand_term)
      (b : operand_term) : ('a, Diagnostic.t list) result =
    Result.bind (substitute_register args r) (fun r ->
        Result.bind (substitute_operand args a) (fun a ->
            Result.map (c r a) (substitute_operand args b)))

  let substitute_registers_and_operand_with
      (c : register_term -> register_term -> operand_term -> 'a)
      (args : (string * macro_argument) list) (r : register_term) (s : register_term)
      (o : operand_term) : ('a, Diagnostic.t list) result =
    Result.bind (substitute_register args r) (fun r ->
        Result.bind (substitute_register args s) (fun s ->
            Result.map (c r s) (substitute_operand args o)))

  let substitute_instruction (args : (string * macro_argument) list) (term : instruction_term) :
      (instruction_term, Diagnostic.t list) result =
    match term with
    | Jmp_term a -> substitute_register_with (fun a -> Jmp_term a) args a
    | Jnz_term (a, b) -> substitute_two_registers_with (fun a b -> Jnz_term (a, b)) args a b
    | Move_term (r, o) ->
        substitute_register_and_operand_with (fun r o -> Move_term (r, o)) args r o
    | Load_term (a, b) -> substitute_two_registers_with (fun a b -> Load_term (a, b)) args a b
    | Store_term (r, o) ->
        substitute_register_and_operand_with (fun r o -> Store_term (r, o)) args r o
    | Add_term (r, a, b) ->
        substitute_register_and_operands_with (fun r a b -> Add_term (r, a, b)) args r a b
    | Sub_term (r, a, b) ->
        substitute_register_and_operands_with (fun r a b -> Sub_term (r, a, b)) args r a b
    | Lt_term (r, a, b) ->
        substitute_register_and_operands_with (fun r a b -> Lt_term (r, a, b)) args r a b
    | Lea_term (r, o) -> substitute_register_and_operand_with (fun r o -> Lea_term (r, o)) args r o
    | Restrict_term (r, o) ->
        substitute_register_and_operand_with (fun r o -> Restrict_term (r, o)) args r o
    | SubSeg_term (r, a, b) ->
        substitute_register_and_operands_with (fun r a b -> SubSeg_term (r, a, b)) args r a b
    | IsPtr_term (a, b) -> substitute_two_registers_with (fun a b -> IsPtr_term (a, b)) args a b
    | GetP_term (a, b) -> substitute_two_registers_with (fun a b -> GetP_term (a, b)) args a b
    | GetL_term (a, b) -> substitute_two_registers_with (fun a b -> GetL_term (a, b)) args a b
    | GetB_term (a, b) -> substitute_two_registers_with (fun a b -> GetB_term (a, b)) args a b
    | GetE_term (a, b) -> substitute_two_registers_with (fun a b -> GetE_term (a, b)) args a b
    | GetA_term (a, b) -> substitute_two_registers_with (fun a b -> GetA_term (a, b)) args a b
    | LoadU_term (r, s, o) ->
        substitute_registers_and_operand_with (fun r s o -> LoadU_term (r, s, o)) args r s o
    | StoreU_term (r, a, b) ->
        substitute_register_and_operands_with (fun r a b -> StoreU_term (r, a, b)) args r a b
    | PromoteU_term r -> substitute_register_with (fun r -> PromoteU_term r) args r
    | Fail_term -> Ok Fail_term
    | Halt_term -> Ok Halt_term

  let substitute_word (args : (string * macro_argument) list) (term : raw_word) :
      (raw_word, Diagnostic.t list) result =
    match term with
    | I_term _ as w -> Ok w
    | Cap_term (p, l, b, e, a) ->
        Result.bind (substitute_permission args p) (fun p ->
            Result.map (fun l -> Cap_term (p, l, b, e, a)) (substitute_locality args l))

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
        | None -> Error [ Diagnostic.error (Printf.sprintf "No argument for $%s." n) ])
    | Constant_argument c ->
        Result.map (fun c -> Constant_argument c) (substitute_constant arguments c)
end

let valid_parameter_kind (name : string) : bool =
  Option.is_some (Macro_processing.parameter_kind name)

let parameter_kind (name : string) : parameter_kind =
  Option.get (Macro_processing.parameter_kind name)

type source_program =
  (statement, word_term, macro_argument, parameter_kind) Assembly_construction.item list

module Assembler = Assembly_construction.Make (Macro_processing)

let assemble_source_program (program : source_program) : (statement list, Diagnostic.t list) result
    =
  Assembler.assemble_source_program program

(** Concrete assembly starts only after the assembler has eliminated macro parameters; encountering
    one here therefore reports an internal pipeline invariant as a user-facing diagnostic instead of
    raising. *)

module Concrete_assembly = struct
  let diagnostic (message : string) : ('a, Diagnostic.t list) result =
    Error [ Diagnostic.error message ]

  let ( let* ) (type value next error) (result : (value, error) result)
      (continuation : value -> (next, error) result) : (next, error) result =
    Result.bind result continuation

  let eval (config : Runtime_config.t) (expression : expression) : (Z.t, Diagnostic.t list) result =
    match Assembly_construction.Expression.evaluate_with_runtime_config config expression with
    | Ok value -> Ok value
    | Error message -> diagnostic message

  let assemble_permission (term : permission_term) : (permission, Diagnostic.t list) result =
    match term with
    | Permission_literal permission -> Ok permission
    | Permission_parameter name ->
        diagnostic (Printf.sprintf "Unexpanded permission parameter $%s." name)

  let assemble_locality (term : locality_term) : (locality, Diagnostic.t list) result =
    match term with
    | Locality locality -> Ok locality
    | Locality_parameter name ->
        diagnostic (Printf.sprintf "Unexpanded locality parameter $%s." name)

  let assemble_word (config : Runtime_config.t) (term : word) : (Ast.word, Diagnostic.t list) result
      =
    match term with
    | I_term expression -> Result.map (fun value -> I value) (eval config expression)
    | Cap_term (permission, locality, base, limit, cursor) ->
        let* permission = assemble_permission permission in
        let* locality = assemble_locality locality in
        let* base = eval config base in
        let* limit = eval config limit in
        Result.map
          (fun cursor -> Cap (Cap (permission, locality, base, limit, cursor)))
          (eval config cursor)

  let assemble_register (term : register_term) : (register, Diagnostic.t list) result =
    match term with
    | Named register -> Ok register
    | Register_parameter name ->
        diagnostic (Printf.sprintf "Unexpanded register parameter $%s." name)

  let assemble_constant (config : Runtime_config.t) (term : constant_term) :
      (Z.t, Diagnostic.t list) result =
    match term with
    | Expression expression -> eval config expression
    | Permission permission -> Ok (Codec.encode_permission permission)
    | Permission_locality (permission, locality) ->
        let* permission = assemble_permission permission in
        Result.map (Codec.encode_permission_locality permission) (assemble_locality locality)
    | Parameterized_permission_locality (name, _) ->
        diagnostic (Printf.sprintf "Unexpanded permission parameter $%s." name)
    | Locality_constant locality -> Ok (Codec.encode_locality locality)
    | Value_parameter name -> diagnostic (Printf.sprintf "Unexpanded value parameter $%s." name)

  let assemble_operand (config : Runtime_config.t) (term : operand_term) :
      (reg_or_const, Diagnostic.t list) result =
    match term with
    | Register_term register ->
        Result.map (fun value -> Register value) (assemble_register register)
    | Constant_term constant ->
        Result.map (fun value -> Constant value) (assemble_constant config constant)

  let assemble_instruction (config : Runtime_config.t) (instruction : instruction_term) :
      (instruction, Diagnostic.t list) result =
    let assemble_register_term = assemble_register
    and assemble_operand_term = assemble_operand config in
    let assemble_two_registers_with (constructor : register * register -> 'a)
        (first : register_term) (second : register_term) : ('a, Diagnostic.t list) result =
      let* first = assemble_register_term first in
      Result.map (fun second -> constructor (first, second)) (assemble_register_term second)
    in
    let assemble_register_and_operand_with (constructor : register * reg_or_const -> 'a)
        (first : register_term) (second : operand_term) : ('a, Diagnostic.t list) result =
      let* first = assemble_register_term first in
      Result.map (fun second -> constructor (first, second)) (assemble_operand_term second)
    in
    let assemble_register_and_operands_with
        (constructor : register * reg_or_const * reg_or_const -> 'a) (first : register_term)
        (second : operand_term) (third : operand_term) : ('a, Diagnostic.t list) result =
      let* first = assemble_register_term first in
      let* second = assemble_operand_term second in
      Result.map (fun third -> constructor (first, second, third)) (assemble_operand_term third)
    in
    let assemble_two_registers_and_operand_with
        (constructor : register * register * reg_or_const -> 'a) (first : register_term)
        (second : register_term) (third : operand_term) : ('a, Diagnostic.t list) result =
      let* first = assemble_register_term first in
      let* second = assemble_register_term second in
      Result.map (fun third -> constructor (first, second, third)) (assemble_operand_term third)
    in
    match instruction with
    | Jmp_term value -> Result.map (fun value -> Jmp value) (assemble_register_term value)
    | Jnz_term (a, b) -> assemble_two_registers_with (fun (a, b) -> Jnz (a, b)) a b
    | Move_term (a, b) -> assemble_register_and_operand_with (fun (a, b) -> Move (a, b)) a b
    | Load_term (a, b) -> assemble_two_registers_with (fun (a, b) -> Load (a, b)) a b
    | Store_term (a, b) -> assemble_register_and_operand_with (fun (a, b) -> Store (a, b)) a b
    | Add_term (a, b, c) ->
        assemble_register_and_operands_with (fun (a, b, c) -> Add (a, b, c)) a b c
    | Sub_term (a, b, c) ->
        assemble_register_and_operands_with (fun (a, b, c) -> Sub (a, b, c)) a b c
    | Lt_term (a, b, c) -> assemble_register_and_operands_with (fun (a, b, c) -> Lt (a, b, c)) a b c
    | Lea_term (a, b) -> assemble_register_and_operand_with (fun (a, b) -> Lea (a, b)) a b
    | Restrict_term (a, b) -> assemble_register_and_operand_with (fun (a, b) -> Restrict (a, b)) a b
    | SubSeg_term (a, b, c) ->
        assemble_register_and_operands_with (fun (a, b, c) -> SubSeg (a, b, c)) a b c
    | IsPtr_term (a, b) -> assemble_two_registers_with (fun (a, b) -> IsPtr (a, b)) a b
    | GetP_term (a, b) -> assemble_two_registers_with (fun (a, b) -> GetP (a, b)) a b
    | GetL_term (a, b) -> assemble_two_registers_with (fun (a, b) -> GetL (a, b)) a b
    | GetB_term (a, b) -> assemble_two_registers_with (fun (a, b) -> GetB (a, b)) a b
    | GetE_term (a, b) -> assemble_two_registers_with (fun (a, b) -> GetE (a, b)) a b
    | GetA_term (a, b) -> assemble_two_registers_with (fun (a, b) -> GetA (a, b)) a b
    | Fail_term -> Ok Fail
    | Halt_term -> Ok Halt
    | LoadU_term (a, b, c) ->
        assemble_two_registers_and_operand_with (fun (a, b, c) -> LoadU (a, b, c)) a b c
    | StoreU_term (a, b, c) ->
        assemble_register_and_operands_with (fun (a, b, c) -> StoreU (a, b, c)) a b c
    | PromoteU_term value -> Result.map (fun value -> PromoteU value) (assemble_register_term value)

  let assemble_program (config : Runtime_config.t) (program : statement list) :
      (Ast.word list, Diagnostic.t list) result =
    let rec loop (words : Ast.word list) (term : statement list) :
        (Ast.word list, Diagnostic.t list) result =
      match term with
      | [] -> Ok (List.rev words)
      | statement :: rest -> (
          match statement with
          | Word term ->
              let* word = assemble_word config term in
              loop (word :: words) rest
          | Op term -> (
              let* instruction = assemble_instruction config term in
              match Codec.encode instruction with
              | Ok encoded -> loop (I encoded :: words) rest
              | Error error -> diagnostic (Instruction_codec.error_message error)))
    in
    loop [] program

  let assemble_regfile (config : Runtime_config.t) (entries : ('a * word) list) :
      (('a * Ast.word) list, Diagnostic.t list) result =
    List.fold_left
      (fun result (register, term) ->
        let* entries = result in
        Result.map (fun word -> (register, word) :: entries) (assemble_word config term))
      (Ok []) entries
    |> Result.map List.rev
end

include Concrete_assembly
