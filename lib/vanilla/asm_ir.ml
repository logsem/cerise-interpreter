open Ast

type expression = Assembly_construction.Expression.t
type register_term = Named of register | Register_parameter of string
type permission_term = Permission_literal of permission | Permission_parameter of string

type seal_permission_term =
  | Seal_permission_literal of seal_permission
  | Seal_permission_parameter of string

type constant_term =
  | Expression of expression
  | Permission of permission
  | Seal_permission of seal_permission
  | Word_type of word_type
  | Value_parameter of string

type operand_term = Register_term of register_term | Constant_term of constant_term

type sealable_term =
  | Cap_term of permission_term * expression * expression * expression
  | SealRange_term of seal_permission_term * expression * expression * expression

type word_term =
  | I_term of expression
  | Sealable_term of sealable_term
  | Sealed_term of expression * sealable_term

type instruction_term =
  | Jmp_term of register_term
  | Jnz_term of register_term * register_term
  | Move_term of register_term * operand_term
  | Load_term of register_term * register_term
  | Store_term of register_term * operand_term
  | Add_term of register_term * operand_term * operand_term
  | Sub_term of register_term * operand_term * operand_term
  | Mul_term of register_term * operand_term * operand_term
  | Rem_term of register_term * operand_term * operand_term
  | Div_term of register_term * operand_term * operand_term
  | Lt_term of register_term * operand_term * operand_term
  | Lea_term of register_term * operand_term
  | Restrict_term of register_term * operand_term
  | SubSeg_term of register_term * operand_term * operand_term
  | GetB_term of register_term * register_term
  | GetE_term of register_term * register_term
  | GetA_term of register_term * register_term
  | GetP_term of register_term * register_term
  | GetOType_term of register_term * register_term
  | GetWType_term of register_term * register_term
  | Seal_term of register_term * register_term * register_term
  | UnSeal_term of register_term * register_term * register_term
  | Invoke_term of register_term * register_term
  | Fail_term
  | Halt_term

type statement = Op of instruction_term | Word of word_term
type program = statement list
type regfile = (register * word_term) list
type word = word_term

let parse_register_name name =
  match String.lowercase_ascii name with
  | "pc" -> Some PC
  | "ddc" -> Some (Reg 0)
  | "r0" -> Some (Reg 0)
  | "stk" -> Some (Reg 31)
  | "r31" -> Some (Reg 31)
  | name when String.length name > 1 && name.[0] = 'r' -> (
      match int_of_string_opt (String.sub name 1 (String.length name - 1)) with
      | Some n when n >= 0 && n <= 31 -> Some (Reg n)
      | _ -> None)
  | _ -> None

let parse_permission = function
  | "O" -> Some O
  | "E" -> Some E
  | "RO" -> Some RO
  | "RX" -> Some RX
  | "RW" -> Some RW
  | "RWX" -> Some RWX
  | _ -> None

let parse_seal_permission = function
  | "SO" -> Some (false, false)
  | "S" -> Some (true, false)
  | "U" -> Some (false, true)
  | "SU" -> Some (true, true)
  | _ -> None

let parse_word_type = function
  | "Int" -> Some Integer
  | "Cap" -> Some Capability
  | "SealRange" -> Some Seal_range
  | "Sealed" -> Some Sealed
  | _ -> None

type parameter_kind =
  | Register_kind
  | Expression_kind
  | Value_kind
  | Permission_kind
  | Seal_permission_kind
  | Word_type_kind

type macro_argument = Register_argument of register | Constant_argument of constant_term

module Syntax = struct
  type nonrec statement = statement
  type raw_word = word_term
  type nonrec macro_argument = macro_argument
  type nonrec parameter_kind = parameter_kind

  let statement_of_raw_word word = Word word

  let parameter_kind = function
    | "reg" -> Some Register_kind
    | "expr" -> Some Expression_kind
    | "value" -> Some Value_kind
    | "perm" -> Some Permission_kind
    | "sealperm" -> Some Seal_permission_kind
    | "wtype" -> Some Word_type_kind
    | _ -> None

  let parameter_kind_name = function
    | Register_kind -> "reg"
    | Expression_kind -> "expr"
    | Value_kind -> "value"
    | Permission_kind -> "perm"
    | Seal_permission_kind -> "sealperm"
    | Word_type_kind -> "wtype"

  let argument_kind = function
    | Register_argument _ -> Register_kind
    | Constant_argument (Expression _) -> Expression_kind
    | Constant_argument (Permission _) -> Permission_kind
    | Constant_argument (Seal_permission _) -> Seal_permission_kind
    | Constant_argument (Word_type _) -> Word_type_kind
    | Constant_argument (Value_parameter _) -> Value_kind

  let accepts_argument kind argument =
    match (kind, argument_kind argument) with
    | Value_kind, _ -> true
    | Expression_kind, Expression_kind
    | Register_kind, Register_kind
    | Permission_kind, Permission_kind
    | Seal_permission_kind, Seal_permission_kind
    | Word_type_kind, Word_type_kind ->
        true
    | _ -> false

  let expression_of_argument = function Constant_argument (Expression e) -> Some e | _ -> None
  let map_register _ r = r
  let map_constant f = function Expression e -> Expression (f e) | c -> c

  let map_operand f = function
    | Register_term r -> Register_term r
    | Constant_term c -> Constant_term (map_constant f c)

  let map_instruction f = function
    | Jmp_term r -> Jmp_term r
    | Jnz_term (a, b) -> Jnz_term (a, b)
    | Load_term (a, b) -> Load_term (a, b)
    | GetB_term (a, b) -> GetB_term (a, b)
    | GetE_term (a, b) -> GetE_term (a, b)
    | GetA_term (a, b) -> GetA_term (a, b)
    | GetP_term (a, b) -> GetP_term (a, b)
    | GetOType_term (a, b) -> GetOType_term (a, b)
    | GetWType_term (a, b) -> GetWType_term (a, b)
    | Invoke_term (a, b) -> Invoke_term (a, b)
    | Seal_term (a, b, c) -> Seal_term (a, b, c)
    | UnSeal_term (a, b, c) -> UnSeal_term (a, b, c)
    | Move_term (r, o) -> Move_term (r, map_operand f o)
    | Store_term (r, o) -> Store_term (r, map_operand f o)
    | Lea_term (r, o) -> Lea_term (r, map_operand f o)
    | Restrict_term (r, o) -> Restrict_term (r, map_operand f o)
    | Add_term (r, a, b) -> Add_term (r, map_operand f a, map_operand f b)
    | Sub_term (r, a, b) -> Sub_term (r, map_operand f a, map_operand f b)
    | Mul_term (r, a, b) -> Mul_term (r, map_operand f a, map_operand f b)
    | Rem_term (r, a, b) -> Rem_term (r, map_operand f a, map_operand f b)
    | Div_term (r, a, b) -> Div_term (r, map_operand f a, map_operand f b)
    | Lt_term (r, a, b) -> Lt_term (r, map_operand f a, map_operand f b)
    | SubSeg_term (r, a, b) -> SubSeg_term (r, map_operand f a, map_operand f b)
    | Fail_term -> Fail_term
    | Halt_term -> Halt_term

  let map_sealable f = function
    | Cap_term (p, b, e, a) -> Cap_term (p, f b, f e, f a)
    | SealRange_term (p, b, e, a) -> SealRange_term (p, f b, f e, f a)

  let map_word f = function
    | I_term e -> I_term (f e)
    | Sealable_term s -> Sealable_term (map_sealable f s)
    | Sealed_term (o, s) -> Sealed_term (f o, map_sealable f s)

  let map_statement_expressions f = function
    | Op op -> Op (map_instruction f op)
    | Word w -> Word (map_word f w)

  let map_raw_word_expressions = map_word

  let map_argument_expressions f = function
    | Constant_argument c -> Constant_argument (map_constant f c)
    | a -> a

  let rec expression_parameters parameters acc = function
    | Assembly_construction.Expression.Parameter name -> (
        match List.assoc_opt name parameters with
        | Some (Expression_kind | Value_kind) -> acc
        | Some _ ->
            Diagnostic.error (Printf.sprintf "$%s is not an expression parameter." name) :: acc
        | None -> Diagnostic.error (Printf.sprintf "Unknown macro parameter $%s." name) :: acc)
    | Add (a, b) | Subtract (a, b) | Multiply (a, b) | Logand (a, b) | Logor (a, b)
    | Shift_left (a, b) | Shift_right (a, b) ->
        expression_parameters parameters (expression_parameters parameters acc a) b
    | _ -> acc

  let validate_register parameters acc = function
    | Named _ -> acc
    | Register_parameter n -> (
        match List.assoc_opt n parameters with
        | Some (Register_kind | Value_kind) -> acc
        | _ -> Diagnostic.error (Printf.sprintf "Invalid register parameter $%s." n) :: acc)

  let validate_constant parameters acc = function
    | Expression e -> expression_parameters parameters acc e
    | Value_parameter n -> (
        match List.assoc_opt n parameters with
        | Some
            (Value_kind | Expression_kind | Permission_kind | Seal_permission_kind | Word_type_kind)
          ->
            acc
        | _ -> Diagnostic.error (Printf.sprintf "Invalid value parameter $%s." n) :: acc)
    | _ -> acc

  let validate_operand parameters acc = function
    | Register_term r -> validate_register parameters acc r
    | Constant_term c -> validate_constant parameters acc c

  let validate_permission parameters acc = function
    | Permission_literal _ -> acc
    | Permission_parameter name -> (
        match List.assoc_opt name parameters with
        | Some Permission_kind -> acc
        | _ -> Diagnostic.error (Printf.sprintf "Invalid permission parameter $%s." name) :: acc)

  let validate_seal_permission parameters acc = function
    | Seal_permission_literal _ -> acc
    | Seal_permission_parameter name -> (
        match List.assoc_opt name parameters with
        | Some Seal_permission_kind -> acc
        | _ ->
            Diagnostic.error (Printf.sprintf "Invalid seal-permission parameter $%s." name) :: acc)

  let validate_word_terms parameters acc = function
    | I_term _ -> acc
    | Sealable_term (Cap_term (permission, _, _, _))
    | Sealed_term (_, Cap_term (permission, _, _, _)) ->
        validate_permission parameters acc permission
    | Sealable_term (SealRange_term (permission, _, _, _))
    | Sealed_term (_, SealRange_term (permission, _, _, _)) ->
        validate_seal_permission parameters acc permission

  let validate_statement ~parameters statement =
    let acc = ref [] in
    let r x = acc := validate_register parameters !acc x
    and o x = acc := validate_operand parameters !acc x in
    (match statement with
    | Word w ->
        acc := validate_word_terms parameters !acc w;
        ignore
          (map_word
             (fun e ->
               acc := expression_parameters parameters !acc e;
               e)
             w)
    | Op op -> (
        match op with
        | Jmp_term a -> r a
        | Jnz_term (a, b)
        | Load_term (a, b)
        | GetB_term (a, b)
        | GetE_term (a, b)
        | GetA_term (a, b)
        | GetP_term (a, b)
        | GetOType_term (a, b)
        | GetWType_term (a, b)
        | Invoke_term (a, b) ->
            r a;
            r b
        | Move_term (a, b) | Store_term (a, b) | Lea_term (a, b) | Restrict_term (a, b) ->
            r a;
            o b
        | Add_term (a, b, c)
        | Sub_term (a, b, c)
        | Mul_term (a, b, c)
        | Rem_term (a, b, c)
        | Div_term (a, b, c)
        | Lt_term (a, b, c)
        | SubSeg_term (a, b, c) ->
            r a;
            o b;
            o c
        | Seal_term (a, b, c) | UnSeal_term (a, b, c) ->
            r a;
            r b;
            r c
        | Fail_term | Halt_term -> ()));
    List.rev !acc

  let validate_raw_word ~parameters word = validate_statement ~parameters (Word word)
  let lookup arguments name = List.assoc_opt name arguments

  let sub_register arguments = function
    | Named _ as r -> Ok r
    | Register_parameter n -> (
        match lookup arguments n with
        | Some (Register_argument r) -> Ok (Named r)
        | _ -> Error [ Diagnostic.error (Printf.sprintf "No register argument for $%s." n) ])

  let sub_operand arguments = function
    | Register_term r -> Result.map (fun r -> Register_term r) (sub_register arguments r)
    | Constant_term (Value_parameter n) -> (
        match lookup arguments n with
        | Some (Register_argument r) -> Ok (Register_term (Named r))
        | Some (Constant_argument c) -> Ok (Constant_term c)
        | None -> Error [ Diagnostic.error (Printf.sprintf "No value argument for $%s." n) ])
    | o -> Ok o

  let sub_permission arguments = function
    | Permission_literal _ as permission -> Ok permission
    | Permission_parameter name -> (
        match lookup arguments name with
        | Some (Constant_argument (Permission permission)) -> Ok (Permission_literal permission)
        | _ -> Error [ Diagnostic.error (Printf.sprintf "No permission argument for $%s." name) ])

  let sub_seal_permission arguments = function
    | Seal_permission_literal _ as permission -> Ok permission
    | Seal_permission_parameter name -> (
        match lookup arguments name with
        | Some (Constant_argument (Seal_permission permission)) ->
            Ok (Seal_permission_literal permission)
        | _ ->
            Error [ Diagnostic.error (Printf.sprintf "No seal-permission argument for $%s." name) ])

  let sub_word arguments = function
    | I_term _ as word -> Ok word
    | Sealable_term (Cap_term (permission, b, e, a)) ->
        Result.map
          (fun permission -> Sealable_term (Cap_term (permission, b, e, a)))
          (sub_permission arguments permission)
    | Sealed_term (otype, Cap_term (permission, b, e, a)) ->
        Result.map
          (fun permission -> Sealed_term (otype, Cap_term (permission, b, e, a)))
          (sub_permission arguments permission)
    | Sealable_term (SealRange_term (permission, b, e, a)) ->
        Result.map
          (fun permission -> Sealable_term (SealRange_term (permission, b, e, a)))
          (sub_seal_permission arguments permission)
    | Sealed_term (otype, SealRange_term (permission, b, e, a)) ->
        Result.map
          (fun permission -> Sealed_term (otype, SealRange_term (permission, b, e, a)))
          (sub_seal_permission arguments permission)

  let bind1 c arguments a = Result.map c (sub_register arguments a)

  let bind2 c arguments a b =
    match (sub_register arguments a, sub_register arguments b) with
    | Ok a, Ok b -> Ok (c a b)
    | Error e, _ -> Error e
    | _, Error e -> Error e

  let bind3 c arguments a b d =
    match (sub_register arguments a, sub_register arguments b, sub_register arguments d) with
    | Ok a, Ok b, Ok d -> Ok (c a b d)
    | Error e, _, _ -> Error e
    | _, Error e, _ -> Error e
    | _, _, Error e -> Error e

  let bindro c arguments r o =
    match (sub_register arguments r, sub_operand arguments o) with
    | Ok r, Ok o -> Ok (c r o)
    | Error e, _ -> Error e
    | _, Error e -> Error e

  let bindroo c arguments r a b =
    match (sub_register arguments r, sub_operand arguments a, sub_operand arguments b) with
    | Ok r, Ok a, Ok b -> Ok (c r a b)
    | Error e, _, _ -> Error e
    | _, Error e, _ -> Error e
    | _, _, Error e -> Error e

  let substitute_instruction arguments = function
    | Jmp_term a -> bind1 (fun a -> Jmp_term a) arguments a
    | Jnz_term (a, b) -> bind2 (fun a b -> Jnz_term (a, b)) arguments a b
    | Load_term (a, b) -> bind2 (fun a b -> Load_term (a, b)) arguments a b
    | GetB_term (a, b) -> bind2 (fun a b -> GetB_term (a, b)) arguments a b
    | GetE_term (a, b) -> bind2 (fun a b -> GetE_term (a, b)) arguments a b
    | GetA_term (a, b) -> bind2 (fun a b -> GetA_term (a, b)) arguments a b
    | GetP_term (a, b) -> bind2 (fun a b -> GetP_term (a, b)) arguments a b
    | GetOType_term (a, b) -> bind2 (fun a b -> GetOType_term (a, b)) arguments a b
    | GetWType_term (a, b) -> bind2 (fun a b -> GetWType_term (a, b)) arguments a b
    | Invoke_term (a, b) -> bind2 (fun a b -> Invoke_term (a, b)) arguments a b
    | Move_term (r, o) -> bindro (fun r o -> Move_term (r, o)) arguments r o
    | Store_term (r, o) -> bindro (fun r o -> Store_term (r, o)) arguments r o
    | Lea_term (r, o) -> bindro (fun r o -> Lea_term (r, o)) arguments r o
    | Restrict_term (r, o) -> bindro (fun r o -> Restrict_term (r, o)) arguments r o
    | Add_term (r, a, b) -> bindroo (fun r a b -> Add_term (r, a, b)) arguments r a b
    | Sub_term (r, a, b) -> bindroo (fun r a b -> Sub_term (r, a, b)) arguments r a b
    | Mul_term (r, a, b) -> bindroo (fun r a b -> Mul_term (r, a, b)) arguments r a b
    | Rem_term (r, a, b) -> bindroo (fun r a b -> Rem_term (r, a, b)) arguments r a b
    | Div_term (r, a, b) -> bindroo (fun r a b -> Div_term (r, a, b)) arguments r a b
    | Lt_term (r, a, b) -> bindroo (fun r a b -> Lt_term (r, a, b)) arguments r a b
    | SubSeg_term (r, a, b) -> bindroo (fun r a b -> SubSeg_term (r, a, b)) arguments r a b
    | Seal_term (a, b, c) -> bind3 (fun a b c -> Seal_term (a, b, c)) arguments a b c
    | UnSeal_term (a, b, c) -> bind3 (fun a b c -> UnSeal_term (a, b, c)) arguments a b c
    | Fail_term -> Ok Fail_term
    | Halt_term -> Ok Halt_term

  let substitute_statement ~arguments = function
    | Op op -> Result.map (fun op -> Op op) (substitute_instruction arguments op)
    | Word w -> Result.map (fun word -> Word word) (sub_word arguments w)

  let substitute_raw_word ~arguments w = sub_word arguments w

  let substitute_argument ~arguments = function
    | Register_argument _ as a -> Ok a
    | Constant_argument (Value_parameter n) -> (
        match lookup arguments n with
        | Some a -> Ok a
        | None -> Error [ Diagnostic.error (Printf.sprintf "No argument for $%s." n) ])
    | a -> Ok a
end

let valid_parameter_kind name = Option.is_some (Syntax.parameter_kind name)
let parameter_kind name = Option.get (Syntax.parameter_kind name)

type source_program =
  (statement, word_term, macro_argument, parameter_kind) Assembly_construction.item list

module Assembler = Assembly_construction.Make (Syntax)

let assemble = Assembler.assemble

let diagnostic message = Error [ Diagnostic.error message ]
let ( let* ) = Result.bind

let eval config expression =
  match Assembly_construction.Expression.evaluate_runtime config expression with
  | Ok z -> Ok z
  | Error message -> diagnostic message

let lower_permission = function
  | Permission_literal permission -> Ok permission
  | Permission_parameter name ->
      diagnostic (Printf.sprintf "Unexpanded permission parameter $%s." name)

let lower_seal_permission = function
  | Seal_permission_literal permission -> Ok permission
  | Seal_permission_parameter name ->
      diagnostic (Printf.sprintf "Unexpanded seal-permission parameter $%s." name)

let lower_sealable config = function
  | Cap_term (permission, base, limit, cursor) ->
      let* permission = lower_permission permission in
      let* base = eval config base in
      let* limit = eval config limit in
      let* cursor = eval config cursor in
      Ok (Cap (permission, base, limit, cursor))
  | SealRange_term (permission, base, limit, cursor) ->
      let* permission = lower_seal_permission permission in
      let* base = eval config base in
      let* limit = eval config limit in
      let* cursor = eval config cursor in
      Ok (SealRange (permission, base, limit, cursor))

let lower_word config = function
  | I_term expression -> Result.map (fun value -> I value) (eval config expression)
  | Sealable_term sealable ->
      Result.map (fun value -> Sealable value) (lower_sealable config sealable)
  | Sealed_term (object_type, sealable) ->
      let* object_type = eval config object_type in
      Result.map (fun value -> Sealed (object_type, value)) (lower_sealable config sealable)

let lower_register = function
  | Named register -> Ok register
  | Register_parameter name ->
      diagnostic (Printf.sprintf "Unexpanded register parameter $%s." name)

let lower_constant config = function
  | Expression expression -> eval config expression
  | Permission permission -> Ok (Codec.encode_permission permission)
  | Seal_permission permission -> Ok (Codec.encode_seal_permission permission)
  | Word_type word_type -> Ok (Codec.encode_word_type word_type)
  | Value_parameter name -> diagnostic (Printf.sprintf "Unexpanded value parameter $%s." name)

let lower_operand config = function
  | Register_term register -> Result.map (fun value -> Register value) (lower_register register)
  | Constant_term constant -> Result.map (fun value -> Constant value) (lower_constant config constant)

let lower_instruction config instruction =
  let r = lower_register and o = lower_operand config in
  let rr constructor first second =
    let* first = r first in
    Result.map (fun second -> constructor (first, second)) (r second)
  in
  let ro constructor first second =
    let* first = r first in
    Result.map (fun second -> constructor (first, second)) (o second)
  in
  let roo constructor first second third =
    let* first = r first in
    let* second = o second in
    Result.map (fun third -> constructor (first, second, third)) (o third)
  in
  let rrr constructor first second third =
    let* first = r first in
    let* second = r second in
    Result.map (fun third -> constructor (first, second, third)) (r third)
  in
  match instruction with
  | Jmp_term value -> Result.map (fun value -> Jmp value) (r value)
  | Jnz_term (a, b) -> rr (fun (a, b) -> Jnz (a, b)) a b
  | Move_term (a, b) -> ro (fun (a, b) -> Move (a, b)) a b
  | Load_term (a, b) -> rr (fun (a, b) -> Load (a, b)) a b
  | Store_term (a, b) -> ro (fun (a, b) -> Store (a, b)) a b
  | Add_term (a, b, c) -> roo (fun (a, b, c) -> Add (a, b, c)) a b c
  | Sub_term (a, b, c) -> roo (fun (a, b, c) -> Sub (a, b, c)) a b c
  | Mul_term (a, b, c) -> roo (fun (a, b, c) -> Mul (a, b, c)) a b c
  | Rem_term (a, b, c) -> roo (fun (a, b, c) -> Rem (a, b, c)) a b c
  | Div_term (a, b, c) -> roo (fun (a, b, c) -> Div (a, b, c)) a b c
  | Lt_term (a, b, c) -> roo (fun (a, b, c) -> Lt (a, b, c)) a b c
  | Lea_term (a, b) -> ro (fun (a, b) -> Lea (a, b)) a b
  | Restrict_term (a, b) -> ro (fun (a, b) -> Restrict (a, b)) a b
  | SubSeg_term (a, b, c) -> roo (fun (a, b, c) -> SubSeg (a, b, c)) a b c
  | GetB_term (a, b) -> rr (fun (a, b) -> GetB (a, b)) a b
  | GetE_term (a, b) -> rr (fun (a, b) -> GetE (a, b)) a b
  | GetA_term (a, b) -> rr (fun (a, b) -> GetA (a, b)) a b
  | GetP_term (a, b) -> rr (fun (a, b) -> GetP (a, b)) a b
  | GetOType_term (a, b) -> rr (fun (a, b) -> GetOType (a, b)) a b
  | GetWType_term (a, b) -> rr (fun (a, b) -> GetWType (a, b)) a b
  | Seal_term (a, b, c) -> rrr (fun (a, b, c) -> Seal (a, b, c)) a b c
  | UnSeal_term (a, b, c) -> rrr (fun (a, b, c) -> UnSeal (a, b, c)) a b c
  | Invoke_term (a, b) -> rr (fun (a, b) -> Invoke (a, b)) a b
  | Fail_term -> Ok Fail
  | Halt_term -> Ok Halt

let lower_program config program =
  let rec loop words = function
    | [] -> Ok (List.rev words)
    | statement :: rest -> (
        match statement with
        | Op instruction ->
            let* instruction = lower_instruction config instruction in
            (match Codec.encode instruction with
            | Ok encoded -> loop (I encoded :: words) rest
            | Error error -> diagnostic (Instruction_codec.error_message error))
        | Word word ->
            let* word = lower_word config word in
            loop (word :: words) rest)
  in
  loop [] program

let lower_regfile config entries =
  List.fold_left
    (fun result (register, word) ->
      let* entries = result in
      Result.map (fun word -> (register, word) :: entries) (lower_word config word))
    (Ok []) entries
  |> Result.map List.rev
