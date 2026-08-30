open Ast

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
type word_term = I_term of expression | Cap_term of permission_term * locality_term * expression * expression * expression
type word = word_term

type instruction_term =
  | Jmp_term of register_term | Jnz_term of register_term * register_term
  | Move_term of register_term * operand_term | Load_term of register_term * register_term
  | Store_term of register_term * operand_term
  | Add_term of register_term * operand_term * operand_term
  | Sub_term of register_term * operand_term * operand_term
  | Lt_term of register_term * operand_term * operand_term | Lea_term of register_term * operand_term
  | Restrict_term of register_term * operand_term
  | SubSeg_term of register_term * operand_term * operand_term
  | IsPtr_term of register_term * register_term | GetP_term of register_term * register_term
  | GetL_term of register_term * register_term | GetB_term of register_term * register_term
  | GetE_term of register_term * register_term | GetA_term of register_term * register_term
  | Fail_term | Halt_term
  | LoadU_term of register_term * register_term * operand_term
  | StoreU_term of register_term * operand_term * operand_term
  | PromoteU_term of register_term

type statement = Op of instruction_term | Word of word_term
type program = statement list
type regfile = (register * word_term) list

let parse_register_name name =
  match String.lowercase_ascii name with
  | "pc" -> Some PC
  | "ddc" | "r0" -> Some (Reg 0)
  | "stk" | "r31" -> Some (Reg 31)
  | name when String.length name > 1 && name.[0] = 'r' -> (
      match int_of_string_opt (String.sub name 1 (String.length name - 1)) with
      | Some n when n >= 0 && n <= 31 -> Some (Reg n)
      | _ -> None)
  | _ -> None

let parse_permission = function
  | "O" -> Some O | "E" -> Some E | "RO" -> Some RO | "RX" -> Some RX
  | "RW" -> Some RW | "RWX" -> Some RWX | "RWL" -> Some RWL | "RWLX" -> Some RWLX
  | "URW" -> Some URW | "URWX" -> Some URWX | "URWL" -> Some URWL
  | "URWLX" -> Some URWLX | _ -> None

let parse_locality = function
  | "GLOBAL" | "Global" -> Some Global
  | "LOCAL" | "Local" -> Some Local
  | "DIRECTED" | "Directed" -> Some Directed
  | _ -> None

type parameter_kind = Register_kind | Expression_kind | Value_kind | Permission_kind | Locality_kind
type macro_argument = Register_argument of register | Constant_argument of constant_term

module Syntax = struct
  type nonrec statement = statement
  type raw_word = word_term
  type nonrec macro_argument = macro_argument
  type nonrec parameter_kind = parameter_kind
  let statement_of_raw_word w = Word w
  let parameter_kind = function
    | "reg" -> Some Register_kind | "expr" -> Some Expression_kind
    | "value" -> Some Value_kind | "perm" -> Some Permission_kind
    | "locality" -> Some Locality_kind | _ -> None
  let parameter_kind_name = function
    | Register_kind -> "reg" | Expression_kind -> "expr" | Value_kind -> "value"
    | Permission_kind -> "perm" | Locality_kind -> "locality"
  let argument_kind = function
    | Register_argument _ -> Register_kind
    | Constant_argument (Expression _) -> Expression_kind
    | Constant_argument (Permission _) -> Permission_kind
    | Constant_argument (Locality_constant _) -> Locality_kind
    | Constant_argument _ -> Value_kind
  let accepts_argument k a = k = Value_kind || k = argument_kind a
  let expression_of_argument = function Constant_argument (Expression e) -> Some e | _ -> None
  let map_constant f = function Expression e -> Expression (f e) | c -> c
  let map_operand f = function
    | Register_term r -> Register_term r | Constant_term c -> Constant_term (map_constant f c)
  let map_instruction f = function
    | Move_term (r,o) -> Move_term (r,map_operand f o)
    | Store_term (r,o) -> Store_term (r,map_operand f o)
    | Add_term (r,a,b) -> Add_term (r,map_operand f a,map_operand f b)
    | Sub_term (r,a,b) -> Sub_term (r,map_operand f a,map_operand f b)
    | Lt_term (r,a,b) -> Lt_term (r,map_operand f a,map_operand f b)
    | Lea_term (r,o) -> Lea_term (r,map_operand f o)
    | Restrict_term (r,o) -> Restrict_term (r,map_operand f o)
    | SubSeg_term (r,a,b) -> SubSeg_term (r,map_operand f a,map_operand f b)
    | LoadU_term (r,s,o) -> LoadU_term (r,s,map_operand f o)
    | StoreU_term (r,a,b) -> StoreU_term (r,map_operand f a,map_operand f b)
    | op -> op
  let map_word f = function
    | I_term e -> I_term (f e)
    | Cap_term (p,l,b,e,a) -> Cap_term (p,l,f b,f e,f a)
  let map_statement_expressions f = function
    | Op op -> Op (map_instruction f op) | Word w -> Word (map_word f w)
  let map_raw_word_expressions = map_word
  let map_regfile_expressions f = List.map (fun (r,w) -> r,map_word f w)
  let map_argument_expressions f = function
    | Constant_argument c -> Constant_argument (map_constant f c) | a -> a

  let rec expression_parameters parameters acc = function
    | Assembly_construction.Expression.Parameter name -> (
        match List.assoc_opt name parameters with
        | Some (Expression_kind | Value_kind) -> acc
        | Some _ ->
            Diagnostic.error (Printf.sprintf "$%s is not an expression parameter." name) :: acc
        | None -> Diagnostic.error (Printf.sprintf "Unknown macro parameter $%s." name) :: acc)
    | Add (a,b) | Subtract (a,b) | Multiply (a,b) | Logand (a,b) | Logor (a,b)
    | Shift_left (a,b) | Shift_right (a,b) ->
        expression_parameters parameters (expression_parameters parameters acc a) b
    | _ -> acc
  let validate_register parameters acc = function
    | Named _ -> acc
    | Register_parameter name -> (
        match List.assoc_opt name parameters with
        | Some (Register_kind | Value_kind) -> acc
        | _ -> Diagnostic.error (Printf.sprintf "Invalid register parameter $%s." name) :: acc)
  let validate_locality parameters acc = function
    | Locality _ -> acc
    | Locality_parameter name -> (
        match List.assoc_opt name parameters with
        | Some Locality_kind -> acc
        | _ -> Diagnostic.error (Printf.sprintf "Invalid locality parameter $%s." name) :: acc)
  let validate_permission parameters acc = function
    | Permission_literal _ -> acc
    | Permission_parameter name -> (
        match List.assoc_opt name parameters with
        | Some Permission_kind -> acc
        | _ -> Diagnostic.error (Printf.sprintf "Invalid permission parameter $%s." name) :: acc)
  let validate_constant parameters acc = function
    | Expression e -> expression_parameters parameters acc e
    | Value_parameter name -> (
        match List.assoc_opt name parameters with
        | Some (Value_kind | Expression_kind | Permission_kind | Locality_kind) -> acc
        | _ -> Diagnostic.error (Printf.sprintf "Invalid value parameter $%s." name) :: acc)
    | Permission_locality (permission,locality) ->
        validate_locality parameters
          (validate_permission parameters acc permission) locality
    | Parameterized_permission_locality (name,locality) ->
        let acc =
          match List.assoc_opt name parameters with
          | Some Permission_kind -> acc
          | _ -> Diagnostic.error (Printf.sprintf "Invalid permission parameter $%s." name) :: acc
        in
        validate_locality parameters acc locality
    | Permission _ | Locality_constant _ -> acc
  let validate_operand parameters acc = function
    | Register_term r -> validate_register parameters acc r
    | Constant_term c -> validate_constant parameters acc c
  let validate_word parameters acc = function
    | I_term e -> expression_parameters parameters acc e
    | Cap_term (permission,locality,b,e,a) ->
        let acc = validate_permission parameters acc permission in
        let acc = validate_locality parameters acc locality in
        let acc = expression_parameters parameters acc b in
        let acc = expression_parameters parameters acc e in
        expression_parameters parameters acc a
  let validate_statement ~parameters statement =
    let acc = ref [] in
    let r x = acc := validate_register parameters !acc x
    and o x = acc := validate_operand parameters !acc x in
    (match statement with
    | Word word -> acc := validate_word parameters !acc word
    | Op op -> (
        match op with
        | Jmp_term a | PromoteU_term a -> r a
        | Jnz_term (a,b) | Load_term (a,b) | IsPtr_term (a,b)
        | GetP_term (a,b) | GetL_term (a,b) | GetB_term (a,b)
        | GetE_term (a,b) | GetA_term (a,b) -> r a; r b
        | Move_term (a,b) | Store_term (a,b) | Lea_term (a,b)
        | Restrict_term (a,b) -> r a; o b
        | Add_term (a,b,c) | Sub_term (a,b,c) | Lt_term (a,b,c)
        | SubSeg_term (a,b,c) | StoreU_term (a,b,c) -> r a; o b; o c
        | LoadU_term (a,b,c) -> r a; r b; o c
        | Fail_term | Halt_term -> ()));
    List.rev !acc
  let validate_raw_word ~parameters word = validate_word parameters [] word |> List.rev
  let lookup args n = List.assoc_opt n args
  let sub_reg args = function
    | Named _ as r -> Ok r
    | Register_parameter n -> (
        match lookup args n with Some (Register_argument r) -> Ok (Named r)
        | _ -> Error [Diagnostic.error (Printf.sprintf "No register argument for $%s." n)])
  let sub_loc args = function
    | Locality _ as l -> Ok l
    | Locality_parameter n -> (
        match lookup args n with Some (Constant_argument (Locality_constant l)) -> Ok (Locality l)
        | _ -> Error [Diagnostic.error (Printf.sprintf "No locality argument for $%s." n)])
  let sub_perm args = function
    | Permission_literal _ as p -> Ok p
    | Permission_parameter n -> (
        match lookup args n with Some (Constant_argument (Permission p)) -> Ok (Permission_literal p)
        | _ -> Error [Diagnostic.error (Printf.sprintf "No permission argument for $%s." n)])
  let sub_constant args = function
    | Permission_locality (p,l) ->
        Result.bind (sub_perm args p) (fun p ->
          Result.map (fun l -> Permission_locality (p,l)) (sub_loc args l))
    | Parameterized_permission_locality (n,l) -> (
        match lookup args n with
        | Some (Constant_argument (Permission p)) ->
            Result.map (fun l -> Permission_locality (Permission_literal p,l))
              (sub_loc args l)
        | _ -> Error [Diagnostic.error (Printf.sprintf "No permission argument for $%s." n)])
    | c -> Ok c
  let sub_const args = function
    | Value_parameter n -> (
        match lookup args n with Some (Constant_argument c) -> Ok (Constant_term c)
        | Some (Register_argument r) -> Ok (Register_term (Named r))
        | None -> Error [Diagnostic.error (Printf.sprintf "No value argument for $%s." n)])
    | c -> Result.map (fun c -> Constant_term c) (sub_constant args c)
  let sub_operand args = function
    | Register_term r -> Result.map (fun r -> Register_term r) (sub_reg args r)
    | Constant_term c -> sub_const args c
  let b1 c args a = Result.map c (sub_reg args a)
  let b2 c args a b = Result.bind (sub_reg args a) (fun a -> Result.map (c a) (sub_reg args b))
  let bro c args r o = Result.bind (sub_reg args r) (fun r -> Result.map (c r) (sub_operand args o))
  let broo c args r a b = Result.bind (sub_reg args r) (fun r ->
    Result.bind (sub_operand args a) (fun a -> Result.map (c r a) (sub_operand args b)))
  let brro c args r s o = Result.bind (sub_reg args r) (fun r ->
    Result.bind (sub_reg args s) (fun s -> Result.map (c r s) (sub_operand args o)))
  let substitute_instruction args = function
    | Jmp_term a -> b1 (fun a -> Jmp_term a) args a
    | Jnz_term (a,b) -> b2 (fun a b -> Jnz_term (a,b)) args a b
    | Move_term (r,o) -> bro (fun r o -> Move_term (r,o)) args r o
    | Load_term (a,b) -> b2 (fun a b -> Load_term (a,b)) args a b
    | Store_term (r,o) -> bro (fun r o -> Store_term (r,o)) args r o
    | Add_term (r,a,b) -> broo (fun r a b -> Add_term (r,a,b)) args r a b
    | Sub_term (r,a,b) -> broo (fun r a b -> Sub_term (r,a,b)) args r a b
    | Lt_term (r,a,b) -> broo (fun r a b -> Lt_term (r,a,b)) args r a b
    | Lea_term (r,o) -> bro (fun r o -> Lea_term (r,o)) args r o
    | Restrict_term (r,o) -> bro (fun r o -> Restrict_term (r,o)) args r o
    | SubSeg_term (r,a,b) -> broo (fun r a b -> SubSeg_term (r,a,b)) args r a b
    | IsPtr_term (a,b) -> b2 (fun a b -> IsPtr_term (a,b)) args a b
    | GetP_term (a,b) -> b2 (fun a b -> GetP_term (a,b)) args a b
    | GetL_term (a,b) -> b2 (fun a b -> GetL_term (a,b)) args a b
    | GetB_term (a,b) -> b2 (fun a b -> GetB_term (a,b)) args a b
    | GetE_term (a,b) -> b2 (fun a b -> GetE_term (a,b)) args a b
    | GetA_term (a,b) -> b2 (fun a b -> GetA_term (a,b)) args a b
    | LoadU_term (r,s,o) -> brro (fun r s o -> LoadU_term (r,s,o)) args r s o
    | StoreU_term (r,a,b) -> broo (fun r a b -> StoreU_term (r,a,b)) args r a b
    | PromoteU_term r -> b1 (fun r -> PromoteU_term r) args r
    | Fail_term -> Ok Fail_term | Halt_term -> Ok Halt_term
  let sub_word args = function
    | I_term _ as w -> Ok w
    | Cap_term (p,l,b,e,a) -> Result.bind (sub_perm args p) (fun p ->
        Result.map (fun l -> Cap_term (p,l,b,e,a)) (sub_loc args l))
  let substitute_statement ~arguments = function
    | Op op -> Result.map (fun op -> Op op) (substitute_instruction arguments op)
    | Word w -> Result.map (fun w -> Word w) (sub_word arguments w)
  let substitute_raw_word ~arguments w = sub_word arguments w
  let substitute_argument ~arguments = function
    | Register_argument _ as a -> Ok a
    | Constant_argument (Value_parameter n) -> (
        match lookup arguments n with Some a -> Ok a
        | None -> Error [Diagnostic.error (Printf.sprintf "No argument for $%s." n)])
    | Constant_argument c ->
        Result.map (fun c -> Constant_argument c) (sub_constant arguments c)
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
  | Ok value -> Ok value
  | Error message -> diagnostic message

let lower_permission = function
  | Permission_literal permission -> Ok permission
  | Permission_parameter name ->
      diagnostic (Printf.sprintf "Unexpanded permission parameter $%s." name)

let lower_locality = function
  | Locality locality -> Ok locality
  | Locality_parameter name ->
      diagnostic (Printf.sprintf "Unexpanded locality parameter $%s." name)

let lower_word config = function
  | I_term expression -> Result.map (fun value -> I value) (eval config expression)
  | Cap_term (permission, locality, base, limit, cursor) ->
      let* permission = lower_permission permission in
      let* locality = lower_locality locality in
      let* base = eval config base in
      let* limit = eval config limit in
      Result.map
        (fun cursor -> Cap (Cap (permission, locality, base, limit, cursor)))
        (eval config cursor)

let lower_register = function
  | Named register -> Ok register
  | Register_parameter name ->
      diagnostic (Printf.sprintf "Unexpanded register parameter $%s." name)

let lower_constant config = function
  | Expression expression -> eval config expression
  | Permission permission -> Ok (Codec.encode_permission permission)
  | Permission_locality (permission, locality) ->
      let* permission = lower_permission permission in
      Result.map (Codec.encode_permission_locality permission) (lower_locality locality)
  | Parameterized_permission_locality (name, _) ->
      diagnostic (Printf.sprintf "Unexpanded permission parameter $%s." name)
  | Locality_constant locality -> Ok (Codec.encode_locality locality)
  | Value_parameter name -> diagnostic (Printf.sprintf "Unexpanded value parameter $%s." name)

let lower_operand config = function
  | Register_term register ->
      Result.map (fun value -> Register value) (lower_register register)
  | Constant_term constant ->
      Result.map (fun value -> Constant value) (lower_constant config constant)

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
  let rro constructor first second third =
    let* first = r first in
    let* second = r second in
    Result.map (fun third -> constructor (first, second, third)) (o third)
  in
  match instruction with
  | Jmp_term value -> Result.map (fun value -> Jmp value) (r value)
  | Jnz_term (a, b) -> rr (fun (a, b) -> Jnz (a, b)) a b
  | Move_term (a, b) -> ro (fun (a, b) -> Move (a, b)) a b
  | Load_term (a, b) -> rr (fun (a, b) -> Load (a, b)) a b
  | Store_term (a, b) -> ro (fun (a, b) -> Store (a, b)) a b
  | Add_term (a, b, c) -> roo (fun (a, b, c) -> Add (a, b, c)) a b c
  | Sub_term (a, b, c) -> roo (fun (a, b, c) -> Sub (a, b, c)) a b c
  | Lt_term (a, b, c) -> roo (fun (a, b, c) -> Lt (a, b, c)) a b c
  | Lea_term (a, b) -> ro (fun (a, b) -> Lea (a, b)) a b
  | Restrict_term (a, b) -> ro (fun (a, b) -> Restrict (a, b)) a b
  | SubSeg_term (a, b, c) -> roo (fun (a, b, c) -> SubSeg (a, b, c)) a b c
  | IsPtr_term (a, b) -> rr (fun (a, b) -> IsPtr (a, b)) a b
  | GetP_term (a, b) -> rr (fun (a, b) -> GetP (a, b)) a b
  | GetL_term (a, b) -> rr (fun (a, b) -> GetL (a, b)) a b
  | GetB_term (a, b) -> rr (fun (a, b) -> GetB (a, b)) a b
  | GetE_term (a, b) -> rr (fun (a, b) -> GetE (a, b)) a b
  | GetA_term (a, b) -> rr (fun (a, b) -> GetA (a, b)) a b
  | Fail_term -> Ok Fail
  | Halt_term -> Ok Halt
  | LoadU_term (a, b, c) -> rro (fun (a, b, c) -> LoadU (a, b, c)) a b c
  | StoreU_term (a, b, c) -> roo (fun (a, b, c) -> StoreU (a, b, c)) a b c
  | PromoteU_term value -> Result.map (fun value -> PromoteU value) (r value)

let lower_program config program =
  let rec loop words = function
    | [] -> Ok (List.rev words)
    | statement :: rest -> (
        match statement with
        | Word term ->
            let* word = lower_word config term in
            loop (word :: words) rest
        | Op term ->
            let* instruction = lower_instruction config term in
            (match Codec.encode instruction with
            | Ok encoded -> loop (I encoded :: words) rest
            | Error error -> diagnostic (Instruction_codec.error_message error)))
  in
  loop [] program

let lower_regfile config entries =
  List.fold_left
    (fun result (register, term) ->
      let* entries = result in
      Result.map (fun word -> (register, word) :: entries) (lower_word config term))
    (Ok []) entries
  |> Result.map List.rev
