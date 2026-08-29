open Mcerise_ast
open Assembly_frontend

let error tokens message =
  match tokens with
  | token :: _ -> Error [ Diagnostic.error ~location:(Token.location token) message ]
  | [] -> Error [ Diagnostic.error message ]

let punctuation expected = function
  | token :: rest when Token.kind token = Token.Punctuation expected -> Ok rest
  | tokens -> error tokens (Printf.sprintf "Expected `%c`." expected)

let parse_register_name name =
  match String.lowercase_ascii name with
  | "pc" -> Some PC | "ddc" | "r0" -> Some (Reg 0) | "stk" | "r31" -> Some (Reg 31)
  | name when String.length name > 1 && name.[0] = 'r' ->
      Option.bind (int_of_string_opt (String.sub name 1 (String.length name - 1)))
        (fun n -> if n >= 0 && n <= 31 then Some (Reg n) else None)
  | _ -> None

let parse_register = function
  | token :: rest -> (
      match Token.kind token with
      | Identifier name -> (
          match parse_register_name name with
          | Some register -> Ok (Named register, rest)
          | None -> error [ token ] "Expected a register.")
      | Parameter name -> Ok (Register_parameter name, rest)
      | _ -> error [ token ] "Expected a register.")
  | [] -> error [] "Expected a register."

let parse_permission = function
  | "O" -> Some O | "E" -> Some E | "RO" -> Some RO | "RX" -> Some RX
  | "RW" -> Some RW | "RWX" -> Some RWX | "RWL" -> Some RWL | "RWLX" -> Some RWLX
  | "URW" -> Some URW | "URWX" -> Some URWX | "URWL" -> Some URWL | "URWLX" -> Some URWLX
  | _ -> None

let parse_locality = function
  | "GLOBAL" | "Global" -> Some Global | "LOCAL" | "Local" -> Some Local
  | "DIRECTED" | "Directed" -> Some Directed | _ -> None

let scalar_constant token =
  match Token.kind token with
  | Identifier name -> (
      match parse_permission name with
      | Some p -> Some (Permission p)
      | None -> Option.map (fun l -> Locality_constant l) (parse_locality name))
  | _ -> None

let locality_term token =
  match Token.kind token with
  | Identifier name -> Option.map (fun l -> Locality l) (parse_locality name)
  | Parameter name -> Some (Locality_parameter name)
  | _ -> None

let parse_operand = function
  | open_t :: perm_t :: comma :: loc_t :: close :: rest
    when Token.kind open_t = Punctuation '(' && Token.kind comma = Punctuation ','
      && Token.kind close = Punctuation ')' -> (
      match Token.kind perm_t, locality_term loc_t with
      | Identifier p, Some l -> (
          match parse_permission p with
          | Some p -> Ok (Constant_term (Permission_locality (Permission_literal p, l)), rest)
          | None -> error [ perm_t ] "Expected a permission/locality pair.")
      | Parameter p, Some l ->
          Ok (Constant_term (Parameterized_permission_locality (p, l)), rest)
      | _ -> error [ perm_t ] "Expected a permission/locality pair.")
  | token :: rest as tokens -> (
      match Token.kind token with
      | Parameter name -> Ok (Constant_term (Value_parameter name), rest)
      | Identifier name -> (
          match parse_register_name name, scalar_constant token with
          | Some r, _ -> Ok (Register_term (Named r), rest)
          | None, Some c -> Ok (Constant_term c, rest)
          | None, None ->
              Result.map (fun (e, rest) -> Constant_term (Expression e), rest)
                (parse_expression tokens))
      | _ ->
          Result.map (fun (e, rest) -> Constant_term (Expression e), rest)
            (parse_expression tokens))
  | [] -> error [] "Expected a register or constant."

let unary c tokens = Result.map (fun (a,r) -> c a,r) (parse_register tokens)
let binary c tokens =
  match parse_register tokens with
  | Error _ as e -> e
  | Ok (a,tokens) -> Result.map (fun (b,r) -> c a b,r) (parse_register tokens)
let reg_operand c tokens =
  match parse_register tokens with
  | Error _ as e -> e
  | Ok (a,tokens) -> Result.map (fun (b,r) -> c a b,r) (parse_operand tokens)
let ternary c tokens =
  match parse_register tokens with
  | Error _ as e -> e
  | Ok (a,tokens) -> (
      match parse_operand tokens with
      | Error _ as e -> e
      | Ok (b,tokens) -> Result.map (fun (d,r) -> c a b d,r) (parse_operand tokens))
let reg_reg_operand c tokens =
  match parse_register tokens with
  | Error _ as e -> e
  | Ok (a,tokens) -> (
      match parse_register tokens with
      | Error _ as e -> e
      | Ok (b,tokens) -> Result.map (fun (d,r) -> c a b d,r) (parse_operand tokens))

let parse_instruction = function
  | token :: rest -> (
      match Token.kind token with
      | Identifier name ->
          let parsed =
            match String.lowercase_ascii name with
            | "jmp" -> unary (fun a -> Jmp_term a) rest
            | "jnz" -> binary (fun a b -> Jnz_term (a,b)) rest
            | "move" | "mov" -> reg_operand (fun a b -> Move_term (a,b)) rest
            | "load" -> binary (fun a b -> Load_term (a,b)) rest
            | "store" -> reg_operand (fun a b -> Store_term (a,b)) rest
            | "add" -> ternary (fun a b c -> Add_term (a,b,c)) rest
            | "sub" -> ternary (fun a b c -> Sub_term (a,b,c)) rest
            | "lt" -> ternary (fun a b c -> Lt_term (a,b,c)) rest
            | "lea" -> reg_operand (fun a b -> Lea_term (a,b)) rest
            | "restrict" -> reg_operand (fun a b -> Restrict_term (a,b)) rest
            | "subseg" -> ternary (fun a b c -> SubSeg_term (a,b,c)) rest
            | "isptr" -> binary (fun a b -> IsPtr_term (a,b)) rest
            | "getp" -> binary (fun a b -> GetP_term (a,b)) rest
            | "getl" -> binary (fun a b -> GetL_term (a,b)) rest
            | "getb" -> binary (fun a b -> GetB_term (a,b)) rest
            | "gete" -> binary (fun a b -> GetE_term (a,b)) rest
            | "geta" -> binary (fun a b -> GetA_term (a,b)) rest
            | "fail" -> Ok (Fail_term,rest)
            | "halt" -> Ok (Halt_term,rest)
            | "loadu" -> reg_reg_operand (fun a b c -> LoadU_term (a,b,c)) rest
            | "storeu" -> ternary (fun a b c -> StoreU_term (a,b,c)) rest
            | "promoteu" -> unary (fun a -> PromoteU_term a) rest
            | unsupported ->
                error [token] (Printf.sprintf "Unsupported mCerise instruction `%s`." unsupported)
          in
          Result.map_error (List.map (fun d ->
            match Diagnostic.location d with
            | Some _ -> d
            | None -> Diagnostic.make ~severity:(Diagnostic.severity d)
                ~location:(Token.location token) (Diagnostic.message d))) parsed
      | _ -> error [token] "Expected an instruction.")
  | [] -> error [] "Expected an instruction."

let parse_permission_term = function
  | token :: rest -> (
      match Token.kind token with
      | Identifier name -> (
          match parse_permission name with
          | Some p -> Ok (Permission_literal p, rest)
          | None -> error [token] "Expected a mCerise permission.")
      | Parameter name -> Ok (Permission_parameter name, rest)
      | _ -> error [token] "Expected a mCerise permission.")
  | [] -> error [] "Expected a mCerise permission."

let parse_locality_term = function
  | token :: rest -> (
      match locality_term token with
      | Some l -> Ok (l,rest)
      | None -> error [token] "Expected GLOBAL, LOCAL, or DIRECTED.")
  | [] -> error [] "Expected locality."

let parse_word = function
  | open_t :: rest when Token.kind open_t = Punctuation '(' -> (
      let ( let* ) = Result.bind in
      let* p,rest = parse_permission_term rest in
      let* rest = punctuation ',' rest in
      let* l,rest = parse_locality_term rest in
      let* rest = punctuation ',' rest in
      let* b,rest = parse_expression rest in
      let* rest = punctuation ',' rest in
      let* e,rest = parse_expression rest in
      let* rest = punctuation ',' rest in
      let* a,rest = parse_expression rest in
      Result.map (fun rest -> Cap_term (p,l,b,e,a),rest) (punctuation ')' rest))
  | tokens -> Result.map (fun (e,rest) -> I_term e,rest) (parse_expression tokens)

type parameter_kind = Register_kind | Expression_kind | Value_kind | Permission_kind | Locality_kind
type macro_argument = Register_argument of register | Constant_argument of constant_term

module Syntax = struct
  type nonrec statement = statement
  type raw_word = word_term
  type nonrec regfile = regfile
  type nonrec macro_argument = macro_argument
  type nonrec parameter_kind = parameter_kind
  let parse_statement ts = Result.map (fun (x,r) -> Op x,r) (parse_instruction ts)
  let parse_raw_word = parse_word
  let parse_regfile tokens =
    let rec loop acc = function
      | [] -> Ok (List.rev acc,[])
      | token :: rest -> (
          match Token.kind token, rest with
          | Identifier name, assign :: rest when Token.kind assign = Assign -> (
              match parse_register_name name with
              | None -> error [token] "Expected a register assignment."
              | Some r -> Result.bind (parse_word rest)
                  (fun (w,rest) -> loop ((r,w)::acc) rest))
          | _ -> error [token] "Expected a register assignment.")
    in loop [] tokens
  let parse_macro_argument tokens =
    match parse_operand tokens with
    | Ok (Register_term (Named r),rest) -> Ok (Register_argument r,rest)
    | Ok (Constant_term c,rest) -> Ok (Constant_argument c,rest)
    | Ok _ -> error tokens "A macro argument cannot contain a register parameter."
    | Error e -> Error e
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
    | Assembly_frontend.Expression.Parameter name -> (
        match List.assoc_opt name parameters with
        | Some (Expression_kind | Value_kind) -> acc
        | Some _ ->
            Diagnostic.error (Printf.sprintf "$%s is not an expression parameter." name) :: acc
        | None -> Diagnostic.error (Printf.sprintf "Unknown macro parameter $%s." name) :: acc)
    | Add (a,b) | Subtract (a,b) ->
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
  let sub_const args = function
    | Value_parameter n -> (
        match lookup args n with Some (Constant_argument c) -> Ok (Constant_term c)
        | Some (Register_argument r) -> Ok (Register_term (Named r))
        | None -> Error [Diagnostic.error (Printf.sprintf "No value argument for $%s." n)])
    | Permission_locality (p,l) ->
        Result.bind (sub_perm args p) (fun p ->
          Result.map (fun l -> Constant_term (Permission_locality (p,l))) (sub_loc args l))
    | Parameterized_permission_locality (n,l) -> (
        match lookup args n with
        | Some (Constant_argument (Permission p)) ->
            Result.map (fun l -> Constant_term (Permission_locality (Permission_literal p,l)))
              (sub_loc args l)
        | _ -> Error [Diagnostic.error (Printf.sprintf "No permission argument for $%s." n)])
    | c -> Ok (Constant_term c)
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
    | a -> Ok a
end

module Frontend = Assembly_frontend.Make(Syntax)
include Frontend
