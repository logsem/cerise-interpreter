%{
open Asm_ir

let reject (position : Lexing.position) (message : string) : 'a =
  raise (Assembly_construction.Parse_error (Assembly_construction.location position, message))

let register (position : Lexing.position) (name : string) : Ast.register =
  match parse_register_name name with
  | Some register -> register
  | None -> reject position "Expected a Griotte register."

let system_register (position : Lexing.position) (name : string) : Ast.system_register =
  if String.equal (String.lowercase_ascii name) "mtdc" then Ast.MTDC
  else reject position "Expected the Griotte system register MTDC."

let permission (position : Lexing.position) (rx : string) (write : string)
    (deep_local : string) (deep_read_only : string) : Ast.permission =
  match
    ( parse_rx (String.lowercase_ascii rx),
      parse_write (String.lowercase_ascii write),
      parse_dl (String.lowercase_ascii deep_local),
      parse_dro (String.lowercase_ascii deep_read_only) )
  with
  | Some rx, Some write, Some deep_local, Some deep_read_only ->
      (rx, write, deep_local, deep_read_only)
  | _ -> reject position "Invalid Griotte composite permission component."

let null_permission (position : Lexing.position) (name : string) : Ast.permission =
  if String.equal (String.uppercase_ascii name) "O" then Ast.null_permission
  else reject position "Expected O or a Griotte composite permission."

let locality (position : Lexing.position) (name : string) : Ast.locality =
  match parse_locality_name name with
  | Some locality -> locality
  | None -> reject position "Expected Local or Global."

let seal_permission (position : Lexing.position) (name : string) : Ast.seal_permission =
  match parse_seal_permission_name name with
  | Some permission -> permission
  | None -> reject position "Expected SO, S, U, or SU."

let word_type (position : Lexing.position) (name : string) : Ast.word_type =
  match parse_word_type_name name with
  | Some word_type -> word_type
  | None -> reject position "Expected Int, Cap, SealRange, Sealed, or Sentry."

let sentry_prefix (position : Lexing.position) (name : string) : unit =
  if not (String.equal (String.uppercase_ascii name) "E") then
    reject position "Expected E- before a Griotte sentry permission."
%}

%%

%public statement:
  | JALR; first = register_term; second = register_term { Op (Jalr_term (first, second)) }
  | JMP; target = operand { Op (Jmp_term target) }
  | JNZ; condition = register_term; target = operand { Op (Jnz_term (condition, target)) }
  | READSR; destination = register_term; source = machine_system_register
      { Op (ReadSR_term (destination, source)) }
  | WRITESR; destination = machine_system_register; source = register_term
      { Op (WriteSR_term (destination, source)) }
  | MOVE; destination = register_term; source = operand { Op (Move_term (destination, source)) }
  | LOAD; destination = register_term; source = register_term { Op (Load_term (destination, source)) }
  | STORE; destination = register_term; source = operand { Op (Store_term (destination, source)) }
  | ADD; destination = register_term; left = operand; right = operand
      { Op (Add_term (destination, left, right)) }
  | SUB; destination = register_term; left = operand; right = operand
      { Op (Sub_term (destination, left, right)) }
  | MUL; destination = register_term; left = operand; right = operand
      { Op (Mul_term (destination, left, right)) }
  | LAND; destination = register_term; left = operand; right = operand
      { Op (LAnd_term (destination, left, right)) }
  | LOR; destination = register_term; left = operand; right = operand
      { Op (LOr_term (destination, left, right)) }
  | LSHIFTL; destination = register_term; left = operand; right = operand
      { Op (LShiftL_term (destination, left, right)) }
  | LSHIFTR; destination = register_term; left = operand; right = operand
      { Op (LShiftR_term (destination, left, right)) }
  | LT; destination = register_term; left = operand; right = operand
      { Op (Lt_term (destination, left, right)) }
  | LEA; destination = register_term; source = operand { Op (Lea_term (destination, source)) }
  | RESTRICT; destination = register_term; source = operand
      { Op (Restrict_term (destination, source)) }
  | SUBSEG; destination = register_term; left = operand; right = operand
      { Op (SubSeg_term (destination, left, right)) }
  | GETL; destination = register_term; source = register_term { Op (GetL_term (destination, source)) }
  | GETB; destination = register_term; source = register_term { Op (GetB_term (destination, source)) }
  | GETE; destination = register_term; source = register_term { Op (GetE_term (destination, source)) }
  | GETA; destination = register_term; source = register_term { Op (GetA_term (destination, source)) }
  | GETP; destination = register_term; source = register_term { Op (GetP_term (destination, source)) }
  | GETOTYPE; destination = register_term; source = register_term
      { Op (GetOType_term (destination, source)) }
  | GETWTYPE; destination = register_term; source = register_term
      { Op (GetWType_term (destination, source)) }
  | SEAL; destination = register_term; source = register_term; sealing = register_term
      { Op (Seal_term (destination, source, sealing)) }
  | UNSEAL; destination = register_term; source = register_term; sealing = register_term
      { Op (UnSeal_term (destination, source, sealing)) }
  | FAIL { Op Fail_term }
  | HALT { Op Halt_term }
  | mnemonic = REM
      { reject $startpos(mnemonic) (Printf.sprintf "Unsupported Griotte instruction `%s`." (String.lowercase_ascii mnemonic)) }
  | mnemonic = DIV
      { reject $startpos(mnemonic) (Printf.sprintf "Unsupported Griotte instruction `%s`." (String.lowercase_ascii mnemonic)) }

register_term:
  | name = REGISTER { Named (register $startpos name) }
  | name = PARAMETER { Register_parameter name }

machine_register:
  | name = REGISTER { register $startpos name }

machine_system_register:
  | name = SYSTEM_REGISTER { system_register $startpos name }

permission_literal:
  | name = PERMISSION { null_permission $startpos name }
  | LBRACKET; rx = GRIOTTE_PERMISSION; write = GRIOTTE_PERMISSION; RBRACKET
      { permission $startpos(rx) rx write "lg" "lm" }
  | LBRACKET; rx = GRIOTTE_PERMISSION; write = GRIOTTE_PERMISSION;
      deep_local = GRIOTTE_PERMISSION; RBRACKET
      { permission $startpos(rx) rx write deep_local "lm" }
  | LBRACKET; rx = GRIOTTE_PERMISSION; write = GRIOTTE_PERMISSION;
      deep_local = GRIOTTE_PERMISSION; deep_read_only = GRIOTTE_PERMISSION; RBRACKET
      { permission $startpos(rx) rx write deep_local deep_read_only }

permission_term:
  | value = permission_literal { Permission_literal value }
  | name = PARAMETER { Permission_parameter name }

seal_permission_term:
  | name = SEAL_PERMISSION { Seal_permission_literal (seal_permission $startpos name) }
  | name = PARAMETER { Seal_permission_parameter name }

locality_term:
  | name = LOCALITY { Locality_literal (locality $startpos name) }
  | name = PARAMETER { Locality_parameter name }

constant:
  | value = permission_literal { Permission value }
  | name = SEAL_PERMISSION { Seal_permission (seal_permission $startpos name) }
  | name = LOCALITY { Locality (locality $startpos name) }
  | name = WORD_TYPE { Word_type (word_type $startpos name) }
  | LPAREN; permission = permission_literal; COMMA; locality = locality_term; RPAREN
      { Permission_locality (Permission_literal permission, locality) }
  | LPAREN; permission = PARAMETER; COMMA; locality = locality_term; RPAREN
      { Permission_locality (Permission_parameter permission, locality) }
  | LPAREN; permission = SEAL_PERMISSION; COMMA; locality = locality_term; RPAREN
      { Seal_permission_locality
          (Seal_permission_literal (seal_permission $startpos(permission) permission), locality) }

operand:
  | name = REGISTER { Register_term (Named (register $startpos name)) }
  | name = PARAMETER { Constant_term (Value_parameter name) }
  | value = constant { Constant_term value }
  | value = operand_expression %prec OPERAND_END { Constant_term (Expression value) }

sealable:
  | LPAREN; permission = permission_term; COMMA; locality = locality_term; COMMA;
      base = expression; COMMA; limit = expression; COMMA; cursor = expression; RPAREN
      { Cap_term (permission, locality, base, limit, cursor) }
  | LBRACKET; permission = seal_permission_term; COMMA; locality = locality_term; COMMA;
      base = expression; COMMA; limit = expression; COMMA; cursor = expression; RBRACKET
      { Seal_range_term (permission, locality, base, limit, cursor) }

%public raw_word:
  | value = expression { I_term value }
  | value = sealable { Sealable_term value }
  | LPAREN; prefix = PERMISSION; MINUS; permission = permission_term; COMMA;
      locality = locality_term; COMMA; base = expression; COMMA; limit = expression;
      COMMA; cursor = expression; RPAREN
      { sentry_prefix $startpos(prefix) prefix; Sentry_term (permission, locality, base, limit, cursor) }
  | LBRACE; object_type = expression; COLON; value = sealable; RBRACE
      { Sealed_term (object_type, value) }

%public regfile_entry:
  | register = machine_register; ASSIGN; value = raw_word
      { Register_entry (register, value) }
  | register = machine_system_register; ASSIGN; value = raw_word
      { System_register_entry (register, value) }

%public macro_argument:
  | name = REGISTER { Register_argument (register $startpos name) }
  | name = PARAMETER { Constant_argument (Value_parameter name) }
  | value = constant { Constant_argument value }
  | value = operand_expression { Constant_argument (Expression value) }
