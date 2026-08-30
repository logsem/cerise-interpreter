%{
open Asm_ir

let reject (position : Lexing.position) (message : string) : 'a =
  raise (Assembly_construction.Parse_error (Assembly_construction.location position, message))

let register (position : Lexing.position) (name : string) : Ast.register =
  match parse_register_name name with
  | Some register -> register
  | None -> reject position "Expected a Cerisier register."

let permission (position : Lexing.position) (name : string) : Ast.permission =
  match parse_permission name with
  | Some permission -> permission
  | None -> reject position "Expected a Cerisier permission."

let locality (position : Lexing.position) (name : string) : Ast.locality =
  match parse_locality name with
  | Some locality -> locality
  | None -> reject position "Expected GLOBAL, LOCAL, or DIRECTED."

let seal_permission (position : Lexing.position) (name : string) : Ast.seal_permission =
  match parse_seal_permission name with
  | Some permission -> permission
  | None -> reject position "Expected a sealing permission."

let word_type (position : Lexing.position) (name : string) : Ast.word_type =
  match parse_word_type name with
  | Some word_type -> word_type
  | None -> reject position "Expected a Cerisier word type."
%}

%%

%public statement:
  | JMP; register = register_term { Op (Jmp_term register) }
  | JNZ; first = register_term; second = register_term { Op (Jnz_term (first, second)) }
  | MOVE; destination = register_term; source = operand { Op (Move_term (destination, source)) }
  | LOAD; destination = register_term; source = register_term { Op (Load_term (destination, source)) }
  | STORE; destination = register_term; source = operand { Op (Store_term (destination, source)) }
  | ADD; destination = register_term; left = operand; right = operand
      { Op (Add_term (destination, left, right)) }
  | SUB; destination = register_term; left = operand; right = operand
      { Op (Sub_term (destination, left, right)) }
  | MUL; destination = register_term; left = operand; right = operand
      { Op (Mul_term (destination, left, right)) }
  | REM; destination = register_term; left = operand; right = operand
      { Op (Rem_term (destination, left, right)) }
  | DIV; destination = register_term; left = operand; right = operand
      { Op (Div_term (destination, left, right)) }
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
  | INVOKE; first = register_term; second = register_term { Op (Invoke_term (first, second)) }
  | LOADU; destination = register_term; source = register_term; offset = operand
      { Op (LoadU_term (destination, source, offset)) }
  | STOREU; destination = register_term; offset = operand; source = operand
      { Op (StoreU_term (destination, offset, source)) }
  | PROMOTEU; register = register_term { Op (PromoteU_term register) }
  | EINIT; destination = register_term; source = register_term
      { Op (EInit_term (destination, source)) }
  | EDEINIT; source = register_term { Op (EDeInit_term source) }
  | ESTOREID; destination = register_term; source = register_term
      { Op (EStoreId_term (destination, source)) }
  | ISUNIQUE; destination = register_term; source = register_term
      { Op (IsUnique_term (destination, source)) }
  | FAIL { Op Fail_term }
  | HALT { Op Halt_term }

register_term:
  | name = REGISTER { Named (register $startpos name) }
  | name = PARAMETER { Register_parameter name }

machine_register:
  | name = REGISTER { register $startpos name }

permission_term:
  | name = PERMISSION { Permission_literal (permission $startpos name) }
  | name = PARAMETER { Permission_parameter name }

seal_permission_term:
  | name = SEAL_PERMISSION { Seal_permission_literal (seal_permission $startpos name) }
  | name = PARAMETER { Seal_permission_parameter name }

locality_term:
  | name = LOCALITY { Locality (locality $startpos name) }
  | name = PARAMETER { Locality_parameter name }

constant:
  | name = PERMISSION { Permission (permission $startpos name) }
  | name = SEAL_PERMISSION { Seal_permission (seal_permission $startpos name) }
  | name = LOCALITY { Locality_constant (locality $startpos name) }
  | name = WORD_TYPE { Word_type (word_type $startpos name) }
  | LPAREN; name = PERMISSION; COMMA; locality = locality_term; RPAREN
      { Permission_locality (Permission_literal (permission $startpos(name) name), locality) }
  | LPAREN; name = SEAL_PERMISSION; COMMA; locality = locality_term; RPAREN
      { Seal_permission_locality (Seal_permission_literal (seal_permission $startpos(name) name), locality) }
  | LPAREN; name = PARAMETER; COMMA; locality = locality_term; RPAREN
      { Parameterized_permission_locality (name, locality) }

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
      { SealRange_term (permission, locality, base, limit, cursor) }

%public raw_word:
  | value = expression { I_term value }
  | value = sealable { Sealable_term value }
  | LBRACE; object_type = expression; COLON; value = sealable; RBRACE
      { Sealed_term (object_type, value) }

%public regfile_entry:
  | register = machine_register; ASSIGN; value = raw_word { (register, value) }

%public macro_argument:
  | name = REGISTER { Register_argument (register $startpos name) }
  | name = PARAMETER { Constant_argument (Value_parameter name) }
  | value = constant { Constant_argument value }
  | value = operand_expression { Constant_argument (Expression value) }
