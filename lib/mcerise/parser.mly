%{
open Asm_ir

let reject position message =
  raise (Assembly_construction.Parse_error (Assembly_construction.location position, message))

let register position name =
  match parse_register_name name with
  | Some register -> register
  | None -> reject position "Expected a mCerise register."

let permission position name =
  match parse_permission name with
  | Some permission -> permission
  | None -> reject position "Expected a mCerise permission."

let locality position name =
  match parse_locality name with
  | Some locality -> locality
  | None -> reject position "Expected GLOBAL, LOCAL, or DIRECTED."
%}

%%

%public statement:
  | JMP; r = register_term { Op (Jmp_term r) }
  | JNZ; a = register_term; b = register_term { Op (Jnz_term (a, b)) }
  | MOVE; r = register_term; o = operand { Op (Move_term (r, o)) }
  | LOAD; a = register_term; b = register_term { Op (Load_term (a, b)) }
  | STORE; r = register_term; o = operand { Op (Store_term (r, o)) }
  | ADD; r = register_term; a = operand; b = operand { Op (Add_term (r, a, b)) }
  | SUB; r = register_term; a = operand; b = operand { Op (Sub_term (r, a, b)) }
  | LT; r = register_term; a = operand; b = operand { Op (Lt_term (r, a, b)) }
  | LEA; r = register_term; o = operand { Op (Lea_term (r, o)) }
  | RESTRICT; r = register_term; o = operand { Op (Restrict_term (r, o)) }
  | SUBSEG; r = register_term; a = operand; b = operand { Op (SubSeg_term (r, a, b)) }
  | ISPTR; a = register_term; b = register_term { Op (IsPtr_term (a, b)) }
  | GETP; a = register_term; b = register_term { Op (GetP_term (a, b)) }
  | GETL; a = register_term; b = register_term { Op (GetL_term (a, b)) }
  | GETB; a = register_term; b = register_term { Op (GetB_term (a, b)) }
  | GETE; a = register_term; b = register_term { Op (GetE_term (a, b)) }
  | GETA; a = register_term; b = register_term { Op (GetA_term (a, b)) }
  | FAIL { Op Fail_term }
  | HALT { Op Halt_term }
  | LOADU; r = register_term; s = register_term; o = operand { Op (LoadU_term (r, s, o)) }
  | STOREU; r = register_term; a = operand; b = operand { Op (StoreU_term (r, a, b)) }
  | PROMOTEU; r = register_term { Op (PromoteU_term r) }

register_term:
  | name = REGISTER { Named (register $startpos name) }
  | name = PARAMETER { Register_parameter name }

machine_register:
  | name = REGISTER { register $startpos name }

permission_term:
  | name = PERMISSION { Permission_literal (permission $startpos name) }
  | name = PARAMETER { Permission_parameter name }

locality_term:
  | name = LOCALITY { Locality (locality $startpos name) }
  | name = PARAMETER { Locality_parameter name }

constant:
  | name = PERMISSION { Permission (permission $startpos name) }
  | name = LOCALITY { Locality_constant (locality $startpos name) }
  | LPAREN; name = PERMISSION; COMMA; locality = locality_term; RPAREN
      { Permission_locality (Permission_literal (permission $startpos(name) name), locality) }
  | LPAREN; name = PARAMETER; COMMA; locality = locality_term; RPAREN
      { Parameterized_permission_locality (name, locality) }

operand:
  | name = REGISTER { Register_term (Named (register $startpos name)) }
  | name = PARAMETER { Constant_term (Value_parameter name) }
  | value = constant { Constant_term value }
  | value = operand_expression %prec OPERAND_END { Constant_term (Expression value) }

%public raw_word:
  | value = expression { I_term value }
  | LPAREN; permission = permission_term; COMMA; locality = locality_term; COMMA;
      base = expression; COMMA; limit = expression; COMMA; cursor = expression; RPAREN
      { Cap_term (permission, locality, base, limit, cursor) }

%public regfile_entry:
  | register = machine_register; ASSIGN; value = raw_word { (register, value) }

%public macro_argument:
  | name = REGISTER { Register_argument (register $startpos name) }
  | name = PARAMETER { Constant_argument (Value_parameter name) }
  | value = constant { Constant_argument value }
  | value = operand_expression { Constant_argument (Expression value) }
