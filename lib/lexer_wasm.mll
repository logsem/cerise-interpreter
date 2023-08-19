{
  open Parser_wasm
  exception Error of string
  let error lexbuf msg =
    let position = Lexing.lexeme_start_p lexbuf in
    let err_str = Printf.sprintf "Lexing error in file %s at position %d:%d\n"
                  position.pos_fname position.pos_lnum (position.pos_cnum - position.pos_bol + 1)
                  ^ msg ^ "\n" in
    raise (Error err_str)
}

let digit = ['0'-'9']
let hex = (digit | ['a'-'f'] | ['A'-'F'])
let letter = ['a'-'z' 'A'-'Z']

rule token = parse
| eof { EOF }
| [' ' '\t'] { token lexbuf }
| '\n' { Lexing.new_line lexbuf; token lexbuf }
| ";;" { comment lexbuf }
| ((digit+) | ("0x" hex+)) as i { try INT (int_of_string i)
                                  with Failure _ -> error lexbuf ("Invalid integer '" ^ i ^ "'.")}

(* single-character tokens *)
| '(' { LPAR }
| ')' { RPAR }

(* keywords *)
| "module" { MODULE }
| "type" { TYPE }
| "func" { FUNC }
| "param" { PARAM }
| "result" { RESULT }
| "handle" { HANDLE }
| "table" { TABLE }
| "global" { GLOBAL }
| "module" { MODULE }
| "offset" { OFFSET }
| "memory" { MEM }
| "mut" { MUT }
| "start" { START }
| "elem" { ELEM }
| "import" { IMPORT }
| "export" { EXPORT }
| "local" { LOCAL }
| "i32" { I32 }
| "i64" { I64 }

(* instructions *)
| "nop" { NOP }
| "unreachable" { UNREACHABLE }
| "drop" { DROP }
| "select" { SELECT }
| "block" { BLOCK }
| "loop" { LOOP }
| "if" { IF }
| "else" { ELSE }
| "end" { END }
| "br" { BR }
| "br_if" { BR_IF }
| "return" { RETURN }
| "call" { CALL }
| "call_indirect" { CALL_INDIRECT }
| "local.get" { LOCAL_GET }
| "local.set" { LOCAL_SET }
| "local.tee" { LOCAL_TEE }
| "global.get" { GLOBAL_GET }
| "global.set" { GLOBAL_SET }

| "i32.segload" { I32_SEGLOAD }
| "i32.segstore" { I32_SEGSTORE }
| "i64.segload" { I64_SEGLOAD }
| "i64.segstore" { I64_SEGSTORE }
| "handle.segload" { HANDLE_SEGLOAD }
| "handle.segstore" { HANDLE_SEGSTORE }

| "segalloc" { SEGALLOC }
| "segfree" { SEGFREE }
| "slice" { SLICE }
| "handleadd" { HANDLEADD }

| "i32.const" { I32_CONST }
| "i64.const" { I64_CONST }

| "i32.load" { I32_LOAD }
| "i32.store" { I32_STORE }
| "i64.load" { I64_LOAD }
| "i64.store" { I64_STORE }
| "memory.current" { CURRENT_MEMORY }
| "memory.grow" { GROW_MEMORY }

(* binop *)
| "i32.add" { I32_ADD }
| "i32.sub" { I32_SUB }
| "i32.mul" { I32_MUL }
| "i32.rem_s" { I32_REM_S }
| "i32.rem_u" { I32_REM_U }
| "i32.div_s" { I32_DIV_S }
| "i32.div_u" { I32_DIV_U }

| "i64.add" { I64_ADD }
| "i64.sub" { I64_SUB }
| "i64.mul" { I64_MUL }
| "i64.rem_s" { I64_REM_S }
| "i64.rem_u" { I64_REM_U }
| "i64.div_s" { I64_DIV_S }
| "i64.div_u" { I64_DIV_U }


(* testop *)
| "i32.eqz" { I32_EQZ }
| "i64.eqz" { I64_EQZ }

(* relop *)
| "i32.eq" { I32_EQ }
| "i32.ne" { I32_NE }

| "i32.lt_u" { I32_LT_U }
| "i32.lt_s" { I32_LT_S }
| "i32.gt_u" { I32_GT_U }
| "i32.gt_s" { I32_GT_S }
| "i32.le_u" { I32_LE_U }
| "i32.le_s" { I32_LE_S }
| "i32.ge_u" { I32_GE_U }
| "i32.ge_s" { I32_GE_S }

| "i64.eq" { I64_EQ }
| "i64.ne" { I64_NE }

| "i64.lt_u" { I64_LT_U }
| "i64.lt_s" { I64_LT_S }
| "i64.gt_u" { I64_GT_U }
| "i64.gt_s" { I64_GT_S }
| "i64.le_u" { I64_LE_U }
| "i64.le_s" { I64_LE_S }
| "i64.ge_u" { I64_GE_U }
| "i64.ge_s" { I64_GE_S }


| '$' ((( '_' | letter | '.')+) as name) { SYMB name }
| '"' ((letter+) as str) '"' { STR str }

and comment = parse
| eof { EOF }
| '\n' { Lexing.new_line lexbuf; token lexbuf }
| _ { comment lexbuf }
