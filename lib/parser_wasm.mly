%token EOF
%token LPAR RPAR DOLLAR
%token TYPE FUNC PARAM RESULT I32 I64 HANDLE
%token TABLE GLOBAL MODULE OFFSET MEM MUT START ELEM IMPORT EXPORT LOCAL
%token NOP UNREACHABLE DROP SELECT
%token BLOCK LOOP IF BR BR_IF RETURN CALL CALL_INDIRECT ELSE END
%token LOCAL_GET LOCAL_SET LOCAL_TEE GLOBAL_GET GLOBAL_SET
%token I32_SEGLOAD I32_SEGSTORE I64_SEGLOAD I64_SEGSTORE HANDLE_SEGLOAD HANDLE_SEGSTORE
%token SEGALLOC SEGFREE SLICE HANDLEADD
%token I32_CONST I64_CONST
%token I32_LOAD I32_STORE I64_LOAD I64_STORE
%token CURRENT_MEMORY GROW_MEMORY
%token I32_ADD I32_SUB I32_MUL I32_REM_S I32_REM_U I32_DIV_S I32_DIV_U
%token I32_EQZ I32_EQ I32_NE
%token I32_LT_U I32_LT_S I32_GT_U I32_GT_S I32_LE_U I32_LE_S I32_GE_U I32_GE_S
%token I64_ADD I64_SUB I64_MUL I64_REM_S I64_REM_U I64_DIV_S I64_DIV_U
%token I64_EQZ I64_EQ I64_NE
%token I64_LT_U I64_LT_S I64_GT_U I64_GT_S I64_LE_U I64_LE_S I64_GE_U I64_GE_S
%token <int> INT
%token <string> STR

%start <ws_module> main
%{ open Ir_wasm %}

%%

main: | m = modul ; EOF { m }

modul:
  | LPAR ; MODULE ; i = id ; m = mod_fields ; RPAR { m }

mod_fields:
  | m1 = mod_field ; m2 = mod_fields { compose_ws_modules m1 m2 }
  | { null_module }

mod_field:
  | mtype = mod_type { mono_module_type mtype }
  | mfunc = mod_func { mono_module_func mfunc }
  | mmem = mod_mem { mono_module_mem mmem }
  | mtable = mod_table { mono_module_table mtable }
  | mglobal = mod_global { mono_module_global mglobal }
  | melem = mod_elem { mono_module_elem melem }
  | mstart = mod_start { mono_module_start mstart }
  | mimport = mod_import { mono_module_import mimport }
  | mexport = mod_export { mono_module_export mexport }

id: | DOLLAR ; i = STR { Some i } | { None }
name: s = STR { s }

limit:
    | n = INT { {lim_min = (Big_int_Z.big_int_of_int n) ;
                 lim_max = None} }
    | n = INT ; m = INT { {lim_min = (Big_int_Z.big_int_of_int n) ;
                           lim_max = Some (Big_int_Z.big_int_of_int m)} }


/* Types */
mod_type:
    | LPAR ; TYPE ; i = id ; t = fun_type ; RPAR { t }

fun_type:
    | LPAR ; FUNC ; p = param_result_list ; RPAR { let (p,r) = p in Tf (p,r) }

param_result_list:
  | LPAR ; PARAM ; i = id ; t = val_type ; RPAR ; ps = param_result_list
    { let (p,r) = ps in (t::p, r) }
  | ps = result_list { ([], ps) }
result_list:
  | LPAR ; RESULT ; t = val_type ; RPAR ; ps = result_list { t::ps }
  | { [] }

val_type:
  | I32 { T_int }
  | I64 { T_int }
  | HANDLE { T_handle }

/* Linear memory */
mod_mem:
  | LPAR ; MEM ; i = id ; mt = mem_type ; RPAR { mt }

mem_type:
  | l = limit { l }

/* Indirect table */
mod_table:
  | LPAR ; TABLE ; i = id ; tt = table_type ; RPAR { tt }

table_type:
  | l = limit { l }

/* Globals  */
mod_global:
  | LPAR ; GLOBAL ; i = id ; gt = global_type ; gv = value ; RPAR
    { { modglob_type = gt ; modglob_init = gv } }

global_type:
  | vt = val_type { { tg_mut = MUT_immut ; tg_t = vt } }
  | MUT ; vt = val_type { { tg_mut = MUT_mut ; tg_t = vt } }

value:
  | n = INT { Val_int (Big_int_Z.big_int_of_int n) }

/* Elem  */
mod_elem:
  | LPAR ; ELEM ; tidx = table_idx ; LPAR ; OFFSET ; i = INT ; RPAR ; fidxs = list_fun_idx ; RPAR
    { { modelem_table = tidx ;
        modelem_offset = (Big_int_Z.big_int_of_int i) ;
        modelem_init = fidxs }
    }

/* Start  */
mod_start:
    LPAR ; START ; fidx = fun_idx ; RPAR { fidx }

/* Exports */
mod_export:
    | LPAR ; EXPORT ;  sym = name ; d = export_desc ; RPAR
    { {modexp_name = sym ; modexp_desc = d} }

export_desc:
    | LPAR ; FUNC   ; fidx = fun_idx    ; RPAR { MED_func fidx }
    | LPAR ; TABLE  ; tidx = table_idx  ; RPAR { MED_table tidx }
    | LPAR ; MEM    ; midx = mem_idx    ; RPAR { MED_mem midx }
    | LPAR ; GLOBAL ; gidx = global_idx ; RPAR { MED_global gidx }

list_fun_idx:
  | f = fun_idx ; l = list_fun_idx { f::l }
  | { [] }

table_idx:  | i = INT { Big_int_Z.big_int_of_int i }
fun_idx:    | i = INT { Big_int_Z.big_int_of_int i }
global_idx: | i = INT { Big_int_Z.big_int_of_int i }
mem_idx:    | i = INT { Big_int_Z.big_int_of_int i }
type_idx:   | i = INT { Big_int_Z.big_int_of_int i }


/* Imports */
mod_import:
    | LPAR ; IMPORT ; modname = name ; sym = name ; d = import_desc ; RPAR
    { {imp_module = modname ; imp_name = sym ; imp_desc = d} }

import_desc:
    | LPAR ; FUNC ;  i = id ; tidx = type_idx ; RPAR { ID_func tidx }
    | LPAR ; TABLE ;  i = id ; tt = table_type ; RPAR { ID_table tt }
    | LPAR ; MEM ;  i = id ; mt = mem_type ; RPAR { ID_mem mt }
    | LPAR ; GLOBAL ;  i = id ; gt = global_type ; RPAR { ID_global gt }


/* Func  */
mod_func:
    | LPAR ; FUNC ; i = id ; tidx = type_idx ; lis = locals_instrs_list ; RPAR
    { let (locals, e) = lis in
      { modfunc_type = tidx ; modfunc_locals = locals ; modfunc_body = e } }

locals_instrs_list:
  | LPAR ; LOCAL ; i = id ; t = val_type ; RPAR ; lis = locals_instrs_list
    { let (l,e) = lis in (t::l, e) }
  | e = instrs_list { ([], e) }

instrs_list:
    | i = plain_instr ; il = instrs_list { i::il }
    | i = block_instr ; il = instrs_list { i::il }
    | { [] }

immediate:
    i = INT { Big_int_Z.big_int_of_int i }

plain_instr:
  | UNREACHABLE { I_unreachable }
  | NOP { I_nop }
  | DROP { I_drop }
  | SELECT { I_select }
  | BR; imm = immediate { I_br imm }
  | BR_IF; imm = immediate  { I_br_if imm }
  | RETURN { I_return }
  | CALL ; imm = immediate { I_call imm }
  | CALL_INDIRECT ; imm = immediate { I_call_indirect imm }

  | LOCAL_GET ; imm = immediate { I_get_local imm }
  | LOCAL_SET ; imm = immediate { I_set_local imm }
  | LOCAL_TEE ; imm = immediate { I_tee_local imm }
  | GLOBAL_GET ; imm = immediate { I_get_global imm }
  | GLOBAL_SET ; imm = immediate { I_set_global imm }

  | I32_SEGLOAD { I_segload T_int }
  | I32_SEGSTORE { I_segstore T_int }
  | I64_SEGLOAD { I_segload T_int }
  | I64_SEGSTORE { I_segstore T_int }
  | HANDLE_SEGLOAD { I_segload T_handle }
  | HANDLE_SEGSTORE { I_segstore T_handle }

  | SEGALLOC { I_segalloc }
  | SEGFREE { I_segfree }
  | SLICE { I_slice }
  | HANDLEADD { I_handleadd }

  | I32_CONST ; v = value { I_const v }
  | I64_CONST ; v = value { I_const v }

  | I32_LOAD { I_load T_int }
  | I32_STORE { I_store T_int }

  | I64_LOAD { I_load T_int }
  | I64_STORE { I_store T_int }

  | CURRENT_MEMORY { I_current_memory }
  | GROW_MEMORY { I_grow_memory }

    /* binop */
  | I32_ADD { I_binop (T_int, BOI_add) }
  | I32_SUB { I_binop (T_int, BOI_sub) }
  | I32_MUL { I_binop (T_int, BOI_mul) }
  | I32_REM_S { I_binop (T_int, (BOI_rem SX_S)) }
  | I32_REM_U { I_binop (T_int, (BOI_rem SX_U)) }
  | I32_DIV_S { I_binop (T_int, (BOI_div SX_S)) }
  | I32_DIV_U { I_binop (T_int, (BOI_div SX_U)) }

  | I64_ADD { I_binop (T_int, BOI_add) }
  | I64_SUB { I_binop (T_int, BOI_sub) }
  | I64_MUL { I_binop (T_int, BOI_mul) }
  | I64_REM_S { I_binop (T_int, (BOI_rem SX_S)) }
  | I64_REM_U { I_binop (T_int, (BOI_rem SX_U)) }
  | I64_DIV_S { I_binop (T_int, (BOI_div SX_S)) }
  | I64_DIV_U { I_binop (T_int, (BOI_div SX_U)) }

    /* testop */
  | I32_EQZ { I_testop T_int }
  | I64_EQZ { I_testop T_int }


    /* relop */
  | I32_EQ { I_relop (T_int, ROI_eq) }
  | I32_NE { I_relop (T_int, ROI_ne) }
  | I64_EQ { I_relop (T_int, ROI_eq) }
  | I64_NE { I_relop (T_int, ROI_ne) }

  | I32_LT_U { I_relop (T_int, (ROI_lt SX_U)) }
  | I32_LT_S { I_relop (T_int, (ROI_lt SX_S)) }
  | I32_GT_U { I_relop (T_int, (ROI_gt SX_U)) }
  | I32_GT_S { I_relop (T_int, (ROI_gt SX_S)) }
  | I32_LE_U { I_relop (T_int, (ROI_le SX_U)) }
  | I32_LE_S { I_relop (T_int, (ROI_le SX_S)) }
  | I32_GE_U { I_relop (T_int, (ROI_ge SX_U)) }
  | I32_GE_S { I_relop (T_int, (ROI_ge SX_S)) }

  | I64_LT_U { I_relop (T_int, (ROI_lt SX_U)) }
  | I64_LT_S { I_relop (T_int, (ROI_lt SX_S)) }
  | I64_GT_U { I_relop (T_int, (ROI_gt SX_U)) }
  | I64_GT_S { I_relop (T_int, (ROI_gt SX_S)) }
  | I64_LE_U { I_relop (T_int, (ROI_le SX_U)) }
  | I64_LE_S { I_relop (T_int, (ROI_le SX_S)) }
  | I64_GE_U { I_relop (T_int, (ROI_ge SX_U)) }
  | I64_GE_S { I_relop (T_int, (ROI_ge SX_S)) }

block_instr:
  | BLOCK ; lbl = label ; rt = result_type ; e = instrs_list ; END ; i = id
    { I_block (rt, e) }
  | LPAR ; BLOCK ; lbl = label ; rt = result_type ; e = instrs_list ; RPAR
    { I_block (rt, e) }

  | LOOP ; lbl = label ; rt = result_type ; e = instrs_list ; END ; i = id
    { I_loop (rt, e) }
  | LPAR ; LOOP ; lbl = label ; rt = result_type ; e = instrs_list ; RPAR
    { I_loop (rt, e) }

  | IF ; lbl  = label ; rt = result_type ; e1 = instrs_list; ELSE ; i1 = id ; e2 = instrs_list ; END ; i2 = id
    { I_if (rt, e1, e2) }

result_type:
  | vt = val_type { [vt] }

label:
  | i = id { i }

%%
