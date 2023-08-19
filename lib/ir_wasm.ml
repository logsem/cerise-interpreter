
type immediate = Big_int_Z.big_int

type handle = { base : Big_int_Z.big_int; offset : Big_int_Z.big_int;
                bound : Big_int_Z.big_int; valid : bool;
                id0 : Big_int_Z.big_int }

type value =
| Val_int of Big_int_Z.big_int
| Val_handle of handle

type value_type =
| T_int
| T_handle

type sx =
| SX_S
| SX_U

type binop =
| BOI_add
| BOI_sub
| BOI_mul
| BOI_rem of sx
| BOI_div of sx

type relop =
| ROI_eq
| ROI_ne
| ROI_lt of sx
| ROI_gt of sx
| ROI_le of sx
| ROI_ge of sx

type result_type = value_type list

type function_type =
| Tf of result_type * result_type

type ws_basic_instruction =
| I_unreachable
| I_nop
| I_drop
| I_select
| I_block of result_type * ws_basic_instruction list
| I_loop of result_type * ws_basic_instruction list
| I_if of result_type * ws_basic_instruction list * ws_basic_instruction list
| I_br of immediate
| I_br_if of immediate
| I_return
| I_call of immediate
| I_call_indirect of immediate
| I_get_local of immediate
| I_set_local of immediate
| I_tee_local of immediate
| I_get_global of immediate
| I_set_global of immediate
| I_load of value_type
| I_store of value_type
| I_current_memory
| I_grow_memory
| I_segload of value_type
| I_segstore of value_type
| I_segalloc
| I_segfree
| I_slice
| I_handleadd
| I_const of value
| I_binop of value_type * binop
| I_testop of value_type
| I_relop of value_type * relop

(** val page_size : Big_int_Z.big_int **)

let page_size =
  (Big_int_Z.mult_int_big_int 2 (Big_int_Z.mult_int_big_int 2
    (Big_int_Z.mult_int_big_int 2 (Big_int_Z.mult_int_big_int 2
    (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int)))))

type expr = ws_basic_instruction list

type typeidx =
  Big_int_Z.big_int
  (* singleton inductive, whose constructor was Mk_typeidx *)

type funcidx =
  Big_int_Z.big_int
  (* singleton inductive, whose constructor was Mk_funcidx *)

type memidx =
  Big_int_Z.big_int
  (* singleton inductive, whose constructor was Mk_memidx *)

type globalidx =
  Big_int_Z.big_int
  (* singleton inductive, whose constructor was Mk_globalidx *)

type tableidx =
  Big_int_Z.big_int
  (* singleton inductive, whose constructor was Mk_tableidx *)

type name = string

type limits = { lim_min : Big_int_Z.big_int;
                lim_max : Big_int_Z.big_int option }

type mutability =
| MUT_immut
| MUT_mut

type global_type = { tg_mut : mutability; tg_t : value_type }

type table_type =
  limits
  (* singleton inductive, whose constructor was Build_table_type *)

(** val tt_limits : table_type -> limits **)

let tt_limits t0 =
  t0

type memory_type = limits

type import_desc =
| ID_func of typeidx
| ID_table of table_type
| ID_mem of memory_type
| ID_global of global_type

type module_import = { imp_module : name; imp_name : name;
                       imp_desc : import_desc }

type module_export_desc =
| MED_func of funcidx
| MED_table of tableidx
| MED_mem of memidx
| MED_global of globalidx

type module_export = { modexp_name : name; modexp_desc : module_export_desc }

type module_func = { modfunc_type : typeidx;
                     modfunc_locals : value_type list;
                     modfunc_body : expr }

type module_start =
  funcidx
  (* singleton inductive, whose constructor was Build_module_start *)

(** val modstart_func : module_start -> funcidx **)

let modstart_func m =
  m

type module_table =
  table_type
  (* singleton inductive, whose constructor was Build_module_table *)

(** val modtab_type : module_table -> table_type **)

let modtab_type m =
  m

type module_element = { modelem_table : tableidx;
                        modelem_offset : Big_int_Z.big_int;
                        modelem_init : funcidx list }

type module_glob = { modglob_type : global_type; modglob_init : value }

type ws_module = { mod_types : function_type list;
                   mod_funcs : module_func list;
                   mod_tables : module_table list;
                   mod_mems : memory_type list;
                   mod_globals : module_glob list;
                   mod_elem : module_element list;
                   mod_start : module_start option;
                   mod_imports : module_import list;
                   mod_exports : module_export list }

exception ParserException of string
let compose_ws_modules (m1 : ws_module) (m2 : ws_module) : ws_module =
  (* TODO it misses the ordering condition *)
  {
    mod_types = m1.mod_types @ m2.mod_types;
    mod_funcs = m1.mod_funcs @ m2.mod_funcs;
    mod_mems = m1.mod_mems @ m2.mod_mems;
    mod_tables = m1.mod_tables @ m2.mod_tables;
    mod_globals = m1.mod_globals @ m2.mod_globals;
    mod_elem = m1.mod_elem @ m2.mod_elem;
    mod_start =
      begin
        match m1.mod_start, m2.mod_start with
        | Some _, Some _ -> raise @@ ParserException "Only one main allowed"
        | Some s, None
        | None, Some s -> Some s
        | None, None -> None
      end;
    mod_imports = m1.mod_imports @ m2.mod_imports;
    mod_exports = m1.mod_exports @ m2.mod_exports;
    }

let null_module : ws_module =
 { mod_types = [];
   mod_funcs = [];
   mod_tables = [];
   mod_mems = [];
   mod_globals = [];
   mod_elem = [];
   mod_start = None;
   mod_imports = [];
   mod_exports = [] }

let mono_module_type (t : function_type) =
 { mod_types = [t];
   mod_funcs = [];
   mod_tables = [];
   mod_mems = [];
   mod_globals = [];
   mod_elem = [];
   mod_start = None;
   mod_imports = [];
   mod_exports = [] }

let mono_module_func (f : module_func) =
 { mod_types = [];
   mod_funcs = [f];
   mod_tables = [];
   mod_mems = [];
   mod_globals = [];
   mod_elem = [];
   mod_start = None;
   mod_imports = [];
   mod_exports = [] }

let mono_module_table (t : module_table) =
 { mod_types = [];
   mod_funcs = [];
   mod_tables = [t];
   mod_mems = [];
   mod_globals = [];
   mod_elem = [];
   mod_start = None;
   mod_imports = [];
   mod_exports = [] }

let mono_module_mem (m : memory_type) =
 { mod_types = [];
   mod_funcs = [];
   mod_tables = [];
   mod_mems = [m];
   mod_globals = [];
   mod_elem = [];
   mod_start = None;
   mod_imports = [];
   mod_exports = [] }

let mono_module_global (g : module_glob) =
 { mod_types = [];
   mod_funcs = [];
   mod_tables = [];
   mod_mems = [];
   mod_globals = [g];
   mod_elem = [];
   mod_start = None;
   mod_imports = [];
   mod_exports = [] }

let mono_module_elem (e : module_element) =
 { mod_types = [];
   mod_funcs = [];
   mod_tables = [];
   mod_mems = [];
   mod_globals = [];
   mod_elem = [e];
   mod_start = None;
   mod_imports = [];
   mod_exports = [] }

let mono_module_start (s : module_start) =
 { mod_types = [];
   mod_funcs = [];
   mod_tables = [];
   mod_mems = [];
   mod_globals = [];
   mod_elem = [];
   mod_start = Some s;
   mod_imports = [];
   mod_exports = [] }

let mono_module_export (e : module_export) =
 { mod_types = [];
   mod_funcs = [];
   mod_tables = [];
   mod_mems = [];
   mod_globals = [];
   mod_elem = [];
   mod_start = None;
   mod_imports = [];
   mod_exports = [e] }

let mono_module_import (i : module_import) =
 { mod_types = [];
   mod_funcs = [];
   mod_tables = [];
   mod_mems = [];
   mod_globals = [];
   mod_elem = [];
   mod_start = None;
   mod_imports = [i];
   mod_exports = [] }
