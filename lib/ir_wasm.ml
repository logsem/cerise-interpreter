let explode_string s = List.init (String.length s) (String.get s)
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


let extract_value_type (vt : value_type) : Extract.value_type =
  match vt with | T_int -> Extract.T_int | T_handle -> Extract.T_handle

let extract_result_type (rt : result_type) : Extract.result_type =
  List.map extract_value_type rt

let extract_mod_type (ft : function_type) : Extract.function_type =
 match ft with
   | Tf (rt1, rt2) -> Extract.Tf (extract_result_type rt1,
                                  extract_result_type rt2)

let extract_limits (l : limits) : Extract.limits =
  { Extract.lim_min = l.lim_min;
    Extract.lim_max = l.lim_max; }

let extract_handle (h : handle) : Extract.handle =
  { Extract.base = h.base;
    Extract.offset = h.offset;
    Extract.bound = h.bound;
    Extract.valid = h.valid;
    Extract.id0 = h.id0;
  }

let extract_value (v : value) : Extract.value =
  (match v with
   | Val_int n -> Extract.Val_int n
   | Val_handle h -> Extract.Val_handle (extract_handle h))

let extract_table_type (tt : table_type) : Extract.table_type =
  extract_limits tt

let extract_mod_table (mt : module_table) : Extract.module_table =
  extract_table_type mt

let extract_memory_type (mt : memory_type) : Extract.memory_type =
  extract_limits mt

let extract_mutability (m : mutability) : Extract.mutability =
  (match m with | MUT_immut -> Extract.MUT_immut | MUT_mut -> Extract.MUT_mut)

let extract_global_type (gt : global_type) : Extract.global_type =
  { Extract.tg_mut = extract_mutability gt.tg_mut;
    Extract.tg_t = extract_value_type gt.tg_t}

let extract_mod_global (mg : module_glob) : Extract.module_glob =
  { Extract.modglob_type = extract_global_type mg.modglob_type;
    Extract.modglob_init = extract_value mg.modglob_init }

let extract_memidx (tidx : memidx) : Extract.memidx = tidx
let extract_tableidx (tidx : tableidx) : Extract.tableidx = tidx
let extract_funidx (tidx : funcidx) : Extract.funcidx = tidx
let extract_typeidx (tidx : typeidx) : Extract.typeidx = tidx
let extract_globalidx (tidx : globalidx) : Extract.globalidx = tidx

let extract_mod_elem (me : module_element) : Extract.module_element =
{ Extract.modelem_table = extract_tableidx me.modelem_table;
  Extract.modelem_offset = me.modelem_offset;
  Extract.modelem_init = me.modelem_init }

let extract_module_start (ms : module_start) : Extract.module_start =
extract_funidx ms

let extract_import_desc (d : import_desc) : Extract.import_desc =
  (match d with
   | ID_func tidx -> Extract.ID_func tidx
   | ID_table tt -> Extract.ID_table (extract_table_type tt)
   | ID_mem mt -> Extract.ID_mem (extract_memory_type mt)
   | ID_global gt -> Extract.ID_global (extract_global_type gt))

let extract_mod_import (imp : module_import ) : Extract.module_import =
  { Extract.imp_module = explode_string imp.imp_name;
    Extract.imp_name = explode_string imp.imp_name;
    Extract.imp_desc = extract_import_desc imp.imp_desc }

let extract_export_desc (d : module_export_desc) : Extract.module_export_desc =
  (match d with
   | MED_func fidx -> Extract.MED_func (extract_funidx fidx)
   | MED_table tidx -> Extract.MED_table (extract_tableidx tidx)
   | MED_mem midx -> Extract.MED_mem (extract_memidx midx)
   | MED_global gidx -> Extract.MED_global (extract_globalidx gidx))

let extract_mod_export (exp : module_export) : Extract.module_export =
  { Extract.modexp_name = explode_string exp.modexp_name;
    Extract.modexp_desc = extract_export_desc exp.modexp_desc }


let extract_sx (s : sx) : Extract.sx =
  match s with
  | SX_S -> Extract.SX_S
  | SX_U -> Extract.SX_U

let extract_relop (op : relop) : Extract.relop =
  (match op with
   | ROI_eq -> Extract.ROI_eq
   | ROI_ne -> Extract.ROI_ne
   | ROI_lt s -> Extract.ROI_lt (extract_sx s)
   | ROI_gt s -> Extract.ROI_gt (extract_sx s)
   | ROI_le s -> Extract.ROI_le (extract_sx s)
   | ROI_ge s -> Extract.ROI_ge (extract_sx s)
  )

let extract_binop (op : binop) : Extract.binop =
  (match op with
   | BOI_add -> Extract.BOI_add
   | BOI_sub -> Extract.BOI_sub
   | BOI_mul -> Extract.BOI_mul
   | BOI_rem s -> Extract.BOI_rem (extract_sx s)
   | BOI_div s -> Extract.BOI_div (extract_sx s))

let rec extract_ws_binstr (i : ws_basic_instruction) : Extract.ws_basic_instruction =
  (match i with
   | I_unreachable -> Extract.I_unreachable
   | I_nop -> Extract.I_nop
   | I_drop -> Extract.I_drop
   | I_select -> Extract.I_select
   | I_block (rt, e) ->
     Extract.I_block (extract_result_type rt, List.map extract_ws_binstr e)
   | I_loop (rt, e) ->
     Extract.I_loop (extract_result_type rt, List.map extract_ws_binstr e)
   | I_if (rt, e1, e2) ->
     Extract.I_if
       (extract_result_type rt, List.map extract_ws_binstr e1, List.map extract_ws_binstr e2)
   | I_br i -> Extract.I_br i
   | I_br_if i -> Extract.I_br_if i
   | I_return -> Extract.I_return
   | I_call i -> Extract.I_call i
   | I_call_indirect i -> Extract.I_call_indirect i
   | I_get_local i -> Extract.I_get_global i
   | I_set_local i -> Extract.I_set_local i
   | I_tee_local i -> Extract.I_tee_local i
   | I_get_global i -> Extract.I_get_global i
   | I_set_global i -> Extract.I_set_global i
   | I_load vt -> Extract.I_load (extract_value_type vt)
   | I_store vt -> Extract.I_store (extract_value_type vt)
   | I_current_memory -> Extract.I_current_memory
   | I_grow_memory -> Extract.I_grow_memory
   | I_segload vt -> Extract.I_segload (extract_value_type vt)
   | I_segstore vt -> Extract.I_segstore (extract_value_type vt)
   | I_segalloc -> Extract.I_segalloc
   | I_segfree -> Extract.I_segfree
   | I_slice -> Extract.I_slice
   | I_handleadd -> Extract.I_handleadd
   | I_const v -> Extract.I_const (extract_value v)
   | I_binop (vt, op) -> Extract.I_binop (extract_value_type vt, extract_binop op)
   | I_testop vt -> Extract.I_testop (extract_value_type vt)
   | I_relop (vt, op) -> Extract.I_relop (extract_value_type vt, extract_relop op))

let extract_expr (e : expr) : Extract.expr =
  List.map extract_ws_binstr e

let extract_mod_func (f : module_func) : Extract.module_func =
 { Extract.modfunc_type = (extract_typeidx f.modfunc_type);
   Extract.modfunc_locals = List.map extract_value_type f.modfunc_locals;
   Extract.modfunc_body = extract_expr f.modfunc_body}

let extract_module (m : ws_module) : Extract.ws_module =
  { Extract.mod_types = List.map extract_mod_type m.mod_types;
    Extract.mod_funcs = List.map extract_mod_func m.mod_funcs;
    Extract.mod_tables = List.map extract_mod_table m.mod_tables;
    Extract.mod_mems = List.map extract_memory_type m.mod_mems;
    Extract.mod_globals = List.map extract_mod_global m.mod_globals;
    Extract.mod_elem = List.map extract_mod_elem m.mod_elem;
    Extract.mod_start =
      begin
      match m.mod_start with
      | None -> None
      | Some mstart -> Some (extract_module_start mstart)
    end ;
    Extract.mod_imports = List.map extract_mod_import m.mod_imports;
    Extract.mod_exports = List.map extract_mod_export m.mod_exports;
  }
