open Ir

type symbol = string
type symbolic_word = ConcreteWord of machine_op | Symbol of symbol
type section = symbolic_word list
type section_type = CodeSection | DataSection

let compare_symbol = compare

module SymbolMap =
  Map.Make(struct
    type t = symbol
    let compare = compare_symbol
  end)

type section_offset = (section_type * int)

type linkable_object =
  {
    text_section : section;
    data_section : section;
    exports_section : section_offset SymbolMap.t;
    imports_section : int SymbolMap.t;
    init_section : ((int * machine_op) list) SymbolMap.t;
    start_offset : section_offset option;
  }

type t = linkable_object
