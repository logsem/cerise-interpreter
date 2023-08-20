open Ir

type symbol = string
type symbolic_word = ConcreteWord of machine_op | Symbol of symbol
type section = symbolic_word list

let compare_symbol = compare

module ExportMap =
  Map.Make(struct
    type t = symbol
    let compare = compare_symbol
  end)

type linkable_object =
  {
    text_section : section;
    data_section : section;
    exports_section : int ExportMap.t;
    start_offset : int option;
  }

type t = linkable_object
