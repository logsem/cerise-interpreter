module Vanilla_ast = Cerise.Vanilla.Ast
module Vanilla_asm_ir = Cerise.Vanilla.Asm_ir
module Vanilla_parser = Cerise.Vanilla.Parser
module Vanilla_printer = Cerise.Vanilla.Printer
module Vanilla_codec = Cerise.Vanilla.Codec
module Vanilla_machine = Cerise.Vanilla.Machine
module Vanilla_backend = Cerise.Vanilla.Backend

module Locality_ast = Cerise.Locality_cerise.Ast
module Locality_asm_ir = Cerise.Locality_cerise.Asm_ir
module Locality_parser = Cerise.Locality_cerise.Parser
module Locality_printer = Cerise.Locality_cerise.Printer
module Locality_codec = Cerise.Locality_cerise.Codec
module Locality_machine = Cerise.Locality_cerise.Machine
module Locality_backend = Cerise.Locality_cerise.Backend

module Ucerise_ast = Cerise.Ucerise.Ast
module Ucerise_asm_ir = Cerise.Ucerise.Asm_ir
module Ucerise_parser = Cerise.Ucerise.Parser
module Ucerise_printer = Cerise.Ucerise.Printer
module Ucerise_codec = Cerise.Ucerise.Codec
module Ucerise_machine = Cerise.Ucerise.Machine
module Ucerise_backend = Cerise.Ucerise.Backend

module Mcerise_ast = Cerise.Mcerise.Ast
module Mcerise_asm_ir = Cerise.Mcerise.Asm_ir
module Mcerise_parser = Cerise.Mcerise.Parser
module Mcerise_printer = Cerise.Mcerise.Printer
module Mcerise_codec = Cerise.Mcerise.Codec
module Mcerise_machine = Cerise.Mcerise.Machine
module Mcerise_backend = Cerise.Mcerise.Backend

module Cerisier_ast = Cerise.Cerisier.Ast
module Cerisier_asm_ir = Cerise.Cerisier.Asm_ir
module Cerisier_parser = Cerise.Cerisier.Parser
module Cerisier_printer = Cerise.Cerisier.Printer
module Cerisier_codec = Cerise.Cerisier.Codec
module Cerisier_machine = Cerise.Cerisier.Machine
module Cerisier_backend = Cerise.Cerisier.Backend

let cerisier_capability : Cerisier_ast.word =
  Cerisier_ast.Sealable
    (Cerisier_ast.Cap (Cerisier_ast.RW, Z.zero, Z.one, Z.zero))

let cerisier_hash : Cerisier_ast.instruction =
  Cerisier_ast.Hash (Cerisier_ast.Reg 1, Cerisier_ast.Reg 2)

let cerisier_hash_concat : Cerisier_ast.instruction =
  Cerisier_ast.HashConcat
    ( Cerisier_ast.Reg 1,
      Cerisier_ast.Register (Cerisier_ast.Reg 2),
      Cerisier_ast.Constant Z.zero )

let cerisier_einit : Cerisier_ast.instruction =
  Cerisier_ast.EInit (Cerisier_ast.Reg 1, Cerisier_ast.Reg 2)

module Griotte_ast = Cerise.Griotte.Ast
module Griotte_asm_ir = Cerise.Griotte.Asm_ir
module Griotte_parser = Cerise.Griotte.Parser
module Griotte_printer = Cerise.Griotte.Printer
module Griotte_codec = Cerise.Griotte.Codec
module Griotte_machine = Cerise.Griotte.Machine
module Griotte_backend = Cerise.Griotte.Backend

module Extracted_ast = Cerise.Griotte_extracted.Ast
module Extracted_asm_ir = Cerise.Griotte_extracted.Asm_ir
module Extracted_parser = Cerise.Griotte_extracted.Parser
module Extracted_printer = Cerise.Griotte_extracted.Printer
module Extracted_codec = Cerise.Griotte_extracted.Codec
module Extracted_backend = Cerise.Griotte_extracted.Backend

let vanilla_assemble_word = Vanilla_asm_ir.assemble_word
let vanilla_assemble_program = Vanilla_asm_ir.assemble_program
let vanilla_assemble_regfile = Vanilla_asm_ir.assemble_regfile
let locality_assemble_word = Locality_asm_ir.assemble_word
let locality_assemble_program = Locality_asm_ir.assemble_program
let locality_assemble_regfile = Locality_asm_ir.assemble_regfile
let ucerise_assemble_word = Ucerise_asm_ir.assemble_word
let ucerise_assemble_program = Ucerise_asm_ir.assemble_program
let ucerise_assemble_regfile = Ucerise_asm_ir.assemble_regfile
let mcerise_assemble_word = Mcerise_asm_ir.assemble_word
let mcerise_assemble_program = Mcerise_asm_ir.assemble_program
let mcerise_assemble_regfile = Mcerise_asm_ir.assemble_regfile
let cerisier_assemble_word = Cerisier_asm_ir.assemble_word
let cerisier_assemble_program = Cerisier_asm_ir.assemble_program
let cerisier_assemble_regfile = Cerisier_asm_ir.assemble_regfile
let griotte_assemble_word = Griotte_asm_ir.assemble_word
let griotte_assemble_instruction = Griotte_asm_ir.assemble_instruction
let griotte_assemble_program = Griotte_asm_ir.assemble_program
let griotte_assemble_regfile = Griotte_asm_ir.assemble_regfile
let extracted_assemble_word = Extracted_asm_ir.assemble_word
let extracted_assemble_instruction = Extracted_asm_ir.assemble_instruction
let extracted_assemble_program = Extracted_asm_ir.assemble_program
let extracted_assemble_regfile = Extracted_asm_ir.assemble_regfile

let machine_alias (state : Cerise.Vanilla.Machine.t) : Cerise.Machine.t = state

let typed_seal_range (word : Cerise.Machine_view.word) : Cerise.Machine_view.seal_range option =
  word.seal_range

let typed_enclave_table (view : Cerise.Machine_view.t) : Cerise.Machine_view.enclave_table option =
  view.enclave_table

let default_backend_name : string = Cerise.Backend_registry.default_backend_name
let available_backend_names : string list = Cerise.Backend_registry.available_backend_names ()
let selected_backend : (module Cerise.Machine_backend.S) option =
  Cerise.Backend_registry.find_backend default_backend_name
let find_memory_word (address : Z.t) (view : Cerise.Machine_view.t) :
    Cerise.Machine_view.word option = Cerise.Machine_view.find_memory_word address view

let backends : (module Cerise.Machine_backend.S) list =
  [
    (module Vanilla_backend);
    (module Locality_backend);
    (module Ucerise_backend);
    (module Mcerise_backend);
    (module Cerisier_backend);
    (module Griotte_backend);
    (module Extracted_backend);
  ]

let () =
  ignore machine_alias;
  ignore typed_seal_range;
  ignore typed_enclave_table;
  ignore cerisier_capability;
  ignore cerisier_hash;
  ignore cerisier_hash_concat;
  ignore cerisier_einit;
  ignore vanilla_assemble_word;
  ignore vanilla_assemble_program;
  ignore vanilla_assemble_regfile;
  ignore locality_assemble_word;
  ignore locality_assemble_program;
  ignore locality_assemble_regfile;
  ignore ucerise_assemble_word;
  ignore ucerise_assemble_program;
  ignore ucerise_assemble_regfile;
  ignore mcerise_assemble_word;
  ignore mcerise_assemble_program;
  ignore mcerise_assemble_regfile;
  ignore cerisier_assemble_word;
  ignore cerisier_assemble_program;
  ignore cerisier_assemble_regfile;
  ignore griotte_assemble_word;
  ignore griotte_assemble_instruction;
  ignore griotte_assemble_program;
  ignore griotte_assemble_regfile;
  ignore extracted_assemble_word;
  ignore extracted_assemble_instruction;
  ignore extracted_assemble_program;
  ignore extracted_assemble_regfile;
  ignore available_backend_names;
  ignore selected_backend;
  ignore find_memory_word;
  if List.length backends <> 7 then failwith "missing public backend namespace"
