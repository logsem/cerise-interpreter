val parse_prog_from_file : string -> (Ast.t, string) Result.t
val parse_prog_from_string : string -> (Ast.t, string) Result.t

val parse_regfile_from_file :
  string -> (Ast.word Machine.RegMap.t * Ast.word Machine.SRegMap.t, string) Result.t

val parse_regfile_from_string :
  string -> (Ast.word Machine.RegMap.t * Ast.word Machine.SRegMap.t, string) Result.t

val init_machine : Ast.t -> Ast.word Machine.RegMap.t -> Ast.word Machine.SRegMap.t -> Machine.t
