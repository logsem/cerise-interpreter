module Ast :
  sig
    type register =
        PC
      | Reg of int
    type permission =
        O
      | E
      | RO
      | RX
      | RW
      | RWX
      | RWL
      | RWLX
    type locality =
        Global
      | Local
    type seal_permission = bool * bool
    type word_type =
        Integer
      | Capability
      | Seal_range
      | Sealed
    type reg_or_const =
        Register of register
      | Constant of Z.t
    type sealable =
        Cap of permission * locality * Z.t * Z.t * Z.t
      | SealRange of seal_permission * locality * Z.t * Z.t * Z.t
    type word =
        I of Z.t
      | Sealable of sealable
      | Sealed of Z.t * sealable
    type instruction =
        Jmp of register
      | Jnz of register * register
      | Move of register * reg_or_const
      | Load of register * register
      | Store of register * reg_or_const
      | Add of register * reg_or_const * reg_or_const
      | Sub of register * reg_or_const * reg_or_const
      | Mul of register * reg_or_const * reg_or_const
      | Rem of register * reg_or_const * reg_or_const
      | Div of register * reg_or_const * reg_or_const
      | Lt of register * reg_or_const * reg_or_const
      | Lea of register * reg_or_const
      | Restrict of register * reg_or_const
      | SubSeg of register * reg_or_const * reg_or_const
      | GetL of register * register
      | GetB of register * register
      | GetE of register * register
      | GetA of register * register
      | GetP of register * register
      | GetOType of register * register
      | GetWType of register * register
      | Seal of register * register * register
      | UnSeal of register * register * register
      | Invoke of register * register
      | Fail
      | Halt
    type expression = Assembly_frontend.Expression.t
    type register_term =
        Named of register
      | Register_parameter of string
    type permission_term =
        Permission_literal of permission
      | Permission_parameter of string
    type seal_permission_term =
        Seal_permission_literal of seal_permission
      | Seal_permission_parameter of string
    type locality_term =
        Locality of locality
      | Locality_parameter of string
    type constant_term =
        Expression of expression
      | Permission of permission
      | Seal_permission of seal_permission
      | Permission_locality of permission_term * locality_term
      | Seal_permission_locality of seal_permission_term * locality_term
      | Parameterized_permission_locality of string * locality_term
      | Locality_constant of locality
      | Word_type of word_type
      | Value_parameter of string
    type operand_term =
        Register_term of register_term
      | Constant_term of constant_term
    type sealable_term =
        Cap_term of permission_term * locality_term * expression *
          expression * expression
      | SealRange_term of seal_permission_term * locality_term * expression *
          expression * expression
    type word_term =
        I_term of expression
      | Sealable_term of sealable_term
      | Sealed_term of expression * sealable_term
    type instruction_term =
        Jmp_term of register_term
      | Jnz_term of register_term * register_term
      | Move_term of register_term * operand_term
      | Load_term of register_term * register_term
      | Store_term of register_term * operand_term
      | Add_term of register_term * operand_term * operand_term
      | Sub_term of register_term * operand_term * operand_term
      | Mul_term of register_term * operand_term * operand_term
      | Rem_term of register_term * operand_term * operand_term
      | Div_term of register_term * operand_term * operand_term
      | Lt_term of register_term * operand_term * operand_term
      | Lea_term of register_term * operand_term
      | Restrict_term of register_term * operand_term
      | SubSeg_term of register_term * operand_term * operand_term
      | GetL_term of register_term * register_term
      | GetB_term of register_term * register_term
      | GetE_term of register_term * register_term
      | GetA_term of register_term * register_term
      | GetP_term of register_term * register_term
      | GetOType_term of register_term * register_term
      | GetWType_term of register_term * register_term
      | Seal_term of register_term * register_term * register_term
      | UnSeal_term of register_term * register_term * register_term
      | Invoke_term of register_term * register_term
      | Fail_term
      | Halt_term
    type statement =
        Op of instruction_term
      | Word of word_term
    type program = statement list
    type regfile = (register * word_term) list
  end
module Printer :
  sig
    val permission : Ast.permission -> string
    val locality : Ast.locality -> string
    val seal_permission : bool * bool -> string
    val sealable : Ast.sealable -> string
    val word : Ast.word -> string
  end
module Codec :
  sig
    val register_codec :
      Ast.register
      Instruction_codec.scalar_codec
    val operand :
      (Ast.register, Z.t)
      Instruction_codec.register_or_constant
      Instruction_codec.shape
    val from_operand :
      (Ast.register, Z.t)
      Instruction_codec.register_or_constant ->
      Ast.reg_or_const
    val to_operand :
      Ast.reg_or_const ->
      (Ast.register, Z.t)
      Instruction_codec.register_or_constant
    val r :
      Ast.register
      Instruction_codec.shape
    val rr :
      (Ast.register *
       Ast.register)
      Instruction_codec.shape
    val ro :
      (Ast.register *
       (Ast.register, Z.t)
       Instruction_codec.register_or_constant)
      Instruction_codec.shape
    val roo :
      (Ast.register *
       (Ast.register, Z.t)
       Instruction_codec.register_or_constant *
       (Ast.register, Z.t)
       Instruction_codec.register_or_constant)
      Instruction_codec.shape
    val rrr :
      (Ast.register *
       Ast.register *
       Ast.register)
      Instruction_codec.shape
    val unit_case :
      string ->
      'a -> ('a -> bool) -> 'a Instruction_codec.case
    val cases :
      Ast.instruction
      Instruction_codec.case list
    val table :
      Ast.instruction
      Instruction_codec.t
    val encode :
      Ast.instruction ->
      (Z.t, Instruction_codec.error) result
    val decode :
      Z.t ->
      (Ast.instruction,
       Instruction_codec.error)
      result
    val allocations : (string * int * int) list
    val encode_tag : int -> Z.t -> Z.t
    val permission_scalar :
      Ast.permission -> int
    val encode_permission :
      Ast.permission -> Z.t
    val decode_permission :
      Z.t -> (Ast.permission, string) result
    val encode_seal_permission : bool * bool -> Z.t
    val decode_seal_permission : Z.t -> (bool * bool, string) result
    val encode_word_type :
      Ast.word_type -> Z.t
    val locality_scalar : Ast.locality -> int
    val encode_locality : Ast.locality -> Z.t
    val decode_locality :
      Z.t -> (Ast.locality, string) result
    val encode_permission_locality :
      Ast.permission ->
      Ast.locality -> Z.t
    val decode_permission_locality :
      Z.t ->
      (Ast.permission *
       Ast.locality, string)
      result
    val encode_seal_permission_locality :
      bool * bool -> Ast.locality -> Z.t
    val decode_seal_permission_locality :
      Z.t ->
      ((bool * bool) * Ast.locality, string)
      result
  end
module Machine :
  sig
    module RegMap :
      sig
        type key = Ast.register
        type 'a t
        val empty : 'a t
        val add : key -> 'a -> 'a t -> 'a t
        val add_to_list : key -> 'a -> 'a list t -> 'a list t
        val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
        val singleton : key -> 'a -> 'a t
        val remove : key -> 'a t -> 'a t
        val merge :
          (key -> 'a option -> 'b option -> 'c option) ->
          'a t -> 'b t -> 'c t
        val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
        val cardinal : 'a t -> int
        val bindings : 'a t -> (key * 'a) list
        val min_binding : 'a t -> key * 'a
        val min_binding_opt : 'a t -> (key * 'a) option
        val max_binding : 'a t -> key * 'a
        val max_binding_opt : 'a t -> (key * 'a) option
        val choose : 'a t -> key * 'a
        val choose_opt : 'a t -> (key * 'a) option
        val find : key -> 'a t -> 'a
        val find_opt : key -> 'a t -> 'a option
        val find_first : (key -> bool) -> 'a t -> key * 'a
        val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
        val find_last : (key -> bool) -> 'a t -> key * 'a
        val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
        val iter : (key -> 'a -> unit) -> 'a t -> unit
        val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
        val map : ('a -> 'b) -> 'a t -> 'b t
        val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
        val filter : (key -> 'a -> bool) -> 'a t -> 'a t
        val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
        val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
        val split : key -> 'a t -> 'a t * 'a option * 'a t
        val is_empty : 'a t -> bool
        val mem : key -> 'a t -> bool
        val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
        val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
        val for_all : (key -> 'a -> bool) -> 'a t -> bool
        val exists : (key -> 'a -> bool) -> 'a t -> bool
        val to_list : 'a t -> (key * 'a) list
        val of_list : (key * 'a) list -> 'a t
        val to_seq : 'a t -> (key * 'a) Seq.t
        val to_rev_seq : 'a t -> (key * 'a) Seq.t
        val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
        val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
        val of_seq : (key * 'a) Seq.t -> 'a t
      end
    module MemMap :
      sig
        type key = Z.t
        type 'a t = 'a Map.Make(Z).t
        val empty : 'a t
        val add : key -> 'a -> 'a t -> 'a t
        val add_to_list : key -> 'a -> 'a list t -> 'a list t
        val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
        val singleton : key -> 'a -> 'a t
        val remove : key -> 'a t -> 'a t
        val merge :
          (key -> 'a option -> 'b option -> 'c option) ->
          'a t -> 'b t -> 'c t
        val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
        val cardinal : 'a t -> int
        val bindings : 'a t -> (key * 'a) list
        val min_binding : 'a t -> key * 'a
        val min_binding_opt : 'a t -> (key * 'a) option
        val max_binding : 'a t -> key * 'a
        val max_binding_opt : 'a t -> (key * 'a) option
        val choose : 'a t -> key * 'a
        val choose_opt : 'a t -> (key * 'a) option
        val find : key -> 'a t -> 'a
        val find_opt : key -> 'a t -> 'a option
        val find_first : (key -> bool) -> 'a t -> key * 'a
        val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
        val find_last : (key -> bool) -> 'a t -> key * 'a
        val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
        val iter : (key -> 'a -> unit) -> 'a t -> unit
        val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
        val map : ('a -> 'b) -> 'a t -> 'b t
        val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
        val filter : (key -> 'a -> bool) -> 'a t -> 'a t
        val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
        val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
        val split : key -> 'a t -> 'a t * 'a option * 'a t
        val is_empty : 'a t -> bool
        val mem : key -> 'a t -> bool
        val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
        val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
        val for_all : (key -> 'a -> bool) -> 'a t -> bool
        val exists : (key -> 'a -> bool) -> 'a t -> bool
        val to_list : 'a t -> (key * 'a) list
        val of_list : (key * 'a) list -> 'a t
        val to_seq : 'a t -> (key * 'a) Seq.t
        val to_rev_seq : 'a t -> (key * 'a) Seq.t
        val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
        val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
        val of_seq : (key * 'a) Seq.t -> 'a t
      end
    type status =
        Running
      | Halted
      | Failed
    type t = {
      config : Runtime_config.t;
      status : status;
      registers : Ast.word RegMap.t;
      memory : Ast.word MemMap.t;
    }
    val diagnostic :
      string -> ('a, Diagnostic.t list) result
    val eval :
      Runtime_config.t ->
      Assembly_frontend.Expression.t ->
      (Z.t, Diagnostic.t list) result
    val ( let* ) :
      ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result
    val lower_permission :
      Ast.permission_term ->
      (Ast.permission,
       Diagnostic.t list)
      result
    val lower_seal_permission :
      Ast.seal_permission_term ->
      (Ast.seal_permission,
       Diagnostic.t list)
      result
    val lower_locality :
      Ast.locality_term ->
      (Ast.locality,
       Diagnostic.t list)
      result
    val lower_sealable :
      Runtime_config.t ->
      Ast.sealable_term ->
      (Ast.sealable,
       Diagnostic.t list)
      result
    val lower_word :
      Runtime_config.t ->
      Ast.word_term ->
      (Ast.word,
       Diagnostic.t list)
      result
    val lower_register :
      Ast.register_term ->
      (Ast.register,
       Diagnostic.t list)
      result
    val lower_constant :
      Runtime_config.t ->
      Ast.constant_term ->
      (Z.t, Diagnostic.t list) result
    val lower_operand :
      Runtime_config.t ->
      Ast.operand_term ->
      (Ast.reg_or_const,
       Diagnostic.t list)
      result
    val lower_instruction :
      Runtime_config.t ->
      Ast.instruction_term ->
      (Ast.instruction,
       Diagnostic.t list)
      result
    val lower_program :
      Runtime_config.t ->
      Ast.statement list ->
      (Ast.word MemMap.t,
       Diagnostic.t list)
      result
    val init :
      Runtime_config.t ->
      Ast.statement list ->
      (RegMap.key * Ast.word_term) list option ->
      (t, Diagnostic.t list) result
    val read_register :
      RegMap.key -> t -> Ast.word
    val read_memory :
      MemMap.key -> t -> Ast.word option
    val set_register :
      RegMap.key -> Ast.word -> t -> t
    val set_memory_raw :
      MemMap.key -> Ast.word -> t -> t
    val pc_next : t -> t
    val fail : t -> t
    val word_of_operand :
      t ->
      Ast.reg_or_const ->
      Ast.word
    val permission_flows :
      Ast.permission ->
      Ast.permission -> bool
    val seal_permission_flows : bool * bool -> bool * bool -> bool
    val can_read : Ast.permission -> bool
    val can_write : Ast.permission -> bool
    val can_store_local :
      Ast.permission -> bool
    val is_exec : Ast.permission -> bool
    val word_type :
      Ast.word ->
      Ast.word_type
    val bounds :
      Ast.sealable -> Z.t * Z.t * Z.t
    val locality :
      Ast.sealable ->
      Ast.locality
    val with_cursor :
      Ast.sealable ->
      Z.t -> Ast.sealable
    val with_bounds :
      Ast.sealable ->
      Z.t -> Z.t -> Ast.sealable
    val valid_pc : t -> bool
    val write_next :
      RegMap.key -> Ast.word -> t -> t
    val execute : Ast.instruction -> t -> t
    val step :
      t -> (t, Machine_backend.execution_error) result
    val step_n :
      int ->
      t -> (t, Machine_backend.execution_error) result
    val get_exec_state : t -> status
    val get_regfile : t -> Ast.word RegMap.t
    val get_memory : t -> Ast.word MemMap.t
    val read_reg : RegMap.key -> t -> Ast.word
    val read_mem :
      MemMap.key -> t -> Ast.word option
    val set_reg :
      RegMap.key -> Ast.word -> t -> t
    val set_mem :
      MemMap.key -> Ast.word -> t -> t
    val run : t -> t
  end
module State :
  sig
    type t = Machine.t
    type status =
        Running
      | Halted
      | Failed
    val status : t -> Machine.status
    val registers :
      t ->
      Ast.word
      Machine.RegMap.t
    val memory :
      t ->
      Ast.word
      Machine.MemMap.t
  end
module View :
  sig
    val inspect :
      Machine.t ->
      Machine_view.t
    val word :
      Ast.word ->
      Machine_view.word
  end

module Parser : sig
  type program = Ast.program
  type regfile = Ast.regfile
  type word = Ast.word_term
  val parse_program : ?filename:string -> string -> (program, Diagnostic.t list) result
  val parse_regfile : ?filename:string -> string -> (regfile, Diagnostic.t list) result
  val parse_word : ?filename:string -> string -> (word, Diagnostic.t list) result
end

module Backend : Machine_backend.S with type program = Ast.program and type regfile = Ast.regfile and type word = Ast.word_term and type state = Machine.t
