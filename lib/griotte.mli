module Ast = Cerise_griotte_contract.Ast
module Parser = Cerise_griotte_contract.Parser
module Printer = Cerise_griotte_contract.Printer
module Codec :
  sig
    val register_codec :
      Ast.register
      Instruction_codec.scalar_codec
    val system_register_codec :
      Ast.system_register
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
    val sr :
      Ast.system_register
      Instruction_codec.shape
    val o :
      (Ast.register, Z.t)
      Instruction_codec.register_or_constant
      Instruction_codec.shape
    val rr :
      (Ast.register *
       Ast.register)
      Instruction_codec.shape
    val rs :
      (Ast.register *
       Ast.system_register)
      Instruction_codec.shape
    val sr_r :
      (Ast.system_register *
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
    val case :
      string ->
      int ->
      'a Instruction_codec.shape ->
      ('a -> 'b) ->
      ('b -> 'a option) -> 'b Instruction_codec.case
    val unit_case :
      string ->
      int -> 'a -> ('a -> bool) -> 'a Instruction_codec.case
    val cases :
      Ast.instruction
      Instruction_codec.case list
    val table :
      Ast.instruction Instruction_codec.t
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
    val decode_tag : int -> string -> Z.t -> (Z.t, string) result
    val rx_scalar : Ast.rx_permission -> int
    val write_scalar : Ast.write_permission -> int
    val dl_scalar : Ast.deep_local_permission -> int
    val dro_scalar :
      Ast.deep_read_only_permission -> int
    val permission_scalar :
      Ast.rx_permission *
      Ast.write_permission *
      Ast.deep_local_permission *
      Ast.deep_read_only_permission -> int
    val permission_of_scalar :
      int ->
      (Ast.rx_permission *
       Ast.write_permission *
       Ast.deep_local_permission *
       Ast.deep_read_only_permission)
      option
    val payload_at_most : int -> 'a -> Z.t -> (Z.t, 'a) result
    val decode_permission_payload :
      Z.t ->
      (Ast.rx_permission *
       Ast.write_permission *
       Ast.deep_local_permission *
       Ast.deep_read_only_permission, string)
      result
    val encode_permission :
      Ast.rx_permission *
      Ast.write_permission *
      Ast.deep_local_permission *
      Ast.deep_read_only_permission -> Z.t
    val decode_permission :
      Z.t ->
      (Ast.rx_permission *
       Ast.write_permission *
       Ast.deep_local_permission *
       Ast.deep_read_only_permission, string)
      result
    val seal_permission_scalar : bool * bool -> int
    val encode_seal_permission : bool * bool -> Z.t
    val decode_seal_permission : Z.t -> (bool * bool, string) result
    val locality_scalar : Ast.locality -> int
    val encode_locality : Ast.locality -> Z.t
    val decode_locality :
      Z.t -> (Ast.locality, string) result
    val word_type_scalar : Ast.word_type -> int
    val encode_word_type : Ast.word_type -> Z.t
    val decode_word_type :
      Z.t -> (Ast.word_type, string) result
    val encode_permission_locality :
      Ast.rx_permission *
      Ast.write_permission *
      Ast.deep_local_permission *
      Ast.deep_read_only_permission ->
      Ast.locality -> Z.t
    val decode_permission_locality :
      Z.t ->
      ((Ast.rx_permission *
        Ast.write_permission *
        Ast.deep_local_permission *
        Ast.deep_read_only_permission) *
       Ast.locality, string)
      result
    val encode_seal_permission_locality :
      bool * bool -> Ast.locality -> Z.t
    val decode_seal_permission_locality :
      Z.t ->
      ((bool * bool) * Ast.locality, string) result
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
    module SRegMap :
      sig
        type key = Ast.system_register
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
      system_registers : Ast.word SRegMap.t;
      memory : Ast.word MemMap.t;
    }
    val diagnostic : string -> ('a, Diagnostic.t list) result
    val ( let* ) :
      ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result
    val arch_root_memory_permission :
      Ast.rx_permission *
      Ast.write_permission *
      Ast.deep_local_permission *
      Ast.deep_read_only_permission
    val arch_root_executable_permission :
      Ast.rx_permission *
      Ast.write_permission *
      Ast.deep_local_permission *
      Ast.deep_read_only_permission
    val rx_flows :
      Ast.rx_permission ->
      Ast.rx_permission -> bool
    val write_flows :
      Ast.write_permission ->
      Ast.write_permission -> bool
    val deep_local_flows :
      Ast.deep_local_permission ->
      Ast.deep_local_permission -> bool
    val deep_read_only_flows :
      Ast.deep_read_only_permission ->
      Ast.deep_read_only_permission -> bool
    val permission_flows :
      Ast.rx_permission *
      Ast.write_permission *
      Ast.deep_local_permission *
      Ast.deep_read_only_permission ->
      Ast.rx_permission *
      Ast.write_permission *
      Ast.deep_local_permission *
      Ast.deep_read_only_permission -> bool
    val locality_flows :
      Ast.locality ->
      Ast.locality -> bool
    val seal_permission_flows : bool * bool -> bool * bool -> bool
    val permission_of_word :
      Ast.word ->
      Ast.permission
    val word_is_derived : Ast.word -> bool
    val eval :
      Runtime_config.t ->
      Assembly_frontend.Expression.t ->
      (Z.t, Diagnostic.t list) result
    val lower_register :
      Ast.register_term ->
      (Ast.register, Diagnostic.t list)
      result
    val lower_permission :
      Ast.permission_term ->
      (Ast.permission, Diagnostic.t list)
      result
    val lower_seal_permission :
      Ast.seal_permission_term ->
      (Ast.seal_permission,
       Diagnostic.t list)
      result
    val lower_locality :
      Ast.locality_term ->
      (Ast.locality, Diagnostic.t list)
      result
    val lower_word_type :
      Ast.word_type_term ->
      (Ast.word_type, Diagnostic.t list)
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
    val lower_sealable :
      Runtime_config.t ->
      Ast.sealable_term ->
      (Ast.sealable, Diagnostic.t list)
      result
    val lower_word :
      Runtime_config.t ->
      Ast.word_term ->
      (Ast.word, Diagnostic.t list)
      result
    val lower_instruction :
      Runtime_config.t ->
      Ast.instruction_term ->
      (Ast.instruction,
       Diagnostic.t list)
      result
    val zero_registers : unit -> Ast.word RegMap.t
    val initial_registers :
      Runtime_config.t ->
      Ast.word RegMap.t
    val read_register :
      Ast.register ->
      t -> Ast.word
    val read_system_register :
      SRegMap.key -> t -> Ast.word
    val read_memory :
      MemMap.key -> t -> Ast.word option
    val set_register :
      Ast.register ->
      Ast.word -> t -> t
    val set_system_register :
      SRegMap.key -> Ast.word -> t -> t
    val set_memory_raw :
      MemMap.key -> Ast.word -> t -> t
    val fail : t -> t
    val lower_program :
      Runtime_config.t ->
      Ast.statement list ->
      (Ast.word MemMap.t,
       Diagnostic.t list)
      result
    val init :
      Runtime_config.t ->
      Ast.statement list ->
      ((RegMap.key * Ast.word_term) list *
       (SRegMap.key * Ast.word_term) list)
      option -> (t, Diagnostic.t list) result
    val value :
      t ->
      Ast.reg_or_const ->
      Ast.word
    val is_wl :
      'a * Ast.write_permission * 'b * 'c -> bool
    val is_dl :
      'a * 'b * Ast.deep_local_permission * 'c ->
      bool
    val is_dro :
      'a * 'b * 'c * Ast.deep_read_only_permission ->
      bool
    val executable :
      Ast.rx_permission * 'a * 'b * 'c -> bool
    val can_read :
      Ast.rx_permission * 'a * 'b * 'c -> bool
    val can_write :
      'a * Ast.write_permission * 'b * 'c -> bool
    val locality_of_sealable :
      Ast.sealable ->
      Ast.locality
    val locality_of_word :
      Ast.word ->
      Ast.locality option
    val deep_localize_sealable :
      Ast.sealable ->
      Ast.sealable
    val deep_localize :
      Ast.word -> Ast.word
    val read_only :
      Ast.word -> Ast.word
    val loaded_word :
      'a * 'b * Ast.deep_local_permission *
      Ast.deep_read_only_permission ->
      Ast.word -> Ast.word
    val pc_next : t -> t
    val write_next :
      Ast.register ->
      Ast.word -> t -> t
    val enter :
      Ast.word -> Ast.word
    val valid_pc : t -> bool
    val authorized_system : t -> bool
    val word_type :
      Ast.word ->
      Ast.word_type
    val arithmetic :
      (Z.t -> Z.t -> Z.t option) ->
      Ast.register ->
      Ast.reg_or_const ->
      Ast.reg_or_const -> t -> t
    val execute : Ast.instruction -> t -> t
    val step : t -> (t, Machine_backend.execution_error) result
    val step_n :
      int -> t -> (t, Machine_backend.execution_error) result
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
    val system_registers :
      t ->
      Ast.word
      Machine.SRegMap.t
    val memory :
      t ->
      Ast.word
      Machine.MemMap.t
    val inspect :
      Machine.t -> Machine_view.t
  end
module View :
  sig
    val inspect :
      Machine.t -> Machine_view.t
    val word :
      Ast.word -> Machine_view.word
  end

module Backend : Machine_backend.S with type program = Ast.program and type regfile = Ast.regfile and type word = Ast.word_term and type state = Machine.t
