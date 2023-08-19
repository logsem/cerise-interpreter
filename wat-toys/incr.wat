(module $incr
 (type $add_t (func))
 (global $g mut i32 10)
 (func $add (type 0)
       global.get 0
       i32.const 1
       i32.add
       global.set 0)
 (start 0)
 )
