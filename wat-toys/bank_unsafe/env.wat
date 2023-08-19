(module $env
        (type $adv_t (func (param i32)))
        (memory 0)

        (func $adv (type 0)
              local.get 0
              i32.const 4
              i32.add
              local.set 1

              local.get 1
              i32.const 42
              i32.store)
        (export "adv" (func 0))
        (export "mem" (memory 0))
 )
