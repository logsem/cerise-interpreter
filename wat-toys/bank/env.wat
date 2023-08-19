(module $env
        (type $adv_t (func (param handle) (result i32)))

        (func $adv (type 0)
              local.get 0
              drop
              i32.const 11
              return)
        (export "adv" (func 0))
 )
