(module $client

        (type $bank_t (func))
        (type $adv_t (func (param handle) (result i32)))

        (import "env" "adv" (func $env.adv (type 1)))
        ;; (import "env" "mem" (memory $ 0))
        (func $bank (type 0)
              (local $account_ptr handle)
              (local $account_id handle)
              (local $account_balance handle)

              i32.const 8
              segalloc
              local.set 0

              local.get 0
              i32.const 0
              i32.const 4
              slice
              local.set 1

              local.get 0
              i32.const 4
              i32.const 0
              slice
              local.set 2

              i32.const 4
              local.get 2
              handleadd
              local.set 2

              local.get 2
              i32.const 15
              i32.segstore

              local.get 1
              call 0
              drop

              local.get 2
              i32.segload
              drop
              )

        (start 1)
        )
