(module $client

        (type $bank_t (func))
        (type $assert_t (func (param i32) (param i32)))
        (type $adv_t (func (param handle) (result i32)))
        (global $flag mut i32 0)

        (import "env" "adv" (func $env.adv (type 2)))
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
              i32.const 15
              call 2)

        (func $assert (type 1)
              local.get 0
              local.get 1
              i32.ne
              global.set 0)

        (start 1)
        )
