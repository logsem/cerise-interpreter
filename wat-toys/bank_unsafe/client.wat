(module $client

        (type $bank_t (func))
        (type $adv_t (func (param i32)))

        (import "env" "adv" (func $env.adv (type 1)))
        (import "env" "mem" (memory 0))
        (func $bank (type 0)
              (local $account_ptr i32)
              (local $account_id i32)
              (local $account_balance i32)

              i32.const 1
              memory.grow
              i32.const 0
              local.set 0

              local.get 0
              local.set 1

              i32.const 4
              local.get 0
              i32.add
              local.set 2

              local.get 2
              i32.const 0
              i32.store

              local.get 1
              call 0

              local.get 2
              i32.load
              drop)

        (start 1)
        )
