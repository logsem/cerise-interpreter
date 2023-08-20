(module $main
        (type $main_t (func))
        (global $g mut i32 10)

        (import "env" "adv" (func $env.adv (type 0)))
        (import "env" "h" (global mut i32))
        (export "g" (global 1))

        (func $main (type 0)
              global.get 0
              i32.const 0
              i32.add
              global.set 0

              call 0)

        (start 1))
