(module $client
        (type (func))
        (type (func (result i32)))
        (type (func (param i32) (result i32)))
        (type (func (param i32) (param i32)))

        (global mut i32 0)
        (elem 0 (offset 0) 8)

        (import "stack" "table" (table 1 2))

        (import "stack" "new_stack" (func (type 1)))
        (import "stack" "is_empty" (func (type 2)))
        (import "stack" "is_full" (func (type 2)))
        (import "stack" "pop" (func (type 2)))
        (import "stack" "push" (func (type 3)))
        (import "stack" "stack_map" (func (type 3)))
        (import "stack" "stack_length" (func (type 2)))


        (func $main_stack (type 0)
              (local i32)
              call 0
              local.tee 0
              i32.const -1
              i32.eq

              if
                i32.const -1
                global.set 0
                return
              else
              end

              local.get 0
              i32.const 4
              call 4
              local.get 0
              i32.const 6
              call 4
              local.get 0
              i32.const 0
              call 5
              local.get 0
              call 3
              local.get 0
              call 3
              i32.sub
              global.set 0)

        (func $square (type 2)
              local.get 0
              local.get 0
              i32.mul)

        (export "answer" (global 0))
        (start 7)
        )
