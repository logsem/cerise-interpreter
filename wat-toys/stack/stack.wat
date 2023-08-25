(module $stack
        (type (func (result i32)))
        (type (func (param i32) (result i32)))
        (type (func (param i32) (param i32)))

        (table 1)
        (memory 0)

        (func $new_stack (type 0)
              (local i32)
              i32.const 1
              memory.grow
              local.tee 0
              i32.const -1
              i32.eq

              if i32
                i32.const -1
              else
                local.get 0
                ;; i32.const page_size
                i32.const 32
                i32.mul
                local.tee 0
                local.get 0
                i32.store
                local.get 0
              end)

        (func $is_empty (type 1)

              ;; validate stack
              local.get 0
              ;; i32.const page_size
              i32.const 32
              i32.rem_s
              if unreachable else end

              ;; validate stack_bound
              local.get 0
              i32.load
              drop

              local.get 0
              local.get 0
              i32.load
              i32.eq)

        (func $is_full (type 1)

              ;; validate stack
              local.get 0
              ;; i32.const page_size
              i32.const 32
              i32.rem_s
              if unreachable else end

              ;; validate stack_bound
              local.get 0
              i32.load
              drop

              i32.const 1
              i32.const 0
              local.get 0
              i32.load
              ;; i32.const page_size
              i32.const 32
              i32.rem_s
              ;; i32.const (page_size - 4)
              i32.const 28
              i32.eq
              select)

        (func $pop (type 1)
              (local i32)
              ;; validate stack
              local.get 0
              ;; i32.const page_size
              i32.const 32
              i32.rem_s
              if unreachable else end

              ;; validate stack_bound
              local.get 0
              i32.load
              drop

              ;; is_empty_op
              local.get 0
              local.get 0
              i32.load
              i32.eq

              if unreachable else end
              local.get 0
              i32.load
              local.tee 1
              i32.load
              local.get 0
              local.get 1
              i32.const 4
              i32.sub
              i32.store)

        (func $push (type 2)
              (local i32)
              ;; validate stack
              local.get 0
              ;; i32.const page_size
              i32.const 32
              i32.rem_s
              if unreachable else end

              ;; validate stack_bound
              local.get 0
              i32.load
              drop

              ;; is_full_op
              i32.const 1
              i32.const 0
              local.get 0
              i32.load
              ;; i32.const page_size
              i32.const 32
              i32.rem_s
              ;; i32.const (page_size - 4)
              i32.const 28
              i32.eq
              select

              if unreachable else end
              local.get 0
              i32.load
              i32.const 4
              i32.add
              local.tee 2
              local.get 1
              i32.store
              local.get 0
              local.get 2
              i32.store)


        (func $stack_map (type 2)
              (local i32)
              (local i32)

              ;; validate stack
              local.get 0
              ;; i32.const page_size
              i32.const 32
              i32.rem_s
              if unreachable else end

              ;; validate stack_bound
              local.get 0
              i32.load
              drop

              ;; map_init
              local.get 0
              i32.load
              local.set 3
              local.get 0
              local.set 2

              ;; map_loop
              block
                loop
                    ;; map_loop_body
                    local.get 2
                    local.get 3
                    i32.eq
                    br_if 1

                    local.get 2
                    i32.const 4
                    i32.add
                    local.tee 2

                    local.get 2
                    i32.load
                    local.get 1
                    call_indirect 1
                    i32.store

                    br 0
                end
              end)

        (func $stack_length (type 1)

              ;; validate stack
              local.get 0
              ;; i32.const page_size
              i32.const 32
              i32.rem_s
              if unreachable else end

              ;; validate stack_bound
              local.get 0
              i32.load
              drop

              local.get 0
              i32.load
              local.get 0
              i32.sub
              i32.const 4
              i32.div_u)

        (export "new_stack" (func 0))
        (export "is_empty" (func 1))
        (export "is_full" (func 2))
        (export "pop" (func 3))
        (export "push" (func 4))
        (export "stack_map" (func 5))
        (export "stack_length" (func 6))
        (export "table" (table 0))
 )
