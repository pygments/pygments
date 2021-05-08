(module
    (func $fib (param $n i32) (result i32)
        (local $a i32)
        (local $b i32)
        (local $result i32)
        (if
            (i32.eqz (local.get $n))
            (then
                (return (i32.const 1))
            )
        )
        (local.set $b (i32.const 1))
        (; nested (; comment ;) ;)
        loop
            (local.set $result (i32.add (local.get $a) (local.get $b)))
            (local.set $a (local.get $b))
            (local.set $b (local.get $result))

            ;; decrement $n
            (local.tee $n (i32.sub (local.get $n) (i32.const 1)))

            (; test if $n > 0 ;)
            (i32.gt_u (i32.const 0))

            ;; if so, jump to the beginning of the loop
            br_if 0
        end
        local.get $result
    )
    (func $test_memory_store_args
        i32.const 1
        f64.store align=8 offset=16
    )
    (export "fib" (func $fib))
)