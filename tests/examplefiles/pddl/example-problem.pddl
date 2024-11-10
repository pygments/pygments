(define (problem example-problem)
    (:domain example-domain)
    (:objects
       i1 i2 - item
    )
    (:init
        (adjacent i1 i2)
    )
    (:goal (adjacent i2 i1))
    (:metric minimize (total-cost))
)
