; a comment

(define (domain example-domain)
    (:requirements :typing :action-costs)
    (:types item)
    (:predicates
        (adjacent ?a1 - item ?a2 - item)
    )
    (:functions (total-cost) - number)
    (:action swap
        :parameters (?a1 - item ?a2 - item)
        :precondition (adjacent ?a1 ?a2)
        :effect (and
            (adjacent ?a2 ?a1)
            (not (adjacent ?a1 ?a2))
            (increase (total-cost) 2)
        )
    )
)
