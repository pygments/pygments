class hanoi 
   predicates 
       hanoi : (unsigned N). 
end class hanoi 
 
implement hanoi 
   domains 
       pole = left; center; right. 
 
   clauses 
       hanoi(N) :- move(N, left, center, right). 
 
   class predicates 
       move : (unsigned N, pole A, pole B, pole C). 
   clauses 
       move(0, _, _, _) :- !. 
       move(N, A, B, C) :- 
           move(N-1, A, C, B), 
           stdio::writef("move a disc from % pole to the % pole\n", A, C), 
           move(N-1, B, A, C). 
end implement hanoi 
 
goal 
   console::init(), 
   hanoi::hanoi(4).
