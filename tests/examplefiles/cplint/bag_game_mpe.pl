
:- use_module(library(pita)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- use_rendering(graphviz).
:- use_rendering(table,[header(['Multivalued variable index','Rule index','Grounding substitution'])]).
:- endif.

:- pita.

:- begin_lpad.

win :- red, green.
win :- blue, yellow.

map_query 0.4::red.
map_query 0.9::green.
map_query 0.5::blue.
map_query 0.6::yellow.


:- end_lpad.

/** <examples>
?- map(win,P,Exp).
Exp = [
  rule(0, '', [red:0.4, '' : 0.6], true), 
  rule(1, green, [green:0.9, '' : 0.09999999999999998], true), 
  rule(2, blue, [blue:0.5, '' : 0.5], true), 
  rule(3, yellow, [yellow:0.6, '' : 0.4], true)],
P = 0.162,


*/
