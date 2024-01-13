/*
The Indian GPA Problem. From 
http://www.robots.ox.ac.uk/~fwood/anglican/examples/viewer/?worksheet=indian-gpa 
"This example was inspired by Stuart Russell...the problem is: if you observe 
that a student GPA is exactly 4.04.0 in a model of transcripts of students 
from the USA (GPA's from 0.00.0 to 4.04.0 and India (GPA's from 0.00.0 to 
10.010.0) what is the probability that the student is from India?... 
As we know from statistics, given the mixture distribution and given the 
fact that his/her GPA is exactly 4.0, the probability that the student 
is American must be 1.0.
(i.e. zero probability that the student is from India)."
Probabilistic logic program from 
https://github.com/davidenitti/DC/blob/master/examples/indian-gpa.pl
*/
:- use_module(library(mcintyre)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.
:- mc.
:- begin_lpad.
coin ~ finite([0.95:true,0.05:false]).
agpa ~ beta(8,2) := coin~=true.
american_gpa ~ finite([0.85:4.0,0.15:0.0]) := coin~=false.

american_gpa ~ val(V) := agpa ~=A, V is A*4.0.

coin2 ~ finite([0.99:true,0.01:false]).
igpa ~ beta(5,5) := coin2~=true.
indian_gpa ~ finite([0.1:0.0,0.9:10.0]) := coin2~=false.

indian_gpa ~ val(V) := igpa ~=A, V is A*10.0.

nation ~ finite([0.25:a,0.75:i]).

student_gpa ~ val(A) := nation~=a,american_gpa~=A.
student_gpa ~ val(I) := nation~=i,indian_gpa~=I.


:- end_lpad.


/** <examples>
?- mc_lw_sample(nation(a),student_gpa(4.0),1000,PPost).
% probability that the nation is America given that the student got 4.0
% in his GPA
% expected result: 1.0
?- mc_sample(nation(a),1000,PPrior).
% prior probability that the nation is America 
% expected result: 0.25
*/
 
