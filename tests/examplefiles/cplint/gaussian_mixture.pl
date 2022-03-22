/*
Mixture of two Gaussians. A biased coin is thrown, if it lands heads X in mix(X)
is sampled from a Gaussian with mean 0 and variance 1. if it lands tails X is
sampled from a Gaussian with mean 5 and variance 2.
The example illustrates the use of continuous random variables and
the use of sampling, including
rejection sampling and Metropolis/Hastings. Moreover the example
illustrates the use of the predicate histogram/3 for graphing the
probability density function of continuous random variables.
*/
:- use_module(library(mcintyre)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.
:- mc.
:- begin_lpad.

heads:0.6;tails:0.4. 
% a coin is thrown. The coin is biased: with probability 0.6 it lands heads,
% with probability 0.4 it lands tails

g(X): gaussian(X,0, 1).
% X in g(X)  follows a Gaussian distribution with mean 0 and variance 1
h(X): gaussian(X,5, 2).
% X in h(X)  follows a Gaussian distribution with mean 5 and variance 2

mix(X) :- heads, g(X).
% if the coin lands heads, X in mix(X) is given by g(X)
mix(X) :- tails, h(X).
% if the coin lands tails, X in mix(X) is given by h(X)

:- end_lpad.

hist_uncond(Samples,NBins,Chart):-
  mc_sample_arg(mix(X),Samples,X,L0),
  histogram(L0,Chart,[nbins(NBins)]).
% take SAmples samples of X in mix(X) and draw a histogram with NBins bins representing 
% the probability density of X 

hist_rej_heads(Samples,NBins,Chart):-
  mc_rejection_sample_arg(mix(X),heads,Samples,X,L0),
  histogram(L0,Chart,[nbins(NBins)]).
% take Samples samples of X in mix(X) given that heads was true using 
% rejection sampling and draw an
% histogram with NBins bins representing the probability density of X

hist_mh_heads(Samples,Lag,NBins,Chart):-
  mc_mh_sample_arg(mix(X),heads,Samples,X,L0,[lag(Lag)]),
  histogram(L0,Chart,[nbins(NBins)]).
% take Samples samples of X in mix(X) given that heads was true using 
% Metropolis-Hastings and draw an
% histogram with NBins bins representing the probability density of X

hist_rej_dis(Samples,NBins,Chart):-
  mc_rejection_sample_arg(mix(X),(mix(Y),Y>2),Samples,X,L0),
  histogram(L0,Chart,[nbins(NBins)]).
% take Samples samples of X in mix(X) given that X>2 was true using 
% rejection sampling and draw an
% histogram with NBins bins representing the probability density of X

hist_mh_dis(Samples,Lag,NBins,Chart):-
  mc_mh_sample_arg(mix(X),(mix(Y),Y>2),Samples,X,L0,[lag(Lag)]),
  histogram(L0,Chart,[nbins(NBins)]).
% take Samples samples of X in mix(X) given that X>2 was true using 
% Metropolis-Hastings and draw an
% histogram with NBins bins representing the probability density of X


/** <examples>
?- hist_uncond(1000,40,G).
% take 1000 samples of X in mix(X) and draw a histogram with 40 bins representing 
% the probability density of X 
?- mc_sample_arg(mix(X),1000,X,L),histogram(L,Chart,[nbins(40)]).
% take 1000 samples of X in mix(X) and draw a histogram with 40 bins representing 
% the probability density of X
?- mc_expectation(mix(X),1000,X,E).
% E=2.017964749114414
?- hist_rej_heads(1000,40,G).
% take 1000 samples of X in mix(X) given that heads was true using 
% rejection sampling and draw an
% histogram with 40 bins representing the probability density of X
?- hist_mh_heads(1000,2,40,G).
% take 1000 samples of X in mix(X) given that heads was true using 
% Metropolis-Hastings and draw an
% histogram with 40 bins representing the probability density of X
?- mc_mh_expectation(mix(X),heads,1000,X,E,[lag(2)]).
% E=-0.018433307290594284
?- hist_rej_dis(1000,40,G).
% take 1000 samples of X in mix(X) given that X>2 was true using 
% rejection sampling and draw an
% histogram with 40 bins representing the probability density of X
?- hist_mh_dis(1000,2,40,G).
% take 1000 samples of X in mix(X) given that X>2 was true using 
% Metropolis-Hastings and draw an
% histogram with 40 bins representing the probability density of X
?- mc_mh_expectation(mix(X),(mix(Y),Y>2),1000,X,E,[lag(2)]).

*/
 
