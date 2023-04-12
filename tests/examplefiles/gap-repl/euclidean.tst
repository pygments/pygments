#@local checkEuclideanRing
gap> START_TEST("euclidean.tst");

# test consistency of EuclideanDegree, EuclideanQuotient, EuclideanRemainder,
# and QuotientRemainder for some ring and elements of it
gap> checkEuclideanRing :=
> function(R, colls...)
>   local coll1, coll2, a, b, deg_b, deg_r, q, r, qr;
>   if Length(colls) >= 1 then coll1:=colls[1];
>   elif Size(R) <= 100 then coll1 := R;
>   else coll1 := List([1..100],i->Random(R));
>   fi;
>   if Length(colls) >= 2 then coll2:=colls[2];
>   elif Size(R) <= 100 then coll2 := R;
>   else coll2 := List([1..100],i->Random(R));
>   fi;
>   for b in coll1 do
>     if IsZero(b) then continue; fi;
>     deg_b := EuclideanDegree(R, b);
>     for a in coll2 do
>       q := EuclideanQuotient(R, a, b); Assert(0, q in R);
>       r := EuclideanRemainder(R, a, b); Assert(0, r in R);
>       if a <> q*b + r then Error("a <> q*b + r for ", [R,a,b]); fi;
>       deg_r := EuclideanDegree(R, r);
>       if not IsZero(r) and deg_r >= deg_b then Error("Euclidean degree did not decrease for ",[R,a,b]); fi;
>       qr := QuotientRemainder(R, a, b);
>       if qr <> [q, r] then Error("QuotientRemainder inconsistent for ", [R,a,b]); fi;
>     od;
>   od;
>   return true;
> end;;

# rings in characteristic 0
gap> checkEuclideanRing(Integers,[-100..100],[-100..100]);
true
gap> checkEuclideanRing(Rationals);
true
gap> checkEuclideanRing(GaussianIntegers);
true
gap> checkEuclideanRing(GaussianRationals);
true

# finite fields
gap> ForAll(Filtered([2..50], IsPrimePowerInt), q->checkEuclideanRing(GF(q)));
true

# ZmodnZ
gap> ForAll([1..50], m -> checkEuclideanRing(Integers mod m));
true
gap> checkEuclideanRing(Integers mod ((2*3*5)^2));
true
gap> checkEuclideanRing(Integers mod ((2*3*5)^3));
true
gap> checkEuclideanRing(Integers mod ((2*3*5*7)^2));
true
gap> checkEuclideanRing(Integers mod ((2*3*5*7)^3));
true

#
gap> STOP_TEST( "euclidean.tst", 1);
