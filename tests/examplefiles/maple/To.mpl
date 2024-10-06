# Step 1 of 3: from Hakaru to Maple LO (linear operator)

toLO := module()
  export
  ModuleApply := proc(m, $)
    local h;
    h := gensym('h');
    _Env_HakaruSolve := false;
    LO(h, integrate(m, h, []))
  end proc;

  export
  integrate := proc(m, h, loops :: list(name = range) := [], $)
    local x, n, i, res, l, br, m0;

    if m :: known_continuous then
      integrate_known(Int, Ints, 'xx', m, h, loops)
    elif m :: known_discrete then
      integrate_known(Sum, Sums, 'kk', m, h, loops)
    elif m :: 'Ret(anything)' then
      applyintegrand(h, mk_ary(op(1,m), loops))
    elif m :: 'Bind(anything, name, anything)' then
      res := eval(op(3,m), op(2,m) = mk_idx(op(2,m), loops));
      res := eval(Integrand(op(2,m), 'integrate'(res, x, loops)), x=h);
      integrate(op(1,m), res, loops);
    elif m :: 'specfunc(Msum)' and (nops(loops) = 0 or nops(m) = 1) then
      `+`(op(map(integrate, [op(m)], h, loops)))
    elif m :: 'Weight(anything, anything)' then
      foldl(product, op(1,m), op(loops)) * integrate(op(2,m), h, loops)
    elif m :: t_pw_or_part then
      Partition:-AppPartOrPw
                 ( proc(p)
                       if not Partition:-ConditionsDepend(p, lhs~(loops)) then
                           Partition:-Pmap(z->integrate(z,h,loops),p)
                       end if;
                   end proc
                 , m, ['no_split_disj']);
    elif m :: t_case and not depends(op(1,m), map(lhs, loops)) then
      subsop(2=map(proc(b :: Branch(anything, anything))
                     eval(subsop(2='integrate'(op(2,b), x, loops),b), x=h)
                   end proc,
                   op(2,m)),
             m);
    elif m :: 'LO(name, anything)' then
      eval(op(2,m), op(1,m) = h)
    elif m :: 'Plate(nonnegint, name, anything)' then
      # Unroll Plate when the bound is known. We unroll Plate here (instead
      # of unrolling Ints in reduce, for example) because we have no other
      # way to integrate certain Plates, namely those whose bodies' control
      # flows depend on the index.
      x := mk_sym('pp', h);
      x := [seq(cat(x,i), i=0..op(1,m)-1)];
      if op(1,m) = 0 then
        res := undefined;
      else
        if op(1,m) = 1 then
          res := op(1,x);
        else
           res := idx(x,op(2,m));
#          res := piecewise(seq(op([op(2,m)=i-1, op(i,x)]), i=2..op(1,m)),
#                         op(1,x));
        end if;
        res := mk_idx(res, loops);
      end if;
      res := applyintegrand(h, mk_ary('ary'(op(1,m), op(2,m), res), loops));
      for i from op(1,m) to 1 by -1 do
        res := integrate(eval(op(3,m), op(2,m)=i-1),
                         Integrand(op(i,x), res), loops);
      end do;
      res
    elif m :: 'Plate(anything, name, anything)' then
      integrate(op(3,m), h, [op(2,m)=0..op(1,m)-1, op(loops)])
    elif m :: 'Context(anything, anything)' then
      applyop(integrate, 2, m, h, loops)
    elif h :: appliable then
      x := gensym('xa');
      'integrate'(m, Integrand(x, h(x)), loops)
    else
      'procname(_passed)'
    end if
  end proc;


  local
  integrate_known := proc(make, makes, var, m, h, loops :: list(name=range), $)
    local x, dens, bds;
    x := mk_sym(var, h);
    dens := density[op(0,m)](op(m));
    bds := bounds[op(0,m)](op(m));
    if loops = [] then
      make(dens(x) * applyintegrand(h, x), x = bds);
    else
      makes(foldl(product, dens(mk_idx(x,loops)), op(loops))
              * applyintegrand(h, x),
            x, bds, loops)
    end if;
  end proc;
end module; #toLO

