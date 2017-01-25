implementation module StdGeneric

import StdInt, StdMisc, StdClass, StdFunc

generic bimap a b :: Bimap .a .b

bimapId :: Bimap .a .a
bimapId = { map_to = id, map_from = id }

bimap{|c|} = { map_to = id, map_from = id }

bimap{|PAIR|} bx by = { map_to= map_to, map_from=map_from }
where
	map_to (PAIR x y) 	= PAIR (bx.map_to x) (by.map_to y)
	map_from (PAIR x y) 	= PAIR (bx.map_from x) (by.map_from y)
bimap{|EITHER|} bl br = { map_to= map_to, map_from=map_from }
where	
	map_to (LEFT x) 	= LEFT (bl.map_to x)
	map_to (RIGHT x)	= RIGHT (br.map_to x)
	map_from (LEFT x) 	= LEFT (bl.map_from x)
	map_from (RIGHT x) 	= RIGHT (br.map_from x)

bimap{|(->)|} barg bres = { map_to = map_to, map_from = map_from }
where
	map_to f = comp3 bres.map_to f barg.map_from
	map_from f = comp3 bres.map_from f barg.map_to

bimap{|CONS|} barg = { map_to= map_to, map_from=map_from }
where
	map_to   (CONS x) = CONS (barg.map_to x)
	map_from (CONS x) = CONS (barg.map_from x)

bimap{|FIELD|} barg = { map_to= map_to, map_from=map_from }
where
	map_to   (FIELD x) = FIELD (barg.map_to x)
	map_from (FIELD x) = FIELD (barg.map_from x)

bimap{|OBJECT|} barg = { map_to= map_to, map_from=map_from }
where
	map_to   (OBJECT x) = OBJECT (barg.map_to x)
	map_from (OBJECT x) = OBJECT (barg.map_from x)

bimap{|Bimap|} x y = {map_to = map_to, map_from = map_from}
where
	map_to 	{map_to, map_from} = 
		{ map_to 	= comp3 y.map_to map_to x.map_from
		, map_from 	= comp3 x.map_to map_from y.map_from
		}
	map_from {map_to, map_from} = 
		{ map_to 	= comp3 y.map_from map_to x.map_to
		, map_from 	= comp3 x.map_from map_from y.map_to
		}

comp3 :: !(.a -> .b) u:(.c -> .a) !(.d -> .c) -> u:(.d -> .b)
comp3 f g h
	| is_id f
		| is_id h
			= cast g
			= cast (\x -> g (h x))
		| is_id h
			= cast (\x -> f (g x))
			= \x -> f (g (h x))
where
	is_id :: !.(.a -> .b) -> Bool
	is_id f = code inline
	{
		eq_desc e_StdFunc_did 0 0
		pop_a 1
	}
	
	cast :: !u:a -> u:b
	cast f = code inline
	{
		pop_a 0
	}

getConsPath :: !GenericConsDescriptor -> [ConsPos]
getConsPath {gcd_index, gcd_type_def={gtd_num_conses}}
	= doit gcd_index gtd_num_conses
where
	doit i n
		| n == 0 	
			= abort "getConsPath: zero conses\n"
		| i >= n	
			= abort "getConsPath: cons index >= number of conses"
		| n == 1
			= []
		| i < (n/2)
			= [ ConsLeft : doit i (n/2) ]
		| otherwise
			= [ ConsRight : doit (i - (n/2)) (n - (n/2)) ]
			  	 							 	