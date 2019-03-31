implementation module StdGeneric

/**
 * NOTE: this is a collection of different tricky parts of Clean modules (even
 * though the file is simply called StdGeneric.icl). The code is taken from:
 *
 * - StdGeneric (StdEnv)
 * - Graphics.Scalable.Image (Platform)
 */

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
			  	 							 	
:: NoAttr          m = NoAttr
:: DashAttr        m = { dash        :: ![Int]    }
:: FillAttr        m = { fill        :: !SVGColor }
:: LineEndMarker   m = { endmarker   :: !Image m  }
:: LineMidMarker   m = { midmarker   :: !Image m  }
:: LineStartMarker m = { startmarker :: !Image m  }
:: MaskAttr        m = { mask        :: !Image m  }
:: OpacityAttr     m = { opacity     :: !Real     }
:: StrokeAttr      m = { stroke      :: !SVGColor }
:: StrokeWidthAttr m = { strokewidth :: !Span     }
:: XRadiusAttr     m = { xradius     :: !Span     }
:: YRadiusAttr     m = { yradius     :: !Span     }


instance tuneImage NoAttr          where tuneImage image _    = image
instance tuneImage DashAttr        where tuneImage image attr = Attr` (BasicImageAttr` (BasicImgDashAttr attr.DashAttr.dash)) image
instance tuneImage FillAttr        where tuneImage image attr = Attr` (BasicImageAttr` (BasicImgFillAttr attr.FillAttr.fill)) image
instance tuneImage LineEndMarker   where tuneImage image attr = Attr` (LineMarkerAttr` {LineMarkerAttr | markerImg = attr.LineEndMarker.endmarker, markerPos = LineMarkerEnd}) image
instance tuneImage LineMidMarker   where tuneImage image attr = Attr` (LineMarkerAttr` {LineMarkerAttr | markerImg = attr.LineMidMarker.midmarker, markerPos = LineMarkerMid}) image
instance tuneImage LineStartMarker where tuneImage image attr = Attr` (LineMarkerAttr` {LineMarkerAttr | markerImg = attr.LineStartMarker.startmarker, markerPos = LineMarkerStart}) image
instance tuneImage MaskAttr        where tuneImage image attr = Attr` (MaskAttr` attr.MaskAttr.mask) image
instance tuneImage OpacityAttr     where tuneImage image attr = Attr` (BasicImageAttr` (BasicImgFillOpacityAttr attr.OpacityAttr.opacity)) image
instance tuneImage StrokeAttr      where tuneImage image attr = Attr` (BasicImageAttr` (BasicImgStrokeAttr      attr.StrokeAttr.stroke)) image
instance tuneImage StrokeWidthAttr where tuneImage image attr = Attr` (BasicImageAttr` (BasicImgStrokeWidthAttr attr.StrokeWidthAttr.strokewidth)) image
instance tuneImage XRadiusAttr     where tuneImage image attr = Attr` (BasicImageAttr` (BasicImgXRadiusAttr     attr.XRadiusAttr.xradius)) image
instance tuneImage YRadiusAttr     where tuneImage image attr = Attr` (BasicImageAttr` (BasicImgYRadiusAttr     attr.YRadiusAttr.yradius)) image

instance tuneImage DraggableAttr   where tuneImage image attr = Attr` (HandlerAttr` (ImgEventhandlerDraggableAttr   attr)) image
instance tuneImage OnClickAttr     where tuneImage image attr = Attr` (HandlerAttr` (ImgEventhandlerOnClickAttr     attr)) image
instance tuneImage OnMouseDownAttr where tuneImage image attr = Attr` (HandlerAttr` (ImgEventhandlerOnMouseDownAttr attr)) image
instance tuneImage OnMouseMoveAttr where tuneImage image attr = Attr` (HandlerAttr` (ImgEventhandlerOnMouseMoveAttr attr)) image
instance tuneImage OnMouseOutAttr  where tuneImage image attr = Attr` (HandlerAttr` (ImgEventhandlerOnMouseOutAttr  attr)) image
instance tuneImage OnMouseOverAttr where tuneImage image attr = Attr` (HandlerAttr` (ImgEventhandlerOnMouseOverAttr attr)) image
instance tuneImage OnMouseUpAttr   where tuneImage image attr = Attr` (HandlerAttr` (ImgEventhandlerOnMouseUpAttr   attr)) image
