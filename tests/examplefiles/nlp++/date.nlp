###############################################
# FILE: date.pat
# SUBJ: comment
# AUTH: AM
# CREATED: 11/Jul/04 01:42:17
# MODIFIED:
###############################################

@CODE
L("hello") = 0;
@@CODE

@NODES _TEXTZONE

# 0900 GMT
@POST
  ++S("don't splice");
  single();
@RULES
_date [layer=(_noun _np)] <-
	_xNUM
	_xWILD [star match=(_xWHITE _nbsp)]
	_xWILD [one match=(GMT PST MST CST EST)]
	@@


@POST
  ++S("don't splice");
  S("temporal") = 1;
  S("dy") = num(N("$text",3));
  S("mo") = num(N("$text",1));
  S("yr") = num(N("$text",5));
  S("sem") = "date";
  ++X("date ref");
  # Normalize year.
  if (S("yr") < 20)
    S("yr") = S("yr") + 2000;
  else if (S("yr") < 100)
    S("yr") = S("yr") + 1900;
  single();
@RULES
_date [layer=(_noun _np)] <- 
    _xNUM [s]	### (1)
    \/ [s]		### (2)
    _xNUM [s]	### (3)
    \/ [s]		### (4)
    _xNUM [s]	### (5)
    @@

@CHECK
	S("dy") = num(N("$text",4));
	S("yr") = num(N("$text",7));
	if (S("dy") > 31)
		fail();
	if (S("yr") < 100)
		fail();
@POST
  N("mypos",1) = "NP";
  S("mo") = monthtonum(strtolower(N("$text",1)));
  S("sem") = "date";
  ++X("date ref");
  if (N(7))
    S("don't splice") = 1;	# Keep this np.
  	# (Don't want to lose useful work.)
  S("bracket") = 1;
  single();
@RULES
_date [layer=(_noun _np)] <-
	_xWILD [s one match=(
	_month
	January February March April May June July August
	September October November December
	
	Jan Feb Mar Apr May Jun Jul Aug Sep Sept Oct Nov Dec
		)]
	\. [s opt]
	_xWHITE [s star]
	_xNUM [s]
	\, [s opt]
	_xWHITE [s star]
	_xNUM [s]
	@@

# num month num
# day month year
# year month day
@CHECK
  L("xx") = num(N("$text",1));
  if (!N(7))
    {
	if (L("xx") > 31 || L("xx") == 0)
	  fail();
	L("dy") = L("xx");
	succeed();
	}
  L("zz") = num(N("$text",7));
  if (L("xx") <= 31)
    {
	# Check dd mo yy
	S("dy") = L("xx");
	S("yr") = L("zz");
	}
  else
    {
	# Check yy mo dd
	S("yr") = L("xx");
	S("dy") = L("zz");
	}
  if (S("dy") > 31)
	fail();
  if (S("yr") < 90
   || (S("yr") > 99 && S("yr") < 1900)
   || S("yr") > 2050)
	fail();
@POST
  if (N(7))
    L("flag") = 1;
  S("mo") = monthtonum(strtolower(N("$text",3)));
  	# (Don't want to lose useful work.)
  group(1,7,"_date");
  group(1,1,"_noun");
  nountonp(1,1);
  N("sem",1) = "date";
  N("mo",1) = S("mo");
  N("yr",1) = S("yr");
  N("dy",1) = S("dy");
  ++X("date ref");
  if (L("flag"))
    {
    N("don't splice",1) = 1;	# Keep this np.
    X("date node") = N(1);
	}
  getdate(N(1));
@RULES
_xNIL <-
	_xNUM [s]
	_xWILD [s star match=(_xWHITE \- )]
	_xWILD [s one match=(
	_month
	January February March April May June July August
	September October November December
	
	Jan Feb Mar Apr May Jun Jul Aug Sep Sept Oct Nov Dec
		)]
	\. [s opt]
	\, [s opt]
	_xWILD [s star match=(_xWHITE \- )]
	_xNUM [s opt]
	@@

@CHECK
	S("dy") = 0;
	L("mooryr") = num(N("$text",5));
	L("ch") = strpiece(N("$text",5),0,0);
	if (L("ch") == "0" && L("mooryr") >= 0
		&& L("mooryr") <= 9)
	  S("yr") = 2000 + L("mooryr");
	else if (L("mooryr") >= 1900
	 && L("mooryr") <= 2100)
	  S("yr") = L("mooryr");
	else if (L("mooryr") >= 90 && L("mooryr") <= 99)
	  S("yr") = 1900 + L("mooryr");
	else if (L("mooryr") <= 31)
	  S("dy") = L("mooryr");
	else
	  fail();
@POST
  chpos(N(1),"NP");
  S("mo") = monthtonum(strtolower(N("$text",1)));
  S("sem") = "date";
  S("year") = N("$text",5);	 # 02/13/14 AM.
  ++X("date ref");
  single();
@RULES
_date [layer=(_noun)] <-
	_xWILD [s one match=(
	_month
	January February March April May June July August
	September October November December
	
	Jan Feb Mar Apr May Jun Jul Aug Sep Sept Oct Nov Dec
		)]
	\. [s opt]
	\, [s opt]
	_xWHITE [s star]
	_xNUM [s]
	@@

# pretagged corpus stuff.
@POST
 S("sem") = "date";
  ++X("date ref");
 single();
@RULES
_date [layer=(_noun)] <-
	_month [s]
	_xNUM
	@@

@POST
  S("day") = strtolower(N("$text"));
  S("sem") = "date";
  pncopyvars(1);
  S("mypos") = "NP";
  S("ne") = 1;
  S("ne type") = "date";
  ++X("date ref");
  S("ne type conf") = 95;
  single();
@RULES
_day [layer=(_date _noun)] <- _xWILD [s one match=(
	Sunday Monday Tuesday Wednesday Thursday Friday Saturday
	)] @@

@POST
  S("hr") = num(N("$text",1));
  S("min") = num(N("$text",3));
  if (N(5))
    S("am/pm") = N("$text",5);
  single();
@RULES
_daytime [layer=(_noun _np)] <-
	_xNUM [s]
	\: [s]
	_xNUM [s]
	_xWHITE [s star]
	_xWILD [s opt match=(AM PM)]
	@@


# Put some semantics on.	# 04/15/05 AM.
@POST
  if (G("conform treebank"))
    {
	chpos(N(1),"NN");
	N("bracket") = 1;
	}
  N("sem") = "date";
  N("advl") = 1;	# Possible standalone adverbial.
@RULES
_xNIL <- _xWILD [s one match=(
	today yesterday tomorrow
	)] @@

@POST
  N("sem") = "date";
  ++X("date ref");
@RULES
_xNIL <- _xWILD [s one match=(
	millennium millennia millenniums
	century centuries
	decade decades
	year years
	month months
	week weeks
	day days
	evening evenings
	night nights
	morning mornings
	noon noons
	afternoon afternoons
	hour hours
	minute minutes
	second seconds
	time date season	# 01/12/05 AM.
	holiday holidays
	quarter quarters	# ambig, of course.
	)]
	@@
