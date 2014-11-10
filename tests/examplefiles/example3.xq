xquery version "3.0";

declare function local:word-count($elms as element()*) as xs:integer {
	sum($elms ! count(tokenize(., '\s+')))
};

declare function local:add($a, $b) {
	$a + $b
};