xquery version "3.0";

(: Simple Map Operator example :)
declare function local:word-count($elms as element()*) as xs:integer {
	sum($elms ! count(tokenize(., '\s+')))
};

declare function local:add($a, $b) {
	$a + $b
};

declare function local:dispatch($node as node()) as item()* {
    typeswitch($node)
        case text() return $node
        case comment() return $node
        case element(bill) return local:bill($node)
        case element(btitle) return local:btitle($node)
        case element(section-id) return local:section-id($node)
        case element(bill-text) return local:bill-text($node)
        case element(strike) return local:strike($node)
        default return local:passthru($node)
};

(: Switch expression example :)
declare function local:noise($animal) {
	let $duck := "Duck",
	$quack := "Quack"
	return
		switch ($animal)
			case "Cow" return "Moo"
			case 'Cat' return 'Meow'
			case $duck return $quack
			default return "What's that odd noise?"
};