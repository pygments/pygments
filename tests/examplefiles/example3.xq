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

(: `switch` expression example :)
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

(: `group by` expression with binding example :)
declare function local:a-to-z() {
	let $data as element()* := (
		<item>Apples</item>,
		<item>Bananas</item>,
		<item>Apricots</item>,
		<item>Pears</item>,
		<item>Brambles</item>
	) return
		<GroupedItems>{
			for $item in $data
			group by $key := upper-case(substring($item, 1, 1))
			order by $key
			return
				<Group key="{$key}">{$item}</Group>
		}</GroupedItems>
};

(: `group by` expression example :)
declare function local:plays-by-character() {
	let $plays := (
		document {
			<play>
				<title>Hamlet</title>
				<characters>
					<character>Hamlet</character>       
					<character>Claudius</character>
					<character>Polonius</character>
					<character>Rosencrantz</character>
					<character>Guildenstern</character>
					<character>Francisco</character>
					<character>Reynaldo</character>     
				</characters>
			</play>
		},
		document {
			<play>
				<title>Rosenkrantz and Guildenstern are Dead</title>
				<characters>
					<character>Alfred</character>
					<character>Rosencrantz</character>
					<character>Guildenstern</character>
					<character>Hamlet</character>
					<character>Claudius</character>
				</characters>
			</play>
		}
	) return

		for $play in $plays/play
		let $title := $play/title
		for $character in $play/characters/character
		group by $character
		return
			<character name="{$character}">
			{
				$title ! <play>{ . }</play>
     		}
			</character>	
};
