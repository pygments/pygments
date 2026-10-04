xquery version "3.0";

declare function local:add-log-message($message as xs:string) as empty-sequence()?
{
	let $logfile-collection := "/db/apps/exist101/log"
	let $logfile-name := "exist101-log.xml"
	let $logfile-full := concat($logfile-collection, '/', $logfile-name)
	let $logfile-created :=
	if(doc-available($logfile-full))then
		$logfile-full
	else
		xmldb:store($logfile-collection, $logfile-name, <eXist101-Log/>)
	return
		insert node
			<LogEntry timestamp="{current-dateTime()}">{$message}</LogEntry>
		into doc($logfile-full)/*
};

declare function local:insert-attributes() {
	let $elm as element() := doc('/db/Path/To/Some/Document.xml')/*
	return (
		insert node <NEW/> into $elm,
		insert node attribute x { 'y' } into $elm/*[last()],
		insert node attribute a { 'b' } into $elm/*[last()]
	)
};

declare function local:insert-elem() {
	let $elm as element() := doc('/db/Path/To/Some/Document.xml')/*
	return
		insert node <NEW x="y" a="b"/> into $elm
};

declare function local:insert-elem2() {
	let $elm as element() := doc('/db/Path/To/Some/Document.xml')/*
	let $new-element as element() := <NEW x="y" a="b"/>
	return
		insert node $new-element into $elm	
};

declare function local:insert-single() {
	insert node <LogEntry>Something happened...</LogEntry> into doc('/db/logs/mainlog.xml')/*
};


declare function local:trim-insert() {
	let $document := doc('/db/logs/mainlog.xml')
	let $newentry := <LogEntry>Something happened...</LogEntry>
	return
		delete node $document/*/LogEntry[position() ge 10],
		if(exists($document/*/LogEntry[1]))then
			insert node $newentry before $document/*/LogEntry[1]
		else
			insert node $newentry into $document/*
};


declare function local:attempt-document-node-insert() {
	
	(: This is invalid: :)
	let $document as document-node() := <Root><a/></Root>
	return
		insert node <b/> into $document/*
};

declare function local:attempt-attr-update-with-node() {
	replace node doc('/db/test/test.xml')/*/@name with
		<a>aaa<b>bbb</b></a>
};


(# exist:batch-transaction #) {
	delete node $document/*/LogEntry[position() ge 10],
	insert node $newentry before $document/*/LogEntry[1]
}