ScriptName Foo Extends Bar
{A muliline

Doc Comment}

Int z = 0x123456 ; Comment
Float y = 0.234183 ; Comment
String x = "A String!\n\t\"\\" ; Comment
Int z2 = 0x123456 ; Comment
Float y2 = 0.234183 ; Comment
String x2 = "A String!\n\t\"\\" ; Comment

String Property FullProperty
	String Function Get()
		Return "\"Foo\""
	EndFunction
EndProperty

Int Property AutoProperty = 5 Auto

Int[] Property Array Auto

Auto State Waiting
	Event OnThing(Baz arg)
		;/
			A
			Block
			Comment
		/;
		Int a = 1 + 2
	EndEvent

	Int[] Function Foo()
		Return New Int[2]
	EndFunction

	Int[] Function Bar()
		Return New Int[2]
	EndFunction
EndState ; Comment

State Active
	Function Baz()
		Foo()[0] = 1
		GoToState("Waiting")
	EndFunction
EndState

Function Loop()
	Int i = Array.Length
	While i > 0
		i -= 1
		Debug.Trace("Element: " + Array[i])
	EndWhile
EndFunction