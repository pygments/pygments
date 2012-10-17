Strict

' single line comment

#rem
multi
line
comment
#end

#rem
nested
#rem
multi
line
#end
comment
#end

Import mojo

Const ONECONST:Int = 1
Const TWOCONST := 2
Const THREECONST := 3, FOURCONST:Int = 4

Global someVariable:Int = 4

' sample class from the documentation
Class Game Extends App

    Function DrawSpiral(clock)
        Local w=DeviceWidth/2
        For Local i#=0 Until w*1.5 Step .2
            Local x#,y#
            x=w+i*Sin(i*3+clock)
            y=w+i*Cos(i*2+clock)
            DrawRect  x,y,1,1
        Next
        hitbox.Collide(event.pos)
    End

    Field updateCount

    Method OnCreate()
        Print "spiral"

        SetUpdateRate 60
    End

    Method OnUpdate()
        updateCount+=1
    End

    Method OnRender()
        Cls
        DrawSpiral updateCount
        DrawSpiral updateCount*1.1
    End

End

Class Enemy
  Method Die () Abstract
End

' extending
Class Hoodlum Extends Enemy
    ' field
    Field testField:Bool = True

    ' naming class with modulepath
    Local currentNode:list.Node<Vector2D>

    Method Die ()
        Print "B'oss, he-- he killed me, b'oss!"
    End
End

' extending with generics
Class VectorNode Extends Node<Vector2D>
End

' interfaces
Interface Computer
  Method Boot ()
  Method Process ()
  Method Display ()
End

Class PC Implements Computer
End

' array syntax
Global listOfStuff:String[42]
Global lessStuff:String[5] = listOfStuff[4..8]
Global oneStuff:String = listOfStuff[23]

'a comma separated sequence
Global scores:Int[]=[10,20,30]
'a comma separated sequence
Global text:String[]=["Hello","There","World"]
Global worstCase:worst.List<String[]>

' escape characers in strings
Global string1 := "Hello~zWorld"
Global string2 := "~qHello World~q"
Global string3 := "~tIndented~n"
Global string3 := "tilda is wavey... ~~"
