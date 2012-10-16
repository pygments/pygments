Strict

#rem
this is a
#rem
nested
#end
comment
even other preproc keywords are nested within!
#If TARGET
#End
#end

Import mojo

Const ONECONST:Int = 1
Const TWOCONST := 2
Const THREECONST := 3, FOURCONST:Int = 4

Class Game Extends App

    ' radial sprial with axis aligned phase

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

Class Hoodlum Extends Enemy
    Local currentNode:list.Node<Vector2D>

  ' Must implement Die method...

  Method Die ()
    Print "B'oss, he-- he killed me, b'oss!"
  End

End

Class VectorNode Extends Node<Vector2D>
End



Function Main()
    New Game()
End
