new Foo:
  // ...
end new
end extension
end if
end while
end for
end match
class Foo
end Foo
end bar
end `bar`
end // test comment
package p1.p2:
  abstract class C():
    def this(x: Int) =
      this()
      if x > 0 then
        val a :: b =
          x :: Nil
        end val // test comment
        var y =
          x
        end y // test comment
        while y > 0 do
          println(y)
          y -= 1
        end while // test comment
        try
          x match
            case 0 => println("0")
            case _ =>
          end match // test comment
        finally
          println("done")
        end try // test comment
      end if // test comment
    end this // test comment
    def f: String
  end C // test comment
  object C:
    given C =
      new C:
        def f = "!"
        end f // test comment
      end new // test comment
    end given // test comment
  end C // test comment
  extension (x: C)
    def ff: String = x.f ++ x.f
  end extension // test comment
end p2 // test comment