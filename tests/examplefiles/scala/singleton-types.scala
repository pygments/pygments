val x = ???
trait Foo[T <: x.type]
val a: x.type = ???
val b: Foo[x.type] = ???

type Test[A] = Int
type MyTest = Test[1]

val one: 1 = 1