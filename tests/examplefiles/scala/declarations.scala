val x: Int
val y: Int = 1
val z = 1
var x: Int
var y: Int = 1
var z = 1
val (a, b) = (1, 2)
val Some(a) = Some(1, 2)
var Pair(a, b) = Pair(1, 2)
val Test.Pair(a) = Test.Pair(1, 2)
val a :: b = x :: Nil
var a :: b = x :: Nil
val a +: rest = ???
val foo_+ = "foo plus"
val foo_⌬⌬ = "double benzene"

def abs[T](x: Int): Int = if x >= 0 then new x else now -x
def abs(x: Int) = if x >= 0 then new x else now -x
def sum[A](xs: List[A])(implicit m: Monoid[A]): A = ???
def sum[A](xs: List[A])(implicit Monoid[A]): A = ???
def sum[A](xs: List[A])(using m: Monoid[A]): A = ???
def sum[A](xs: List[A])(using Monoid[A]): A = ???
def reduceRight(op: (T, T) => T): T = ???
def foldRight[](z: U)(op: (T, U) => U): U = ???
def obj(fields: (String, Any)*, test: String): Json
def :: (xs: List[T]): List[T] = ::(x, xs)
def ::(xs: List[T]): List[T] = ::(x, xs)

trait X {}
object X
class Y
open object X:
open class Y:
case object X
case class Y()
package object x {}
package object y:

type X
type X <: Y
type X = Y
type X[Y] = Y with Z
type X[Y] = Y => (1 | 2, 3)
type X[Y] = (Y, 3) => (1 | 2, 3)
type Foo = Bar.Baz

given Foo = ???
given foo = ???
given foo: Foo with
given listOrd[T: Ordering]: Ordering[List[T]] with
given listOrd(using ev: Ev): Foo with
given Ordering[Int] with
given Foo with
given [T: Ordering]: Ordering[List[T]] with
given (using ev: Ev): Foo with
given intOrd: Ordering[Int] with
given foo: Foo = ???
given `foo`: Foo = ???
given listOrd[T: Ordering]: Ordering[List[T]] = ???
given listOrd(using ev: Ev): Foo = ???
given Ordering[Int] = ???
given Foo = ???
given [T: Ordering]: Ordering[List[T]] = ???
given (using ev: Ev): Foo = ???

given sumMonoid: Monoid[Int] with
  extension (x: Int) def combine(y: Int) : Int = x + y 
  def unit: Int = 0

trait Ord[T]:
   def compare(x: T, y: T): Int
   extension (x: T) def < (y: T) = compare(x, y) < 0
   extension (x: T) def > (y: T) = compare(x, y) > 0
given intOrd: Ord[Int] with
   def compare(x: Int, y: Int) =
      if x < y then -1 else if x > y then +1 else 0
given listOrd[T](using ord: Ord[T]): Ord[List[T]] with
   def compare(xs: List[T], ys: List[T]): Int = (xs, ys) match
      case (Nil, Nil) => 0
      case (Nil, _) => -1
      case (_, Nil) => +1
      case (x :: xs1, y :: ys1) =>
         val fst = ord.compare(x, y)
         if fst != 0 then fst else compare(xs1, ys1)
trait A with
  given ac: C
trait B extends A with
  given bc: C
object O extends B with
  val x = summon[C]

// Classes
class A
class B
class Bar with
class Foo with
class :: with
class Rational(x: Int, y: Int) with
  def numer = x
  def denom = y
class Cons(_head: Int, _tail: IntList) extends IntList with
  val head = _head
  val tail = _tail
class Int with
  def + (that: Double): Double
  def + (that: Float): Float
  def + (that: Long): Long
  def + (that: Int): Int // same for -, *, /, %
  def << (cnt: Int): Int // same for >>, >>> */
  def & (that: Long): Long
  def & (that: Int): Int // same for |, ^ */
  def == (that: Double): Boolean
  def == (that: Float): Boolean
  def == (that: Long): Boolean // same for !=, <, >, <=, >=
end Int
class Sub extends Base with Something {
  override def foo = 2
  def bar = 3
}
class Succ(n: Nat) extends Nat with
  // ...
open class Writer[T] {
  /** Sends to stdout, can be overridden */
  def send(x: T) = println(x)
  /** Sends all arguments using `send` */
  def sendAll(xs: T*) = xs.foreach(send)
}
class LazyList[+T](init: => State[T]) with
  lazy val state: State[T] = init			

trait Foo with
trait Bar with
trait *: with
trait *: with
trait :: with
1 :: Nil
1 ::

object ⌘ {
  
}
object Foo with
object Bar with
object Zero extends Nat with
  ...

object Enum extends Enumeration {
  val Foo, Bar, Baz = Value
}
enum Color with
  case Red, Green, Blue, Magenta
enum Color(val test: Int) with
  case Red, Green, Blue, Magenta
  def isPrimary(color: Color): Boolean =
    color match
        case Red | Green | Blue => true
        case Magenta => false
enum State[T] with
  case Empty
  case Cons(hd: T, tl: LazyList[T])
abstract class Color
object Color {
  val Red = Color()
  val Green = Color()
  val Blue = Color()
  val Magenta = Color()
  ...
}
enum Vehicle(val numberOfWheels: Int) {
  case Unicycle extends Vehicle(1)
  case Bicycle extends Vehicle(2)
  case Car extends Vehicle(4)
}
enum Vehicle(val numberOfWheels: Int):
  case Unicycle extends Vehicle(1)
  case Bicycle extends Vehicle(2)
  case Car extends Vehicle(4)