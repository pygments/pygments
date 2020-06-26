#!/usr/bin/scala

// Comments
/* Comment block */
/* Multi-line 
 * comment block
 */
/*  /**/ /** */ /* comments within comments */ */
/**   /* */ /** **/ **/
// /* Commented-out comment block
// Line comment

// Imports
import // This is incorrect Scala but can still be highlighted correctly
import a.{x => y} // Test comment
import a.{x => } // This is incorrect Scala but can still be highlighted correctly
import a.{x => `test-name`}
import a.given
import a.{given a}
import a.{x, y}
import a._
import a.x
import a.x.y.z
import java.io.{File, IOException, FileNotFoundException}
import java.io.File
import scala.math.{given Ordering[Int]}
import scala.math.{given Ordering[?]}
import a.givenSomething
import givenPackage

// Exports
export // This is incorrect Scala but can still be highlighted correctly
export a._
export a.x // Test comment
export a.x.y.z // Test comment
export a.{x, y}
export a.{x => y}
export a.{x => } // This is incorrect Scala but can still be highlighted correctly
export a.{x => `test-name`} // Test comment
export given
export given a // Test comment
export given a.x // Test comment
export given a._
export given a.{x, y} // Test comment
export given a.{x => y}
export given a.{x => `test-name`}
  export scanUnit.scan
  export printUnit.{status => _, _}

// Package declarations
package
package com
package com.example

// Literals
true false null
1 2 3 4
1L 1l 10L 12123123L
3.0 12.345
3f 3.0f 3F 3.0F
3d 3.0d 3D 3.0D
110_222_795_799.99 110.9499_999 2_000.343_999e561_100
1e12 1e+34 1e-56 1e12f 1e+34f 1e-56f 1e12d 1e+34d 1e-56d
1E12 1E+34 1E-56 1E12f 1E+34f 1E-56f 1E12d 1E+34d 1E-56d
.1e12 .1e+34 .1e-56 .1e12f .1e+34f .1e-56f .1e12d .1e+34d .1e-56d
.1E12 .1E+34 .1E-56 .1E12f .1E+34f .1E-56f .1E12d .1E+34d .1E-56d
0x // Can still be highlighted correctly!
0x1234567890ABCDEF 0x1234567890abcdef
0x123_abc 0x123_ABC
"test" "\"test\"" "'test'" // comment
"""test: one ", two "", three """""" // comment
't' '"' '\'' '\n' ' '
super this

// String interpolation
s"1 + 2 = ${ 1 + { val x = 2; x } }."
s"""1 + 2 = ${
  def add(x: Int, y: Int) = {
    x + y
  }
  add(1, 2)
}."""
s"$first$second"
s"$safeTagMarker${mtch.matched}$safeTagMarker"
s"$a$a$a${b}$a${b}${b}"
s"${x$}"
s"$a$$$a" // $$ is an escape
val a = 4; foo(a)
s"$safeTagMarker${val a = 4; foo(a)}$safeTagMarker"

// Vals & vars
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

// Defs
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

// Using
def f(using x: Int): Unit = ()
f(using 2)
f(using .2)
class A(using x: Int)
new A(using 3)
f(using ())
f(using {})
f(using ' ')
f(using "")

// Declarations
trait X {}
object X
class Y
open object X:
open class Y:
case object X
case class Y()
package object x {}
package object y:

// Quoted
'{ 2 }
'[ String ]

// Symbols
object Unicode {
    val blue = '* //red
    val stillRed = '*
    val symbolEndedWithOp  = 'symbol_*
    val symbolWithDigit = 'symbol1 //'
    val greekSymbol = 'ξφδ
    val greekSymbolDigit = 'φδφ0
    val greekSymbolWithOp = 'δφξφξ_+-
    val multiOpSymbol = '***
    
    val symbolFollowedByOp = 'symbol*
    val invalidSymbol  = '**_x //'
    val characterLit = 'x'
}

// Type aliases
type X
type X <: Y
type X = Y
type X[Y] = Y with Z
type X[Y] = Y => (1 | 2, 3)
type X[Y] = (Y, 3) => (1 | 2, 3)
type Foo = Bar.Baz
opaque type Logarithm = Double

// Type lambda
[X, Y] =>> Map[Y, X]

// Match types
type Elem[X] = X match {
  case String => Char
  case Array[t] => t
  case Iterable[t] => t
}
type Concat[Xs <: Tuple, +Ys <: Tuple] <: Tuple = Xs match {
  case Unit => Ys
  case x *: xs => x *: Concat[xs, Ys]
}

// Dependent function types
trait Entry { type Key; val key: Key }
def extractKey(e: Entry): e.Key = e.key
val extractor: (e: Entry) => e.Key = extractKey
type Extractor = Function1[Entry, Entry#Key] {
  def apply(e: Entry): e.Key
}

// Singleton types
val x = ???
trait Foo[T <: x.type]
val a: x.type = ???
val b: Foo[x.type] = ???

// Union and intersection types
Type[A with "user provided string" with B]
def help(id: UserName | Password) = ???
val either: Password | UserName = ???
val both: Object & Product = ???

// Inline
inline def inline(inline x: Int): Double = ???
inline def power(x: Double, inline n: Int): Double =
inline if (n == 0) 1 else 2
inline val c = 0


// Soft keywords (should not be highlighted as keywords here)
val open = true
val inline = true
inline xval
val x = inline + 2
(using)
(usingSomething)

// Storage modifiers
private object a {}
private[com] object b {}
private[com.example] object c {}
protected object d {}
protected[com] object e {}
protected[com.example] object f {}
synchronized {}
abstract class g {}
final val h = ???
lazy val i = ???
sealed trait j
implicit val k = ???
enum m {}
inline val n = ???
opaque type o = Unit
@volatile @transient @native
override def p = ???

// Meta bounds
<% =:= <:< <%< >: <:

// Given
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

// Traits
trait Foo with
trait Bar with
trait *: with
trait *: with
trait :: with
1 :: Nil
1 ::

// Objects
object Foo with
object Bar with
object Zero extends Nat with
  ...

// Enums
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

// New
new A
new { }
new Foo
new foo.Foo
new Foo.Foo
new A:
  def f = 3

// End
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

// Extension methods
extension (x: String)
   def < (y: String): Boolean = ...
extension (x: Elem)
   def +: (xs: Seq[Elem]): Seq[Elem] = ...
extension (x: Number)
   infix def min (y: Number): Number = ...
extension (ss: Seq[String])
   def longestStrings: Seq[String] =
      val maxLength = ss.map(_.length).max
      ss.filter(_.length == maxLength)
   def longestString: String = longestStrings.head
extension (ss: Seq[String]) {
   def longestStrings: Seq[String] = {
      val maxLength = ss.map(_.length).max
      ss.filter(_.length == maxLength)
   }
  def longestString: String = longestStrings.head
}
extension (i: Int) def isZero: Boolean = i == 0
extension (i: Int) def divide(d: Int): Option[(Int, Int)] = ???
extension (x: Rational)
  infix def min(that Rational): Rational = ...
given [T: Ordering]: Ordering[List[T]] with
  extension (xs: List[T])
      def < (ys: List[T]): Boolean = ...

// Extends
trait A extends B
trait A extends (B => B){}
trait Color
object Red extends Color

// Derives
enum Tree[T] derives Eq, Ordering, Show {
  case Branch[T](left: Tree[T], right: Tree[T])
  case Leaf[T](elem: T)
}

// Pattern matching
def f(x: Int, y: Int) = x match {
    case `y` => 
    case  s @ Seq(_, _, _) => 
    case Seq(first, tail @ _*) =>
    case first +: tail =>
    case 3 | 5 | 6 =>
    case y: Number => y.n
    case Lit(n)        => n
    case IsZero(u)     => eval(u) == 0
    case _ => 15
}

// Operators
1 :: 2 :: Nil
a ++ b
a :+ b
a +: b
a :++ b
a ++: b
a + b
