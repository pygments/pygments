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