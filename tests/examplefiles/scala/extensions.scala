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