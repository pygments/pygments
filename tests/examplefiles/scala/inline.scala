inline def inline(inline x: Int): Double = ???
inline def power(x: Double, inline n: Int): Double =
inline if (n == 0) 1 else 2
inline val c = 0