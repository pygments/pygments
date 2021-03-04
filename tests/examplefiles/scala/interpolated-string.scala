val n = 123;
val a = s"n=$n";
val a2 = s"n=$n''";
val b = s"""n=$n""";
val c = f"n=$n%f";
val d = f"""n=$n%f""";
val d2 = s"""a"""";
val e = s"abc\u00e9";
val f = s"a${n}b";
val g = s"a${n + 1}b";

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
