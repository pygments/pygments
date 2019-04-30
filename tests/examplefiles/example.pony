use "somepkg"

/*
 /* Nested */
*/

class trn Foo[A: Stringable ref] is Stringable
  let _x = "\""
  
  fun val dofoo() =>
    """
    DocString
    """
    (U64(2), "foo")._2

actor Main
  new create(env: Env) =>
    env.out.print("Hello world")
