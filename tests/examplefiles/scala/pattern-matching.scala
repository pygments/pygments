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