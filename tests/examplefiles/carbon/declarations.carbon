package ExplorerTest api;

choice MyOptionalElement(ZZ:! type, YY:! type) {
  None(YY),
  Element(ZZ)
}

abstract class C {
  var a: i32;
}

base class B {
  var value_b: i32;
}

interface Vector {
  fn Zero() -> Self;
  fn Add[self: Self](b: Self) -> Self;
  fn Scale[self: Self](v: i32) -> Self;
}

class Pal {
  private var x: i32;
  protected var y: i32;
  friend Buddy;
}

abstract class MyAbstractClass {
  protected fn Create() -> partial Self { }
}

constraint DrawVectorLegoFish {
  impl as VectorLegoFish;
  impl as Drawable;
}

impl JustX as X {}