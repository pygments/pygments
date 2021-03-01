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
